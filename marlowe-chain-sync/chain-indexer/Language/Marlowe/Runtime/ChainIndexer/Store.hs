{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.ChainIndexer.Store
  ( ChainStoreDependencies(..)
  , ChainStoreSelector(..)
  , Changes(..)
  , SaveField(..)
  , chainStore
  ) where

import Cardano.Api (ChainPoint(..), ChainTip(..))
import Control.Concurrent.Component
import Control.Concurrent.STM (STM, atomically, newTVar, readTVar)
import Control.Concurrent.STM.Delay (Delay, newDelay, waitDelay)
import Control.Concurrent.STM.TVar (writeTVar)
import Control.Monad (guard, unless, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Foldable (for_, traverse_)
import Data.Maybe (isJust)
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Language.Marlowe.Runtime.ChainIndexer.Database
import Language.Marlowe.Runtime.ChainIndexer.Genesis (GenesisBlock)
import Language.Marlowe.Runtime.ChainIndexer.NodeClient (Changes(..), isEmptyChanges)
import Observe.Event.Explicit (EventBackend, addField, withEvent)
import Prelude hiding (filter)
import Witherable (Witherable(..))

data ChainStoreSelector f where
  CheckGenesisBlock :: ChainStoreSelector Bool
  Save :: ChainStoreSelector SaveField

data SaveField
  = RollbackPoint ChainPoint
  | BlockCount Int
  | LocalTip ChainTip
  | RemoteTip ChainTip
  | TxCount Int

-- | Set of dependencies required by the ChainStore
data ChainStoreDependencies r = ChainStoreDependencies
  { commitRollback :: !(CommitRollback IO) -- ^ How to persist rollbacks in the database backend
  , commitBlocks   :: !(CommitBlocks IO)   -- ^ How to commit blocks in bulk in the database backend
  , rateLimit      :: !NominalDiffTime     -- ^ The minimum time between database writes
  , getChanges     :: !(STM Changes)       -- ^ A source of changes to commit
  , getGenesisBlock :: !(GetGenesisBlock IO)
  , genesisBlock :: !GenesisBlock
  , commitGenesisBlock :: !(CommitGenesisBlock IO)
  , eventBackend :: EventBackend IO r ChainStoreSelector
  }

-- | Create a ChainStore component.
chainStore :: Component IO (ChainStoreDependencies r) (STM Bool)
chainStore = component \ChainStoreDependencies{..} -> do
  readyVar <- newTVar False
  let
    awaitChanges :: Maybe Delay -> STM Changes
    awaitChanges delay = do
      -- Wait until allowed to write again (determined by rateLimit).
      traverse_ waitDelay delay
      -- Reading this STM action clears the source of changes.
      changes <- getChanges
      guard $ not $ isEmptyChanges changes
      pure changes

    runChainStore :: IO ()
    runChainStore = do
      withEvent eventBackend CheckGenesisBlock \ev -> do
        mDbGenesisBlock <- runGetGenesisBlock getGenesisBlock
        addField ev $ isJust mDbGenesisBlock
        case mDbGenesisBlock of
          Just dbGenesisBlock -> unless (dbGenesisBlock == genesisBlock) do
            fail "Existing genesis block does not match computed genesis block"
          Nothing -> runCommitGenesisBlock commitGenesisBlock genesisBlock
      atomically $ writeTVar readyVar True
      go Nothing
      where
        go lastWrite = do
          delay <- wither computeDelay lastWrite
          Changes{..} <- atomically $ awaitChanges delay
          withEvent eventBackend Save \ev -> do
            for_ changesRollback \point -> do
              addField ev $ RollbackPoint point
              runCommitRollback commitRollback point
            when (changesBlockCount > 0) do
              addField ev $ BlockCount changesBlockCount
              addField ev $ TxCount changesTxCount
              addField ev $ LocalTip changesLocalTip
              addField ev $ RemoteTip changesTip
              runCommitBlocks commitBlocks changesBlocks
          go . Just =<< getCurrentTime

    computeDelay :: UTCTime -> IO (Maybe Delay)
    computeDelay lastWrite = runMaybeT do
      currentTime <- lift getCurrentTime
      let nextWrite = addUTCTime rateLimit lastWrite
      guard $ nextWrite > currentTime
      let delay = nextWrite `diffUTCTime` currentTime
      let delayMicroseconds = floor $ 1_000_000 * nominalDiffTimeToSeconds delay
      lift $ newDelay delayMicroseconds

  pure (runChainStore, readTVar readyVar)
