{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.ChainIndexer.Store (
  ChainStoreDependencies (..),
  ChainStoreSelector (..),
  Changes (..),
  CheckGenesisBlockField (..),
  chainStore,
) where

import Colog (Message, WithLog, logInfo)
import Control.Concurrent.Component
import Control.Concurrent.STM (STM, newTVar, readTVar)
import Control.Concurrent.STM.Delay (Delay, newDelay, waitDelay)
import Control.Concurrent.STM.TVar (writeTVar)
import Control.Monad (guard, unless, when)
import Control.Monad.Event.Class (MonadInjectEvent, withEvent)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Foldable (traverse_)
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Language.Marlowe.Runtime.ChainIndexer.Database
import Language.Marlowe.Runtime.ChainIndexer.Genesis (GenesisBlock)
import Language.Marlowe.Runtime.ChainIndexer.NodeClient (Changes (..), isEmptyChanges)
import Observe.Event (addField)
import UnliftIO (MonadUnliftIO, atomically)
import Witherable (Witherable (..))
import Prelude hiding (filter)

data ChainStoreSelector f where
  CheckGenesisBlock :: ChainStoreSelector CheckGenesisBlockField
  Save :: ChainStoreSelector Changes

data CheckGenesisBlockField
  = Computed GenesisBlock
  | Saved GenesisBlock

-- | Set of dependencies required by the ChainStore
data ChainStoreDependencies r m = ChainStoreDependencies
  { commitRollback :: !(CommitRollback m)
  -- ^ How to persist rollbacks in the database backend
  , commitBlocks :: !(CommitBlocks m)
  -- ^ How to commit blocks in bulk in the database backend
  , rateLimit :: !NominalDiffTime
  -- ^ The minimum time between database writes
  , getChanges :: !(STM Changes)
  -- ^ A source of changes to commit
  , getGenesisBlock :: !(GetGenesisBlock m)
  , genesisBlock :: !GenesisBlock
  , commitGenesisBlock :: !(CommitGenesisBlock m)
  }

-- | Create a ChainStore component.
chainStore
  :: forall r s env m
   . (MonadUnliftIO m, MonadInjectEvent r ChainStoreSelector s m, WithLog env Message m)
  => Component m (ChainStoreDependencies r m) (STM Bool)
chainStore = component "indexer-chain-store" \ChainStoreDependencies{..} -> do
  readyVar <- newTVar False
  let awaitChanges :: Maybe Delay -> STM Changes
      awaitChanges delay = do
        -- Wait until allowed to write again (determined by rateLimit).
        traverse_ waitDelay delay
        -- Reading this STM action clears the source of changes.
        changes <- getChanges
        guard $ not $ isEmptyChanges changes
        pure changes

      runChainStore :: m ()
      runChainStore = do
        withEvent CheckGenesisBlock \ev -> do
          addField ev $ Computed genesisBlock
          mDbGenesisBlock <- runGetGenesisBlock getGenesisBlock
          traverse_ (addField ev . Saved) mDbGenesisBlock
          case mDbGenesisBlock of
            Just dbGenesisBlock -> unless (dbGenesisBlock == genesisBlock) do
              liftIO $ fail "Existing genesis block does not match computed genesis block"
            Nothing -> do
              logInfo "Saving Genesis block, indexes disabled"
              runCommitGenesisBlock commitGenesisBlock genesisBlock
        atomically $ writeTVar readyVar True
        go Nothing
        where
          go :: Maybe UTCTime -> m a
          go lastWrite = do
            delay <- liftIO $ wither computeDelay lastWrite
            changes@Changes{..} <- atomically $ awaitChanges delay
            withEvent Save \ev -> do
              addField ev changes
              traverse_ (runCommitRollback commitRollback) changesRollback
              when (changesBlockCount > 0) do
                runCommitBlocks commitBlocks changesBlocks changesLocalTip changesTip
            go . Just =<< liftIO getCurrentTime

      computeDelay :: UTCTime -> IO (Maybe Delay)
      computeDelay lastWrite = runMaybeT do
        currentTime <- lift getCurrentTime
        let nextWrite = addUTCTime 0 lastWrite
        guard $ nextWrite > currentTime
        let delay = nextWrite `diffUTCTime` currentTime
        let delayMicroseconds = floor $ 1_000_000 * nominalDiffTimeToSeconds delay
        lift $ newDelay delayMicroseconds

  pure (runChainStore, readTVar readyVar)
