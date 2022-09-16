module Language.Marlowe.Runtime.ChainSync.Store
  ( ChainStore(..)
  , ChainStoreDependencies(..)
  , Changes(..)
  , mkChainStore
  ) where

import Cardano.Api (ChainPoint(..), ChainTip(..), SlotNo(..))
import Cardano.Api.Shelley (Hash(..))
import Control.Concurrent.STM (STM, atomically, newTVar, readTVar, writeTVar)
import Control.Concurrent.STM.Delay (Delay, newDelay, waitDelay)
import Control.Monad (guard, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.ByteString.Base16 (encodeBase16)
import Data.ByteString.Short (fromShort)
import Data.Foldable (for_, traverse_)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Data.Void (Void)
import Language.Marlowe.Runtime.ChainSync.Database (CommitBlocks(..), CommitRollback(..))
import Language.Marlowe.Runtime.ChainSync.NodeClient (Changes(..), isEmptyChanges)
import Prelude hiding (filter)
import System.IO (stderr)
import Witherable (Witherable(..))

-- | Set of dependencies required by the ChainStore
data ChainStoreDependencies = ChainStoreDependencies
  { commitRollback :: !(CommitRollback IO) -- ^ How to persist rollbacks in the database backend
  , commitBlocks   :: !(CommitBlocks IO)   -- ^ How to commit blocks in bulk in the database backend
  , rateLimit      :: !NominalDiffTime     -- ^ The minimum time between database writes
  , getChanges     :: !(STM Changes)       -- ^ A source of changes to commit
  }

-- | Public API of the ChainStore component
data ChainStore = ChainStore
  { runChainStore :: !(IO Void)        -- ^ Run the chain store in IO
  , localTip      :: !(STM ChainTip) -- ^ Action to read the current (local) chain tip
  }

-- | Create a ChainStore component.
mkChainStore :: ChainStoreDependencies -> STM ChainStore
mkChainStore ChainStoreDependencies{..} = do
  localTipVar <- newTVar ChainTipAtGenesis
  let
    awaitChanges :: Maybe Delay -> STM Changes
    awaitChanges delay = do
      -- Wait until allowed to write again (determined by rateLimit).
      traverse_ waitDelay delay
      -- Reading this STM action clears the source of changes.
      changes <- getChanges
      guard $ not $ isEmptyChanges changes
      pure changes

    runChainStore :: IO Void
    runChainStore = go Nothing
      where
        go lastWrite = do
          delay <- wither computeDelay lastWrite
          Changes{..} <- atomically $ awaitChanges delay
          for_ changesRollback \point -> do
            case point of
              ChainPointAtGenesis -> T.hPutStrLn stderr "Rolling back to Genesis"
              ChainPoint (SlotNo slot) (HeaderHash hash) -> T.hPutStrLn stderr $ T.intercalate " "
                [ "Rolling back to block"
                , encodeBase16 $ fromShort hash
                , "at slot"
                , T.pack $ show slot
                ]
            runCommitRollback commitRollback point
          when (changesBlockCount > 0) do
            T.hPutStrLn stderr $ mconcat
              [ "Saving "
              , T.pack $ show changesBlockCount
              , " blocks, "
              , T.pack $ show changesTxCount
              , " transactions. New tip: "
              , case changesLocalTip of
                  ChainTipAtGenesis -> "Genesis"
                  ChainTip (SlotNo slot) (HeaderHash hash) _ -> T.intercalate " "
                    [ "block"
                    , encodeBase16 $ fromShort hash
                    , "at slot"
                    , T.pack $ show slot
                    ]
              , " (node tip: "
              , case changesTip of
                  ChainTipAtGenesis           -> "Genesis"
                  ChainTip (SlotNo slot) _  _ -> T.pack $ show slot
              , ")"
              ]
            runCommitBlocks commitBlocks changesBlocks
            atomically $ writeTVar localTipVar changesLocalTip
          go . Just =<< getCurrentTime

    computeDelay :: UTCTime -> IO (Maybe Delay)
    computeDelay lastWrite = runMaybeT do
      currentTime <- lift getCurrentTime
      let nextWrite = addUTCTime rateLimit lastWrite
      guard $ nextWrite > currentTime
      let delay = nextWrite `diffUTCTime` currentTime
      let delayMicroseconds = floor $ 1_000_000 * nominalDiffTimeToSeconds delay
      lift $ newDelay delayMicroseconds

    localTip = readTVar localTipVar

  pure $ ChainStore { runChainStore, localTip }
