module Language.Marlowe.Runtime.ChainSync.Store
  ( Changes(..)
  , StoreDependencies(..)
  , StoreApi(..)
  , mkStore
  , mkStoreEventHandler
  ) where

import Cardano.Api (Block (..), BlockHeader (..), BlockInMode (..), ChainPoint (..), getBlockHeader)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (STM, atomically, modifyTVar, newTVar, readTVar, writeTVar)
import Control.Monad (guard)
import Data.Foldable (traverse_)
import Data.Function (on)
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Language.Marlowe.Runtime.ChainSync.NodeClient (CardanoBlock, ChainEventHandler, ChainSyncEvent (..))
import Witherable (Witherable (..))

type CommitRollback m = ChainPoint -> m ()
type CommitBlocks m = [CardanoBlock] -> m ()

data Changes = Changes
  { changesRollback :: !(Maybe ChainPoint)
  , changesBlocks   :: ![CardanoBlock]
  }

emptyChanges :: Changes
emptyChanges = Changes Nothing []

isEmpty :: Changes -> Bool
isEmpty (Changes Nothing []) = True
isEmpty _                    = False

instance Eq Changes where
  a == b = changesRollback a == changesRollback b
    && on (==) (map (headerToTuple . getBlock) . changesBlocks) a b
    where
      getBlock (BlockInMode block _) = getBlockHeader block
      headerToTuple (BlockHeader slot hash block) = (slot, hash, block)

data StoreDependencies = StoreDependencies
  { commitRollback :: !(CommitRollback IO)
  , commitBlocks   :: !(CommitBlocks IO)
  , awaitChanges   :: !(STM Changes)
  , rateLimit      :: !NominalDiffTime
  }

newtype StoreApi = StoreApi
  { runStore :: IO ()
  }

mkStore :: StoreDependencies -> IO StoreApi
mkStore StoreDependencies{..} = do
  pure $ StoreApi
    { runStore = runStore Nothing
    }
  where
    runStore :: Maybe UTCTime -> IO ()
    runStore lastWrite = do
      delay <- wither computeDelay lastWrite
      traverse_ (threadDelay . round . (* 1_000_000) . nominalDiffTimeToSeconds) delay
      Changes{..} <- atomically awaitChanges
      traverse_ commitRollback changesRollback
      commitBlocks changesBlocks
      runStore . Just =<< getCurrentTime

    computeDelay :: UTCTime -> IO (Maybe NominalDiffTime)
    computeDelay lastWrite = do
      currentTime <- getCurrentTime
      pure do
        let nextRun = addUTCTime rateLimit lastWrite
        guard $ nextRun > currentTime
        pure $ diffUTCTime nextRun currentTime

mkStoreEventHandler :: STM (STM Changes, ChainEventHandler STM)
mkStoreEventHandler = do
  tchanges <- newTVar emptyChanges
  pure
    ( do
        changes <- readTVar tchanges
        guard $ not $ isEmpty changes
        writeTVar tchanges emptyChanges
        pure changes
    , \case
        RollBackward ChainPointAtGenesis _ ->
          writeTVar tchanges $ Changes (Just ChainPointAtGenesis) []
        RollBackward point@(ChainPoint slot _) _ ->
          modifyTVar tchanges \Changes{..} -> Changes
            (Just $ maybe point (minPoint point) changesRollback)
            (filter ((<= slot) . blockSlot) changesBlocks)
        RollForward block _ ->
          modifyTVar tchanges \changes ->
            changes { changesBlocks = block : changesBlocks changes }
    )
  where
    minPoint ChainPointAtGenesis _ = ChainPointAtGenesis
    minPoint _ ChainPointAtGenesis = ChainPointAtGenesis
    minPoint p1@(ChainPoint s1 _) p2@(ChainPoint s2 _)
      | s1 < s2 = p1
      | otherwise = p2

    blockSlot (BlockInMode (Block (BlockHeader slot _ _) _) _) = slot
