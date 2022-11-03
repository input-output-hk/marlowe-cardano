{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE StrictData #-}

module Language.Marlowe.Runtime.History
  where

import Control.Concurrent.STM (STM)
import Data.Foldable (asum)
import Language.Marlowe.Runtime.ChainSync.Api (RuntimeChainSeekClient, SlotConfig)
import Language.Marlowe.Runtime.History.FollowerSupervisor
import Language.Marlowe.Runtime.History.JobServer
import Language.Marlowe.Runtime.History.QueryServer
import Language.Marlowe.Runtime.History.Store
  (HistoryQueries, HistoryStore(..), HistoryStoreDependencies(..), mkHistoryStore)
import Language.Marlowe.Runtime.History.SyncServer
  (HistorySyncServer(..), HistorySyncServerDependencies(..), RunSyncServer, mkHistorySyncServer)
import Language.Marlowe.Runtime.Logging.Colog (prefixLogger)
import Language.Marlowe.Runtime.Logging.Colog.LogIO (ConcurrentlyLogIO(ConcurrentlyLogIO), LogIO, runConcurrentlyLogIO)
import Numeric.Natural (Natural)

data HistoryDependencies m = HistoryDependencies
  { acceptRunJobServer   :: m (RunJobServer m)
  , acceptRunQueryServer :: m (RunQueryServer m)
  , connectToChainSeek   :: forall a. RuntimeChainSeekClient m a -> m a
  , followerPageSize     :: Natural
  , slotConfig           :: SlotConfig
  , securityParameter    :: Int
  , acceptRunSyncServer  :: m (RunSyncServer m)
  , historyQueries       :: HistoryQueries m
  }

newtype History m = History
  { runHistory :: m ()
  }

mkHistory :: HistoryDependencies LogIO -> STM (History LogIO)
mkHistory HistoryDependencies{..} = do
  FollowerSupervisor{..} <- mkFollowerSupervisor FollowerSupervisorDependencies{..}
  HistoryJobServer{..} <- mkHistoryJobServer HistoryJobServerDependencies{..}
  HistoryQueryServer{..} <- mkHistoryQueryServer HistoryQueryServerDependencies{..}
  HistoryStore{..} <- mkHistoryStore HistoryStoreDependencies{..}
  HistorySyncServer{..} <- mkHistorySyncServer HistorySyncServerDependencies{..}
  pure History
    { runHistory = runConcurrentlyLogIO $ asum $ ConcurrentlyLogIO <$>
        [ prefixLogger "[Follower Supervisor] " runFollowerSupervisor
        , prefixLogger "[Job Server] " runHistoryJobServer
        , prefixLogger "[Query Server] " runHistoryQueryServer
        , prefixLogger "[Store] " runHistoryStore
        , prefixLogger "[Sync] " runHistorySyncServer
        ]
    }
