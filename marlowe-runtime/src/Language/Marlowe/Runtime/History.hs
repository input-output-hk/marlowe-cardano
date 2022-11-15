{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE StrictData #-}

module Language.Marlowe.Runtime.History
  where

import Control.Concurrent.Component
import Language.Marlowe.Runtime.ChainSync.Api (RuntimeChainSeekClient, SlotConfig)
import Language.Marlowe.Runtime.History.FollowerSupervisor
import Language.Marlowe.Runtime.History.JobServer
import Language.Marlowe.Runtime.History.QueryServer
import Language.Marlowe.Runtime.History.Store
  (HistoryQueries, HistoryStore(..), HistoryStoreDependencies(..), historyStore)
import Language.Marlowe.Runtime.History.SyncServer (HistorySyncServerDependencies(..), RunSyncServer, historySyncServer)
import Numeric.Natural (Natural)

data HistoryDependencies = HistoryDependencies
  { acceptRunJobServer   :: IO (RunJobServer IO)
  , acceptRunQueryServer :: IO (RunQueryServer IO)
  , connectToChainSeek   :: forall a. RuntimeChainSeekClient IO a -> IO a
  , followerPageSize     :: Natural
  , slotConfig           :: SlotConfig
  , securityParameter    :: Int
  , acceptRunSyncServer  :: IO (RunSyncServer IO)
  , historyQueries       :: HistoryQueries IO
  }

history :: Component IO HistoryDependencies ()
history = proc HistoryDependencies{..} -> do
  FollowerSupervisor{..} <- followerSupervisor -< FollowerSupervisorDependencies{..}
  historyJobServer -< HistoryJobServerDependencies{..}
  historyQueryServer -< HistoryQueryServerDependencies{..}
  HistoryStore{..} <- historyStore -< HistoryStoreDependencies{..}
  historySyncServer -< HistorySyncServerDependencies{..}
