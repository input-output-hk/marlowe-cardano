{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE StrictData #-}

module Language.Marlowe.Runtime.History where

import Control.Concurrent.Async (Concurrently(..))
import Control.Concurrent.STM (STM)
import Data.Foldable (asum)
import Language.Marlowe.Runtime.ChainSync.Api (RuntimeChainSeekClient, ScriptHash, SlotConfig)
import Language.Marlowe.Runtime.Core.Api (SomeMarloweVersion)
import Language.Marlowe.Runtime.History.FollowerSupervisor
import Language.Marlowe.Runtime.History.JobServer
import Language.Marlowe.Runtime.History.QueryServer
import Language.Marlowe.Runtime.History.Store
  (HistoryQueries, HistoryStore(..), HistoryStoreDependencies(..), mkHistoryStore)
import Language.Marlowe.Runtime.History.SyncServer
  (HistorySyncServer(..), HistorySyncServerDependencies(..), RunSyncServer, mkHistorySyncServer)
import Numeric.Natural (Natural)

data HistoryDependencies = HistoryDependencies
  { acceptRunJobServer   :: IO (RunJobServer IO)
  , acceptRunQueryServer :: IO (RunQueryServer IO)
  , getMarloweVersion    :: ScriptHash -> Maybe (SomeMarloweVersion, ScriptHash)
  , connectToChainSeek   :: forall a. RuntimeChainSeekClient IO a -> IO a
  , followerPageSize     :: Natural
  , slotConfig           :: SlotConfig
  , securityParameter    :: Int
  , acceptRunSyncServer  :: IO (RunSyncServer IO)
  , historyQueries       :: HistoryQueries IO
  }

newtype History = History
  { runHistory :: IO ()
  }

mkHistory :: HistoryDependencies -> STM History
mkHistory HistoryDependencies{..} = do
  FollowerSupervisor{..} <- mkFollowerSupervisor FollowerSupervisorDependencies{..}
  HistoryJobServer{..} <- mkHistoryJobServer HistoryJobServerDependencies{..}
  HistoryQueryServer{..} <- mkHistoryQueryServer HistoryQueryServerDependencies{..}
  HistoryStore{..} <- mkHistoryStore HistoryStoreDependencies{..}
  HistorySyncServer{..} <- mkHistorySyncServer HistorySyncServerDependencies{..}
  pure History
    { runHistory = runConcurrently $ asum $ Concurrently <$>
        [ runFollowerSupervisor
        , runHistoryJobServer
        , runHistoryQueryServer
        , runHistoryStore
        , runHistorySyncServer
        ]
    }
