{-# LANGUAGE GADTs       #-}
{-# LANGUAGE RankNTypes  #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE StrictData  #-}

module Language.Marlowe.Runtime.History where

import Control.Concurrent.Async (Concurrently (..))
import Control.Concurrent.STM (STM, atomically)
import Control.Monad (when)
import Data.Foldable (asum)
import Language.Marlowe.Runtime.ChainSync.Api (RuntimeChainSeekClient, ScriptHash, SlotConfig)
import Language.Marlowe.Runtime.Core.Api (SomeMarloweVersion, parseContractId)
import Language.Marlowe.Runtime.History.FollowerSupervisor
import Language.Marlowe.Runtime.History.JobServer
import Language.Marlowe.Runtime.History.QueryServer
import Language.Marlowe.Runtime.History.Store (HistoryQueries, HistoryStore (..), HistoryStoreDependencies (..),
                                               mkHistoryStore)
import Language.Marlowe.Runtime.History.SyncServer (HistorySyncServer (..), HistorySyncServerDependencies (..),
                                                    RunSyncServer, mkHistorySyncServer)
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
  let
    repl = do
      line <- getLine
      loop <- case words line of

        ["add", cidRaw] -> True <$ case parseContractId cidRaw of
          Nothing  -> putStrLn "invalid cid"
          Just cid -> print =<< atomically (followContract cid)

        ["rm", cidRaw] -> True <$ case parseContractId cidRaw of
          Nothing  -> putStrLn "invalid cid"
          Just cid -> print =<< atomically (stopFollowingContract cid)

        ["status"] -> do
          print =<< atomically followerStatuses
          pure True

        ["quit"] -> pure False

        _ -> True <$ putStrLn "invalid command"
      when loop repl
  pure History
    { runHistory = runConcurrently $ asum $ Concurrently <$>
        [ runFollowerSupervisor
        , runHistoryJobServer
        , runHistoryQueryServer
        , putStrLn "enter a command" *> repl
        , runHistoryStore
        , runHistorySyncServer
        ]
    }
