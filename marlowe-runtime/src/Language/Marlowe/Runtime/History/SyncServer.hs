{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StrictData            #-}

module Language.Marlowe.Runtime.History.SyncServer where

import Control.Concurrent.Async (Concurrently (Concurrently, runConcurrently))
import Control.Concurrent.STM (STM, atomically)
import Control.Exception (SomeException, catch)
import Language.Marlowe.Protocol.Sync.Server
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader, ChainPoint, WithGenesis (..))
import Language.Marlowe.Runtime.Core.Api
import Language.Marlowe.Runtime.History.Api
import Language.Marlowe.Runtime.History.Store (GetNextStepsResponse (..))
import System.IO (hPutStrLn, stderr)

newtype RunSyncServer m = RunSyncServer (forall a. MarloweSyncServer m a -> IO a)

data HistorySyncServerDependencies = HistorySyncServerDependencies
  { acceptRunSyncServer :: IO (RunSyncServer IO)
  , findContract        :: ContractId -> IO (Maybe (BlockHeader, SomeCreateStep))
  , intersectContract   :: forall v. ContractId -> MarloweVersion v -> [BlockHeader] -> IO (Maybe BlockHeader)
  , getNextSteps        :: forall v. ContractId -> MarloweVersion v -> ChainPoint -> IO (GetNextStepsResponse v)
  }

newtype HistorySyncServer = HistorySyncServer
  { runHistorySyncServer :: IO ()
  }

mkHistorySyncServer :: HistorySyncServerDependencies -> STM HistorySyncServer
mkHistorySyncServer HistorySyncServerDependencies{..} = do
  let
    runHistorySyncServer = do
      runSyncServer <- acceptRunSyncServer
      Worker{..} <- atomically $ mkWorker WorkerDependencies {..}
      runConcurrently $
        Concurrently (runWorker `catch` catchWorker) *> Concurrently runHistorySyncServer
  pure $ HistorySyncServer { runHistorySyncServer }

catchWorker :: SomeException -> IO ()
catchWorker = hPutStrLn stderr . ("Sync worker crashed with exception: " <>) . show

data WorkerDependencies = WorkerDependencies
  { runSyncServer     :: RunSyncServer IO
  , findContract      :: ContractId -> IO (Maybe (BlockHeader, SomeCreateStep))
  , intersectContract :: forall v. ContractId -> MarloweVersion v -> [BlockHeader] -> IO (Maybe BlockHeader)
  , getNextSteps      :: forall v. ContractId -> MarloweVersion v -> ChainPoint -> IO (GetNextStepsResponse v)
  }

newtype Worker = Worker
  { runWorker :: IO ()
  }

mkWorker :: WorkerDependencies -> STM Worker
mkWorker WorkerDependencies{..} =
  let
    RunSyncServer run = runSyncServer
  in
    pure Worker { runWorker = run server }

  where
    server :: MarloweSyncServer IO ()
    server = MarloweSyncServer $ pure $ ServerStInit
      { recvMsgFollowContract = followServer
      , recvMsgIntersect = intersectServer
      }

    followServer :: ContractId -> IO (ServerStFollow IO ())
    followServer contractId = do
      result <- findContract contractId
      pure case result of
        Nothing                                               -> SendMsgContractNotFound ()
        Just (blockHeader, SomeCreateStep version createStep) -> SendMsgContractFound blockHeader version createStep $ idleServer contractId version $ At blockHeader

    intersectServer :: ContractId -> MarloweVersion v -> [BlockHeader] -> IO (ServerStIntersect v IO ())
    intersectServer contractId version blockHeaders = do
      result <- intersectContract contractId version blockHeaders
      pure case result of
        Nothing          -> SendMsgIntersectNotFound ()
        Just blockHeader -> SendMsgIntersectFound blockHeader $ idleServer contractId version $ At blockHeader

    idleServer :: ContractId -> MarloweVersion v -> ChainPoint -> ServerStIdle v IO ()
    idleServer contractId version point = ServerStIdle
      { recvMsgDone = pure ()
      , recvMsgRequestNext = nextServer contractId version point
      }

    nextServer :: ContractId -> MarloweVersion v -> ChainPoint -> IO (ServerStNext v IO ())
    nextServer contractId version point = do
      result <- getNextSteps contractId version point
      pure case result of
        Rollback Genesis -> SendMsgRollBackCreation ()
        Rollback (At blockHeader) -> SendMsgRollBackward blockHeader $ idleServer contractId version $ At blockHeader
        Next blockHeader steps -> SendMsgRollForward blockHeader steps $ idleServer contractId version $ At blockHeader
        Wait lastUpdateAtRequest lastUpdated -> SendMsgWait $ waitServer contractId version point lastUpdateAtRequest lastUpdated

    waitServer :: ContractId -> MarloweVersion v -> ChainPoint -> BlockHeader -> STM ChainPoint -> ServerStWait v IO ()
    waitServer contractId version blockHeader requestedAt lastUpdated = ServerStWait
      { recvMsgPoll = do
          hasChanged <- atomically $ fmap (== requestedAt) <$> lastUpdated
          case hasChanged of
            Genesis  -> pure $ SendMsgRollBackCreation ()
            At True  -> nextServer contractId version blockHeader
            At False -> pure $ SendMsgWait $ waitServer contractId version blockHeader requestedAt lastUpdated
      , recvMsgCancel = pure $ idleServer contractId version blockHeader
      }
