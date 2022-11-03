{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Language.Marlowe.Runtime.History.SyncServer
  where

import Colog (logError)
import Control.Concurrent.STM (STM, atomically, retry)
import Control.Exception (SomeException)
import Control.Monad.IO.Class (liftIO)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import Language.Marlowe.Protocol.Sync.Server
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader, ChainPoint, WithGenesis(..))
import Language.Marlowe.Runtime.Core.Api
import Language.Marlowe.Runtime.History.Api
import Language.Marlowe.Runtime.History.Store (GetNextStepsResponse(..))
import Language.Marlowe.Runtime.Logging.Colog.LogIO
  (ConcurrentlyLogIO(ConcurrentlyLogIO, runConcurrentlyLogIO), LogIO, catchLogIO)

newtype RunSyncServer m = RunSyncServer (forall a. MarloweSyncServer m a -> LogIO a)

data HistorySyncServerDependencies = HistorySyncServerDependencies
  { acceptRunSyncServer :: LogIO (RunSyncServer LogIO)
  , findContract        :: ContractId -> LogIO (Maybe (BlockHeader, SomeCreateStep))
  , intersectContract   :: forall v. ContractId -> MarloweVersion v -> [BlockHeader] -> LogIO (Maybe BlockHeader)
  , getNextSteps        :: forall v. ContractId -> MarloweVersion v -> ChainPoint -> LogIO (GetNextStepsResponse v)
  , followContract      :: ContractId -> STM Bool
  , followerStatuses    :: STM (Map ContractId FollowerStatus)
  }

newtype HistorySyncServer = HistorySyncServer
  { runHistorySyncServer :: LogIO ()
  }

mkHistorySyncServer :: HistorySyncServerDependencies -> STM HistorySyncServer
mkHistorySyncServer HistorySyncServerDependencies{..} = do
  let
    runHistorySyncServer = do
      runSyncServer <- acceptRunSyncServer
      Worker{..} <- liftIO $ atomically $ mkWorker WorkerDependencies {..}
      runConcurrentlyLogIO $
        ConcurrentlyLogIO (runWorker `catchLogIO` catchWorker) *> ConcurrentlyLogIO runHistorySyncServer
  pure $ HistorySyncServer { runHistorySyncServer }

catchWorker :: SomeException -> LogIO ()
catchWorker = logError . T.pack . mappend "Sync worker crashed with exception: " . show

data WorkerDependencies = WorkerDependencies
  { runSyncServer     :: RunSyncServer LogIO
  , findContract      :: ContractId -> LogIO (Maybe (BlockHeader, SomeCreateStep))
  , intersectContract :: forall v. ContractId -> MarloweVersion v -> [BlockHeader] -> LogIO (Maybe BlockHeader)
  , getNextSteps      :: forall v. ContractId -> MarloweVersion v -> ChainPoint -> LogIO (GetNextStepsResponse v)
  , followContract    :: ContractId -> STM Bool
  , followerStatuses  :: STM (Map ContractId FollowerStatus)
  }

newtype Worker = Worker
  { runWorker :: LogIO ()
  }

mkWorker :: WorkerDependencies -> STM Worker
mkWorker WorkerDependencies{..} =
  let
    RunSyncServer run = runSyncServer
  in
    pure Worker { runWorker = run server }

  where
    server :: MarloweSyncServer LogIO ()
    server = MarloweSyncServer $ pure $ ServerStInit
      { recvMsgFollowContract = followServer
      , recvMsgIntersect = intersectServer
      }

    followServer :: ContractId -> LogIO (ServerStFollow LogIO ())
    followServer contractId = do
      _ <- liftIO $ atomically $ followContract contractId
      liftIO $ atomically do
        statuses <- followerStatuses
        case Map.lookup contractId statuses of
          -- retry on pending, Nothing, or following so we can be sure we have the whole history.
          -- On waiting and error, we continue (findContract will pick up on
          -- the error).
          Just Pending       -> retry
          Just (Following _) -> retry
          Nothing            -> retry
          _                  -> pure ()
      result <- findContract contractId
      pure case result of
        Nothing                                               -> SendMsgContractNotFound ()
        Just (blockHeader, SomeCreateStep version createStep) -> SendMsgContractFound blockHeader version createStep $ idleServer contractId version $ At blockHeader

    intersectServer :: ContractId -> MarloweVersion v -> [BlockHeader] -> LogIO (ServerStIntersect v LogIO ())
    intersectServer contractId version blockHeaders = do
      result <- intersectContract contractId version blockHeaders
      pure case result of
        Nothing          -> SendMsgIntersectNotFound ()
        Just blockHeader -> SendMsgIntersectFound blockHeader $ idleServer contractId version $ At blockHeader

    idleServer :: ContractId -> MarloweVersion v -> ChainPoint -> ServerStIdle v LogIO ()
    idleServer contractId version point = ServerStIdle
      { recvMsgDone = pure ()
      , recvMsgRequestNext = nextServer contractId version point
      }

    nextServer :: ContractId -> MarloweVersion v -> ChainPoint -> LogIO (ServerStNext v LogIO ())
    nextServer contractId version point = do
      result <- getNextSteps contractId version point
      pure case result of
        Rollback Genesis -> SendMsgRollBackCreation ()
        Rollback (At blockHeader) -> SendMsgRollBackward blockHeader $ idleServer contractId version $ At blockHeader
        Next blockHeader steps -> SendMsgRollForward blockHeader steps $ idleServer contractId version $ At blockHeader
        Wait lastUpdateAtRequest lastUpdated -> SendMsgWait $ waitServer contractId version point lastUpdateAtRequest lastUpdated

    waitServer :: ContractId -> MarloweVersion v -> ChainPoint -> BlockHeader -> STM ChainPoint -> ServerStWait v LogIO ()
    waitServer contractId version blockHeader requestedAt lastUpdated = ServerStWait
      { recvMsgPoll = do
          hasChanged <- liftIO $ atomically $ fmap (== requestedAt) <$> lastUpdated
          case hasChanged of
            Genesis  -> pure $ SendMsgRollBackCreation ()
            At True  -> nextServer contractId version blockHeader
            At False -> pure $ SendMsgWait $ waitServer contractId version blockHeader requestedAt lastUpdated
      , recvMsgCancel = pure $ idleServer contractId version blockHeader
      }
