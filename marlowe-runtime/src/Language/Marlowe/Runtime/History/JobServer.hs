{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Language.Marlowe.Runtime.History.JobServer
  where

import Colog (logError)
import Control.Concurrent.STM (STM, atomically, retry)
import Control.Exception (SomeException)
import Control.Monad.IO.Class (liftIO)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import Language.Marlowe.Runtime.Core.Api
import Language.Marlowe.Runtime.History.Api
import Language.Marlowe.Runtime.Logging.Colog.LogIO
  (ConcurrentlyLogIO(ConcurrentlyLogIO, runConcurrentlyLogIO), LogIO, catchLogIO)
import Network.Protocol.Job.Server (liftCommandHandler)

newtype RunJobServer m = RunJobServer (forall a. RuntimeHistoryJobServer m a -> LogIO a)

data HistoryJobServerDependencies = HistoryJobServerDependencies
  { acceptRunJobServer    :: LogIO (RunJobServer LogIO)
  , followContract        :: ContractId -> STM Bool
  , stopFollowingContract :: ContractId -> STM Bool
  , followerStatuses      :: STM (Map ContractId FollowerStatus)
  }

newtype HistoryJobServer = HistoryJobServer
  { runHistoryJobServer :: LogIO ()
  }

mkHistoryJobServer :: HistoryJobServerDependencies -> STM HistoryJobServer
mkHistoryJobServer HistoryJobServerDependencies{..} = do
  let
    runHistoryJobServer = do
      runJobServer <- acceptRunJobServer
      Worker{..} <- liftIO $ atomically $ mkWorker WorkerDependencies {..}
      runConcurrentlyLogIO $
        ConcurrentlyLogIO (runWorker `catchLogIO` catchWorker) *> ConcurrentlyLogIO runHistoryJobServer
  pure $ HistoryJobServer { runHistoryJobServer }

catchWorker :: SomeException -> LogIO ()
catchWorker = logError . T.pack . mappend "Job worker crashed with exception: " . show

data WorkerDependencies = WorkerDependencies
  { runJobServer          :: RunJobServer LogIO
  , followContract        :: ContractId -> STM Bool
  , stopFollowingContract :: ContractId -> STM Bool
  , followerStatuses      :: STM (Map ContractId FollowerStatus)
  }

newtype Worker = Worker
  { runWorker :: LogIO ()
  }

mkWorker :: WorkerDependencies -> STM Worker
mkWorker WorkerDependencies{..} =
  let
    RunJobServer run = runJobServer
  in
    pure Worker { runWorker = run server }

  where
    server :: RuntimeHistoryJobServer LogIO ()
    server = liftCommandHandler commandSchema $ \case
      Left (FollowContract contractId) -> do
        followed <- liftIO $ atomically $ followContract contractId
        if followed
          then liftIO $ atomically do
            statuses <- followerStatuses
            case Map.lookup contractId statuses of
              Nothing           -> retry
              Just Pending      -> retry
              Just (Failed err) -> pure $ Left err
              Just _            -> pure $ Right True
          else pure $ Right False
      Left (StopFollowingContract contractId) -> liftIO $ atomically $ Right <$> stopFollowingContract contractId
      Right jobId -> case jobId of
