{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Language.Marlowe.Runtime.ChainSync.JobServer
  where

import Cardano.Api (CardanoEra(..), CardanoMode, ScriptDataSupportedInEra(..), Tx, TxValidationErrorInMode)
import Colog (Message, WithLog)
import Control.Concurrent.Component
import Control.Monad.Event.Class
import Language.Marlowe.Runtime.ChainSync.Api (ChainSyncCommand(..))
import Network.Protocol.Connection (SomeConnectionSourceTraced, SomeServerConnectorTraced, acceptSomeConnectorTraced)
import Network.Protocol.Driver.Trace (HasSpanContext, runSomeConnectorTraced)
import Network.Protocol.Job.Server
import Ouroboros.Network.Protocol.LocalTxSubmission.Client (SubmitResult(..))
import UnliftIO (MonadUnliftIO)

data ChainSyncJobServerDependencies r s m = ChainSyncJobServerDependencies
  { jobSource :: SomeConnectionSourceTraced (JobServer ChainSyncCommand) r s m
  , submitTxToNodeLocal
      :: forall era
       . CardanoEra era
      -> Tx era
      -> m (SubmitResult (TxValidationErrorInMode CardanoMode))
  }

chainSyncJobServer
  :: (MonadUnliftIO m, MonadEvent r s m, HasSpanContext r, WithLog env Message m)
  => Component m (ChainSyncJobServerDependencies r s m) ()
chainSyncJobServer = serverComponent "chain-sync-job-server" worker \ChainSyncJobServerDependencies{..} -> do
  connector <- acceptSomeConnectorTraced jobSource
  pure WorkerDependencies {..}

data WorkerDependencies r s m = WorkerDependencies
  { connector :: SomeServerConnectorTraced (JobServer ChainSyncCommand) r s m
  , submitTxToNodeLocal
      :: forall era
       . CardanoEra era
      -> Tx era
      -> m (SubmitResult (TxValidationErrorInMode CardanoMode))
  }

worker
  :: forall r s env m. (MonadUnliftIO m, MonadEvent r s m, HasSpanContext r, WithLog env Message m)
  => Component m (WorkerDependencies r s m) ()
worker = component_ "chain-sync-job-worker" \WorkerDependencies{..} -> do
  let
    server :: JobServer ChainSyncCommand m ()
    server = liftCommandHandler $ flip either (\case) \case
      SubmitTx era tx -> ((),) <$> do
        result <- submitTxToNodeLocal
          case era of
            ScriptDataInAlonzoEra -> AlonzoEra
            ScriptDataInBabbageEra -> BabbageEra
          tx
        pure case result of
          SubmitFail err -> Left $ show err
          SubmitSuccess -> Right ()
  runSomeConnectorTraced connector server
