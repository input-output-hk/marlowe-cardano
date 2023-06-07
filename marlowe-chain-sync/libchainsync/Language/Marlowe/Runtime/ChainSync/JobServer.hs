{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Language.Marlowe.Runtime.ChainSync.JobServer where

import Cardano.Api (CardanoEra(..), CardanoMode, ScriptDataSupportedInEra(..), Tx, TxValidationErrorInMode)
import Colog (Message, WithLog)
import Control.Concurrent.Component
import Language.Marlowe.Runtime.ChainSync.Api (ChainSyncCommand(..))
import Network.Protocol.Connection (ConnectionSource, Connector, acceptConnector, runConnector)
import Network.Protocol.Job.Server
import Ouroboros.Network.Protocol.LocalTxSubmission.Client (SubmitResult(..))
import UnliftIO (MonadUnliftIO)

data ChainSyncJobServerDependencies m = ChainSyncJobServerDependencies
  { jobSource :: ConnectionSource (JobServer ChainSyncCommand) m
  , submitTxToNodeLocal
      :: forall era
       . CardanoEra era
      -> Tx era
      -> m (SubmitResult (TxValidationErrorInMode CardanoMode))
  }

chainSyncJobServer
  :: (MonadUnliftIO m, WithLog env Message m)
  => Component m (ChainSyncJobServerDependencies m) ()
chainSyncJobServer = serverComponent "chain-sync-job-server" worker \ChainSyncJobServerDependencies{..} -> do
  connector <- acceptConnector jobSource
  pure WorkerDependencies {..}

data WorkerDependencies m = WorkerDependencies
  { connector :: Connector (JobServer ChainSyncCommand) m
  , submitTxToNodeLocal
      :: forall era
       . CardanoEra era
      -> Tx era
      -> m (SubmitResult (TxValidationErrorInMode CardanoMode))
  }

worker
  :: forall env m. (MonadUnliftIO m, WithLog env Message m)
  => Component m (WorkerDependencies m) ()
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
  runConnector connector server
