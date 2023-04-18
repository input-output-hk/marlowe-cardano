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
import Control.Concurrent.Component.UnliftIO
import Control.Monad.Trans.Control (MonadBaseControl)
import Language.Marlowe.Runtime.ChainSync.Api (ChainSyncCommand(..))
import Network.Protocol.Connection (SomeConnectionSource, SomeServerConnector, acceptSomeConnector)
import Network.Protocol.Driver (runSomeConnector)
import Network.Protocol.Job.Server
import Ouroboros.Network.Protocol.LocalTxSubmission.Client (SubmitResult(..))
import UnliftIO (MonadUnliftIO)

data ChainSyncJobServerDependencies m = ChainSyncJobServerDependencies
  { jobSource :: SomeConnectionSource (JobServer ChainSyncCommand) m
  , submitTxToNodeLocal
      :: forall era
       . CardanoEra era
      -> Tx era
      -> m (SubmitResult (TxValidationErrorInMode CardanoMode))
  }

chainSyncJobServer :: (MonadBaseControl IO m, MonadUnliftIO m) => Component m (ChainSyncJobServerDependencies m) ()
chainSyncJobServer = serverComponent worker \ChainSyncJobServerDependencies{..} -> do
  connector <- acceptSomeConnector jobSource
  pure WorkerDependencies {..}

data WorkerDependencies m = WorkerDependencies
  { connector :: SomeServerConnector (JobServer ChainSyncCommand) m
  , submitTxToNodeLocal
      :: forall era
       . CardanoEra era
      -> Tx era
      -> m (SubmitResult (TxValidationErrorInMode CardanoMode))
  }

worker :: forall m. MonadBaseControl IO m => Component m (WorkerDependencies m) ()
worker = component_ \WorkerDependencies{..} -> do
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
  runSomeConnector connector server
