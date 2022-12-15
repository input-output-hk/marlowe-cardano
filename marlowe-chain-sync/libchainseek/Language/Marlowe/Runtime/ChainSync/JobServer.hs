{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Language.Marlowe.Runtime.ChainSync.JobServer
  where

import Cardano.Api (CardanoEra(..), CardanoMode, ScriptDataSupportedInEra(..), Tx, TxValidationErrorInMode)
import Control.Concurrent.Component
import Language.Marlowe.Runtime.ChainSync.Api (ChainSyncCommand(..))
import Network.Protocol.Driver (RunServer(..))
import Network.Protocol.Job.Server
import Ouroboros.Network.Protocol.LocalTxSubmission.Client (SubmitResult(..))

type RunJobServer m = RunServer m (JobServer ChainSyncCommand)

data ChainSyncJobServerDependencies = ChainSyncJobServerDependencies
  { acceptRunJobServer :: IO (RunJobServer IO)
  , submitTxToNodeLocal
      :: forall era
       . CardanoEra era
      -> Tx era
      -> IO (SubmitResult (TxValidationErrorInMode CardanoMode))
  }

chainSyncJobServer :: Component IO ChainSyncJobServerDependencies ()
chainSyncJobServer = serverComponent worker \ChainSyncJobServerDependencies{..} -> do
  runJobServer <- acceptRunJobServer
  pure WorkerDependencies {..}

data WorkerDependencies = WorkerDependencies
  { runJobServer      :: RunJobServer IO
  , submitTxToNodeLocal
      :: forall era
       . CardanoEra era
      -> Tx era
      -> IO (SubmitResult (TxValidationErrorInMode CardanoMode))
  }

worker :: Component IO WorkerDependencies ()
worker = component_ \WorkerDependencies{..} -> do
  let
    RunServer run = runJobServer

    server :: JobServer ChainSyncCommand IO ()
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
  run server
