{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Language.Marlowe.Runtime.ChainSync.JobServer
  where

import Cardano.Api (CardanoEra(..), CardanoMode, ScriptDataSupportedInEra(..), Tx, TxValidationErrorInMode)
import Control.Concurrent.Async (Concurrently(..))
import Control.Concurrent.STM (STM, atomically)
import Control.Exception (SomeException, catch)
import Data.Void (Void)
import Language.Marlowe.Runtime.ChainSync.Api (ChainSyncCommand(..))
import Network.Protocol.Job.Server
import Ouroboros.Network.Protocol.LocalTxSubmission.Client (SubmitResult(..))
import System.IO (hPutStrLn, stderr)

newtype RunJobServer m = RunJobServer (forall a. JobServer ChainSyncCommand m a -> IO a)

data ChainSyncJobServerDependencies = ChainSyncJobServerDependencies
  { acceptRunJobServer :: IO (RunJobServer IO)
  , submitTxToNodeLocal
      :: forall era
       . CardanoEra era
      -> Tx era
      -> IO (SubmitResult (TxValidationErrorInMode CardanoMode))
  }

newtype ChainSyncJobServer = ChainSyncJobServer
  { runChainSyncJobServer :: IO Void
  }

mkChainSyncJobServer :: ChainSyncJobServerDependencies -> STM ChainSyncJobServer
mkChainSyncJobServer ChainSyncJobServerDependencies{..} = do
  let
    runChainSyncJobServer = do
      runJobServer <- acceptRunJobServer
      Worker{..} <- atomically $ mkWorker WorkerDependencies {..}
      runConcurrently $
        Concurrently (runWorker `catch` catchWorker) *> Concurrently runChainSyncJobServer
  pure $ ChainSyncJobServer { runChainSyncJobServer }

catchWorker :: SomeException -> IO ()
catchWorker = hPutStrLn stderr . ("Job worker crashed with exception: " <>) . show

data WorkerDependencies = WorkerDependencies
  { runJobServer      :: RunJobServer IO
  , submitTxToNodeLocal
      :: forall era
       . CardanoEra era
      -> Tx era
      -> IO (SubmitResult (TxValidationErrorInMode CardanoMode))
  }

newtype Worker = Worker
  { runWorker :: IO ()
  }

mkWorker :: WorkerDependencies -> STM Worker
mkWorker WorkerDependencies{..} =
  let
    RunJobServer run = runJobServer
  in
    pure Worker { runWorker = run server }
  where
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
