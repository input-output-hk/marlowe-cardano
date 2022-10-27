{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Language.Marlowe.Runtime.ChainSync.JobServer
  where

import Cardano.Api (CardanoEra(..), CardanoMode, ScriptDataSupportedInEra(..), Tx, TxValidationErrorInMode)
import Colog (logError)
import Control.Concurrent.STM (STM, atomically)
import Control.Exception (SomeException)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Data.Void (Void)
import GHC.Stack (HasCallStack)
import Language.Marlowe.Runtime.ChainSync.Api (ChainSyncCommand(..), commandSchema)
import Language.Marlowe.Runtime.Logging.Colog.LogIO (ConcurrentlyLogIO(..), LogIO, catchLogIO)
import Network.Protocol.Job.Server
import Ouroboros.Network.Protocol.LocalTxSubmission.Client (SubmitResult(..))

newtype RunJobServer m = RunJobServer (forall a. JobServer ChainSyncCommand m a -> LogIO a)

data ChainSyncJobServerDependencies = ChainSyncJobServerDependencies
  { acceptRunJobServer :: LogIO (RunJobServer LogIO)
  , submitTxToNodeLocal
      :: forall era
       . CardanoEra era
      -> Tx era
      -> LogIO (SubmitResult (TxValidationErrorInMode CardanoMode))
  }

newtype ChainSyncJobServer = ChainSyncJobServer
  { runChainSyncJobServer :: LogIO Void
  }

mkChainSyncJobServer :: ChainSyncJobServerDependencies -> STM ChainSyncJobServer
mkChainSyncJobServer ChainSyncJobServerDependencies{..} = do
  let
    runChainSyncJobServer :: HasCallStack => LogIO Void
    runChainSyncJobServer = do
      runJobServer <- acceptRunJobServer
      Worker{..} <- liftIO $ atomically $ mkWorker WorkerDependencies {..}
      runConcurrentlyLogIO $
        ConcurrentlyLogIO (runWorker `catchLogIO` catchWorker) *> ConcurrentlyLogIO runChainSyncJobServer
  pure $ ChainSyncJobServer { runChainSyncJobServer }

catchWorker :: SomeException -> LogIO ()
catchWorker = logError . T.pack . mappend "Job worker crashed with exception: " . show

data WorkerDependencies = WorkerDependencies
  { runJobServer      :: RunJobServer LogIO
  , submitTxToNodeLocal
      :: forall era
       . CardanoEra era
      -> Tx era
      -> LogIO (SubmitResult (TxValidationErrorInMode CardanoMode))
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
    server :: JobServer ChainSyncCommand LogIO ()
    server = liftCommandHandler commandSchema $ flip either (\case) \case
      SubmitTx era tx -> do
        result <- submitTxToNodeLocal
          case era of
            ScriptDataInAlonzoEra -> AlonzoEra
            ScriptDataInBabbageEra -> BabbageEra
          tx
        pure case result of
          SubmitFail err -> Left $ show err
          SubmitSuccess -> Right ()
