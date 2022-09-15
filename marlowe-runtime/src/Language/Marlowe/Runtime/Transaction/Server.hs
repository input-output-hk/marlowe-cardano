{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}

module Language.Marlowe.Runtime.Transaction.Server
  where

import Cardano.Api (ScriptDataSupportedInEra, SerialiseAsRawBytes(serialiseToRawBytes), Tx, TxBody, getTxBody, getTxId)
import Control.Applicative ((<|>))
import Control.Concurrent (forkFinally)
import Control.Concurrent.Async (Concurrently(..))
import Control.Concurrent.STM (STM, atomically, modifyTVar, newEmptyTMVar, newTVar, putTMVar, readTMVar, readTVar)
import Control.Exception (SomeException, catch)
import qualified Data.Aeson as Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time (UTCTime)
import Data.Void (Void)
import Language.Marlowe.Runtime.ChainSync.Api (Address, BlockHeader, TokenName, TxId(TxId))
import Language.Marlowe.Runtime.Core.Api (Contract, ContractId, MarloweVersion, PayoutDatum, Redeemer)
import Language.Marlowe.Runtime.Transaction.Api
  ( ApplyInputsError
  , CreateError
  , JobId(..)
  , MarloweTxCommand(..)
  , SubmitError(..)
  , SubmitStatus(..)
  , WalletAddresses(..)
  , WithdrawError
  )
import Language.Marlowe.Runtime.Transaction.Submit (SubmitJob(..), SubmitJobStatus(..))
import Network.Protocol.Job.Server
  (JobServer(..), ServerStAttach(..), ServerStAwait(..), ServerStCmd(..), ServerStInit(..), hoistAttach, hoistCmd)
import System.IO (hPutStrLn, stderr)

newtype RunTransactionServer m = RunTransactionServer (forall a. JobServer MarloweTxCommand m a -> m a)

data TransactionServerDependencies = TransactionServerDependencies
  { acceptRunTransactionServer :: IO (RunTransactionServer IO)
  , mkSubmitJob :: forall era. ScriptDataSupportedInEra era -> Tx era -> STM SubmitJob
  }

newtype TransactionServer = TransactionServer
  { runTransactionServer :: IO ()
  }

mkTransactionServer :: TransactionServerDependencies -> STM TransactionServer
mkTransactionServer TransactionServerDependencies{..} = do
  submitJobsVar <- newTVar mempty
  let
    getSubmitJob txId = Map.lookup txId <$> readTVar submitJobsVar
    trackSubmitJob txId = modifyTVar submitJobsVar . Map.insert txId
    runTransactionServer = do
      runServer <- acceptRunTransactionServer
      Worker{..} <- atomically $ mkWorker WorkerDependencies {..}
      runConcurrently $
        Concurrently (runWorker `catch` catchWorker) *> Concurrently runTransactionServer
  pure $ TransactionServer { runTransactionServer }

catchWorker :: SomeException -> IO ()
catchWorker = hPutStrLn stderr . ("Job worker crashed with exception: " <>) . show

data WorkerDependencies = WorkerDependencies
  { runServer :: RunTransactionServer IO
  , getSubmitJob :: TxId -> STM (Maybe SubmitJob)
  , trackSubmitJob :: TxId -> SubmitJob -> STM ()
  , mkSubmitJob :: forall era. ScriptDataSupportedInEra era -> Tx era -> STM SubmitJob
  }

newtype Worker = Worker
  { runWorker :: IO ()
  }

mkWorker :: WorkerDependencies -> STM Worker
mkWorker WorkerDependencies{..} =
  let
    RunTransactionServer run = runServer
  in
    pure Worker { runWorker = run server }

  where
    server :: JobServer MarloweTxCommand IO ()
    server = JobServer $ pure serverInit

    serverInit :: ServerStInit MarloweTxCommand IO ()
    serverInit = ServerStInit
      { recvMsgExec = \case
          Create era version addresses roles metadata contract ->
            execCreate era version addresses roles metadata contract
          ApplyInputs era version addresses contractId invalidBefore invalidHereafter redeemer ->
            execApplyInputs era version addresses contractId invalidBefore invalidHereafter redeemer
          Withdraw era version addresses contractId payoutDatum ->
            execWithdraw era version addresses contractId payoutDatum
          Submit era tx ->
            execSubmit mkSubmitJob trackSubmitJob era tx
      , recvMsgAttach = \case
          jobId@(JobIdSubmit _ txId) ->
            attachSubmit jobId $ getSubmitJob txId
      }

attachSubmit
  :: JobId MarloweTxCommand SubmitStatus SubmitError BlockHeader
  -> STM (Maybe SubmitJob)
  -> IO (ServerStAttach MarloweTxCommand SubmitStatus SubmitError BlockHeader IO ())
attachSubmit jobId getSubmitJob =
  atomically $ fmap (hoistAttach atomically) <$> submitJobServerAttach jobId =<< getSubmitJob

execCreate
  :: ScriptDataSupportedInEra era
  -> MarloweVersion v
  -> WalletAddresses
  -> Map TokenName Address
  -> Map Int Aeson.Value
  -> Contract v
  -> IO (ServerStCmd MarloweTxCommand Void CreateError (ContractId, TxBody era) IO ())
execCreate = error "not implemented"

execApplyInputs
  :: ScriptDataSupportedInEra era
  -> MarloweVersion v
  -> WalletAddresses
  -> ContractId
  -> Maybe UTCTime
  -> Maybe UTCTime
  -> Redeemer v
  -> IO (ServerStCmd MarloweTxCommand Void ApplyInputsError (TxBody era) IO ())
execApplyInputs = error "not implemented"

execWithdraw
  :: ScriptDataSupportedInEra era
  -> MarloweVersion v
  -> WalletAddresses
  -> ContractId
  -> PayoutDatum v
  -> IO (ServerStCmd MarloweTxCommand Void WithdrawError (TxBody era) IO ())
execWithdraw = error "not implemented"

execSubmit
  :: (ScriptDataSupportedInEra era -> Tx era -> STM SubmitJob)
  -> (TxId -> SubmitJob -> STM ())
  -> ScriptDataSupportedInEra era
  -> Tx era
  -> IO (ServerStCmd MarloweTxCommand SubmitStatus SubmitError BlockHeader IO ())
execSubmit mkSubmitJob trackSubmitJob era tx = do
  let txId = TxId $ serialiseToRawBytes $ getTxId $ getTxBody tx
  (submitJob, exVar) <- atomically do
    exVar <- newEmptyTMVar
    submitJob <- mkSubmitJob era tx
    let getExceptionStatus = Failed SubmitException <$ readTMVar exVar
    let submitJob' = submitJob { submitJobStatus = getExceptionStatus <|> submitJobStatus submitJob }
    trackSubmitJob txId submitJob'
    pure (submitJob', exVar)
  -- Run the job in a new thread
  _ <- forkFinally (runSubmitJob submitJob) \case
    Left ex -> atomically $ putTMVar exVar ex
    _ -> pure ()
  -- Make a new server and run it in IO.
  hoistCmd atomically <$> atomically (submitJobServerCmd (JobIdSubmit era txId) submitJob)

submitJobServerAttach
  :: JobId MarloweTxCommand SubmitStatus SubmitError BlockHeader
  -> Maybe SubmitJob
  -> STM (ServerStAttach MarloweTxCommand SubmitStatus SubmitError BlockHeader STM ())
submitJobServerAttach jobId = maybe
  (pure $ SendMsgAttachFailed ())
  (fmap SendMsgAttached . submitJobServerCmd jobId)

submitJobServerCmd
  :: JobId MarloweTxCommand SubmitStatus SubmitError BlockHeader
  -> SubmitJob
  -> STM (ServerStCmd MarloweTxCommand SubmitStatus SubmitError BlockHeader STM ())
submitJobServerCmd jobId submitJob = do
  jobStatus <- submitJobStatus submitJob
  pure case jobStatus of
    Running status -> SendMsgAwait status jobId ServerStAwait
      { recvMsgDetach = pure ()
      , recvMsgPoll = submitJobServerCmd jobId submitJob
      }
    Succeeded block -> SendMsgSucceed block ()
    Failed err -> SendMsgFail err ()
