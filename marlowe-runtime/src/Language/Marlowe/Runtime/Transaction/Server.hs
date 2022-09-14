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
import Control.Concurrent.STM
  (STM, atomically, modifyTVar, newEmptyTMVar, newTVar, putTMVar, readTVar, retry, tryTakeTMVar, writeTVar)
import Control.Exception (SomeException, catch)
import qualified Data.Aeson as Aeson
import Data.Either (fromRight)
import Data.Functor (void)
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
import Network.Protocol.Job.Server
  (JobServer(..), ServerStAttach(..), ServerStAwait(..), ServerStCmd(..), ServerStInit(..), hoistAttach, hoistCmd)
import System.IO (hPutStrLn, stderr)

newtype RunTransactionServer m = RunTransactionServer (forall a. JobServer MarloweTxCommand m a -> m a)

type RunSubmitJob era = ScriptDataSupportedInEra era -> Tx era -> (SubmitStatus -> IO ()) -> IO (Either SubmitError BlockHeader)

data TransactionServerDependencies = TransactionServerDependencies
  { acceptRunTransactionServer :: IO (RunTransactionServer IO)
  , runSubmitJob               :: forall era. RunSubmitJob era
  }

newtype TransactionServer = TransactionServer
  { runTransactionServer :: IO ()
  }

mkHistoryTransactionServer :: TransactionServerDependencies -> STM TransactionServer
mkHistoryTransactionServer TransactionServerDependencies{..} = do
  submitJobsVar <- newTVar mempty
  let
    getSubmitJob txId = do
      submitJobs <- readTVar submitJobsVar
      case Map.lookup txId submitJobs of
        Nothing     -> pure $ SendMsgAttachFailed ()
        Just server -> SendMsgAttached <$> server
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
  { runServer      :: RunTransactionServer IO
  , getSubmitJob   :: TxId -> STM (ServerStAttach MarloweTxCommand SubmitStatus SubmitError BlockHeader STM ())
  , trackSubmitJob :: TxId -> STM (ServerStCmd MarloweTxCommand SubmitStatus SubmitError BlockHeader STM ()) -> STM ()
  , runSubmitJob   :: forall era. RunSubmitJob era
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
            execSubmit runSubmitJob trackSubmitJob era tx
      , recvMsgAttach = \case
          JobIdSubmit _ txId ->
            attachSubmit getSubmitJob txId
      }

attachSubmit
  :: (TxId -> STM (ServerStAttach MarloweTxCommand SubmitStatus SubmitError BlockHeader STM ()))
  -> TxId
  -> IO (ServerStAttach MarloweTxCommand SubmitStatus SubmitError BlockHeader IO ())
attachSubmit getSubmitJob = atomically . fmap (hoistAttach atomically) . getSubmitJob

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
  :: RunSubmitJob era
  -> (TxId -> STM (ServerStCmd MarloweTxCommand SubmitStatus SubmitError BlockHeader STM ()) -> STM ())
  -> ScriptDataSupportedInEra era
  -> Tx era
  -> IO (ServerStCmd MarloweTxCommand SubmitStatus SubmitError BlockHeader IO ())
execSubmit runSubmitJob trackSubmitJob era tx = do
  let txId = TxId $ serialiseToRawBytes $ getTxId $ getTxBody tx
  (mkServer, runJob) <- atomically do
    -- A TVar that holds the current status of the job
    statusVar <- newTVar Submitting
    -- A TMVar that holds the result of the job when it is completed
    resultVar <- newEmptyTMVar
    -- An STM action to create a ServerStCmd that reads from the TVars above
    let mkServer = mkSubmitServer (JobIdSubmit era txId) (readTVar statusVar) (tryTakeTMVar resultVar)
    -- An IO action to run the actual submit job in a new thread.
    let
      runJob = void
        $ forkFinally (runSubmitJob era tx (atomically . writeTVar statusVar))
        $ atomically . putTMVar resultVar . fromRight (Left SubmitException)
    -- Save the server creator so that it can be attached to later.
    trackSubmitJob txId mkServer
    pure (mkServer, runJob)
  -- Run the job in a new thread
  runJob
  -- Make a new server and run it in IO.
  hoistCmd atomically <$> atomically mkServer

mkSubmitServer
  :: JobId MarloweTxCommand SubmitStatus SubmitError BlockHeader
  -> STM SubmitStatus
  -> STM (Maybe (Either SubmitError BlockHeader))
  -> STM (ServerStCmd MarloweTxCommand SubmitStatus SubmitError BlockHeader STM ())
mkSubmitServer jobId getSubmitStatus getResult = go
  where
    go = sendResultServer <|> sendStatusServer
    sendResultServer = do
      result <- maybe retry pure =<< getResult
      pure $ either (flip SendMsgFail ()) (flip SendMsgSucceed ()) result
    sendStatusServer = do
      status <- getSubmitStatus
      pure $ SendMsgAwait status jobId ServerStAwait
        { recvMsgDetach = pure ()
        , recvMsgPoll = go
        }
