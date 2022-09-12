{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.Transaction.Server
  where

import Cardano.Api (ScriptDataSupportedInEra, Tx, TxBody)
import Control.Concurrent.Async (Concurrently(..))
import Control.Concurrent.STM (STM, atomically)
import Control.Exception (SomeException, catch)
import qualified Data.Aeson as Aeson
import Data.Map (Map)
import Data.Time (UTCTime)
import Data.Void (Void)
import Language.Marlowe.Runtime.ChainSync.Api (Address, BlockHeader, TokenName, TxId)
import Language.Marlowe.Runtime.Core.Api (Contract, ContractId, MarloweVersion, PayoutDatum, Redeemer)
import Language.Marlowe.Runtime.Transaction.Api
  ( ApplyInputsError
  , CreateError
  , JobId(..)
  , MarloweTxCommand(..)
  , SubmitError
  , SubmitStatus
  , WalletAddresses(..)
  , WithdrawError
  )
import Network.Protocol.Job.Server (JobServer(..), ServerStCmd, ServerStInit(..))
import System.IO (hPutStrLn, stderr)

newtype RunTransactionServer m = RunTransactionServer (forall a. JobServer MarloweTxCommand m a -> m a)

newtype TransactionServerDependencies = TransactionServerDependencies
  { acceptRunTransactionServer :: IO (RunTransactionServer IO)
  }

newtype TransactionServer = TransactionServer
  { runTransactionServer :: IO ()
  }

mkHistoryTransactionServer :: TransactionServerDependencies -> STM TransactionServer
mkHistoryTransactionServer TransactionServerDependencies{..} = do
  let
    runTransactionServer = do
      runServer <- acceptRunTransactionServer
      Worker{..} <- atomically $ mkWorker WorkerDependencies {..}
      runConcurrently $
        Concurrently (runWorker `catch` catchWorker) *> Concurrently runTransactionServer
  pure $ TransactionServer { runTransactionServer }

catchWorker :: SomeException -> IO ()
catchWorker = hPutStrLn stderr . ("Job worker crashed with exception: " <>) . show

newtype WorkerDependencies = WorkerDependencies
  { runServer  :: RunTransactionServer IO
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
            execSubmit era tx
      , recvMsgAttach = \case
          JobIdSubmit era txId ->
            attachSubmit era txId
      }

attachSubmit
  :: ScriptDataSupportedInEra era
  -> TxId
  -> IO (ServerStCmd MarloweTxCommand SubmitStatus SubmitError BlockHeader IO ())
attachSubmit = error "not implemented"

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
  :: ScriptDataSupportedInEra era
  -> Tx era
  -> IO (ServerStCmd MarloweTxCommand SubmitStatus SubmitError BlockHeader IO ())
execSubmit = error "not implemented"
