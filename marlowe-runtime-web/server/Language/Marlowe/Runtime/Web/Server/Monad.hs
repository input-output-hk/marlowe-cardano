{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Defines a custom Monad for the web server's handler functions to run in.
module Language.Marlowe.Runtime.Web.Server.Monad where

import Colog (LogAction, Message, hoistLogAction)
import Control.Concurrent.Component.Run (AppM, unAppM)
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Catch.Pure (MonadMask)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Control.Monad.Trans.Control (MonadBaseControl)
import Language.Marlowe.Runtime.ChainSync.Api (TxId)
import Language.Marlowe.Runtime.Core.Api (ContractId)
import Language.Marlowe.Runtime.Web.Server.ContractClient (GetContract, ImportBundle)
import Language.Marlowe.Runtime.Web.Server.SyncClient (
  LoadContract,
  LoadContractHeaders,
  LoadPayout,
  LoadPayouts,
  LoadTransaction,
  LoadTransactions,
  LoadWithdrawal,
  LoadWithdrawals,
 )
import Language.Marlowe.Runtime.Web.Server.TxClient (ApplyInputs, CreateContract, Submit, Submit', Withdraw)
import Observe.Event (EventBackend)
import Pipes (MFunctor (..))
import Servant

newtype ServerM a = ServerM {runServerM :: ReaderT AppEnv Handler a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader AppEnv
    , MonadFail
    , MonadCatch
    , MonadMask
    , MonadThrow
    , MonadBaseControl IO
    , MonadError ServerError
    , MonadBase IO
    )

data AppEnv = forall r s.
  AppEnv
  { _loadContractHeaders :: LoadContractHeaders (AppM r s)
  , _loadContract :: LoadContract (AppM r s)
  , _loadWithdrawals :: LoadWithdrawals (AppM r s)
  , _loadWithdrawal :: LoadWithdrawal (AppM r s)
  , _loadPayouts :: LoadPayouts (AppM r s)
  , _loadPayout :: LoadPayout (AppM r s)
  , _loadTransactions :: LoadTransactions (AppM r s)
  , _loadTransaction :: LoadTransaction (AppM r s)
  , _importBundle :: ImportBundle (AppM r s)
  , _createContract :: CreateContract (AppM r s)
  , _getContract :: GetContract (AppM r s)
  , _withdraw :: Withdraw (AppM r s)
  , _applyInputs :: ApplyInputs (AppM r s)
  , _submitContract :: ContractId -> Submit r (AppM r s)
  , _submitTransaction :: ContractId -> TxId -> Submit r (AppM r s)
  , _submitWithdrawal :: TxId -> Submit r (AppM r s)
  , _eventBackend :: EventBackend (AppM r s) r s
  , _requestParent :: r
  , _logAction :: LogAction IO Message
  }

-- | Load a page of contract headers.
importBundle :: ImportBundle ServerM
importBundle label = do
  AppEnv{_eventBackend = backend, _importBundle = ib} <- ask
  hoist (liftBackendM backend) $ ib label

-- | Load a page of contract headers.
loadContractHeaders :: LoadContractHeaders ServerM
loadContractHeaders cFilter range = do
  AppEnv{_eventBackend = backend, _loadContractHeaders = load} <- ask
  liftBackendM backend $ load cFilter range

-- | Load a contract.
loadContract :: LoadContract ServerM
loadContract contractId = do
  AppEnv{_eventBackend = backend, _loadContract = load} <- ask
  liftBackendM backend $ load contractId

-- | Load a page of withdrawal headers.
loadWithdrawals :: LoadWithdrawals ServerM
loadWithdrawals wFilter range = do
  AppEnv{_eventBackend = backend, _loadWithdrawals = load} <- ask
  liftBackendM backend $ load wFilter range

-- | Load a page of payouts.
loadPayouts :: LoadPayouts ServerM
loadPayouts wFilter range = do
  AppEnv{_eventBackend = backend, _loadPayouts = load} <- ask
  liftBackendM backend $ load wFilter range

-- | Load a page of payouts.
loadPayout :: LoadPayout ServerM
loadPayout payoutId = do
  AppEnv{_eventBackend = backend, _loadPayout = load} <- ask
  liftBackendM backend $ load payoutId

-- | Get a contract by its hash.
getContract :: GetContract ServerM
getContract hash = do
  AppEnv{_eventBackend = backend, _getContract = get} <- ask
  liftBackendM backend $ get hash

-- | Load a list of withdrawal headers.
loadWithdrawal :: LoadWithdrawal ServerM
loadWithdrawal withdrawalId = do
  AppEnv{_eventBackend = backend, _loadWithdrawal = load} <- ask
  liftBackendM backend $ load withdrawalId

-- | Load a list of transactions for a contract.
loadTransactions :: LoadTransactions ServerM
loadTransactions contractId range = do
  AppEnv{_eventBackend = backend, _loadTransactions = load} <- ask
  liftBackendM backend $ load contractId range

-- | Load a transaction for a contract.
loadTransaction :: LoadTransaction ServerM
loadTransaction contractId txId = do
  AppEnv{_eventBackend = backend, _loadTransaction = load} <- ask
  liftBackendM backend $ load contractId txId

-- | Create a contract.
createContract :: CreateContract ServerM
createContract stakeCredential version addresses threadName roles metadata minUTxODeposit state contract = do
  AppEnv{_eventBackend = backend, _createContract = create} <- ask
  liftBackendM backend $ create stakeCredential version addresses threadName roles metadata minUTxODeposit state contract

-- | Apply inputs to a contract.
applyInputs :: ApplyInputs ServerM
applyInputs version addresses contractId metadata invalidBefore invalidHereafter inputs = do
  AppEnv{_eventBackend = backend, _applyInputs = apply} <- ask
  liftBackendM backend $ apply version addresses contractId metadata invalidBefore invalidHereafter inputs

-- | Withdraw funds from a role.
withdraw :: Withdraw ServerM
withdraw version addresses payouts = do
  AppEnv{_eventBackend = backend, _withdraw = _withdraw} <- ask
  liftBackendM backend $ _withdraw version addresses payouts

-- | Submit a contract creation transaction to the node
submitContract :: ContractId -> Submit' ServerM
submitContract contractId era tx = do
  AppEnv{_eventBackend = backend, _requestParent, _submitContract = submit} <- ask
  liftBackendM backend $ submit contractId _requestParent era tx

-- | Submit a withdrawal transaction to the node
submitWithdrawal :: TxId -> Submit' ServerM
submitWithdrawal txId era tx = do
  AppEnv{_eventBackend = backend, _requestParent, _submitWithdrawal = submit} <- ask
  liftBackendM backend $ submit txId _requestParent era tx

-- | Submit an apply inputs transaction to the node
submitTransaction :: ContractId -> TxId -> Submit' ServerM
submitTransaction contractId txId era tx = do
  AppEnv{_eventBackend = backend, _requestParent, _submitTransaction = submit} <- ask
  liftBackendM backend $ submit contractId txId _requestParent era tx

liftBackendM :: EventBackend (AppM r s) r s -> AppM r s a -> ServerM a
liftBackendM backend m = ServerM do
  logAction <- asks _logAction
  liftIO $ runReaderT (unAppM m) (backend, hoistLogAction liftIO logAction)
