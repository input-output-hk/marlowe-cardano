{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Defines a custom Monad for the web server's handler functions to run in.

module Language.Marlowe.Runtime.Web.Server.Monad
  where

import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Catch.Pure (MonadMask)
import Control.Monad.Event.Class
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader, ReaderT(..), ask)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.With (MonadWith(..))
import Data.Coerce (coerce)
import Data.GeneralAllocate (GeneralAllocate(..), GeneralAllocated(..))
import Language.Marlowe.Runtime.ChainSync.Api (TxId)
import Language.Marlowe.Runtime.Core.Api (ContractId)
import Language.Marlowe.Runtime.Web.Server.SyncClient
  (LoadContract, LoadContractHeaders, LoadTransaction, LoadTransactions, LoadWithdrawal, LoadWithdrawals)
import Language.Marlowe.Runtime.Web.Server.TxClient (ApplyInputs, CreateContract, Submit, Submit', Withdraw)
import Observe.Event (EventBackend)
import Servant

newtype AppM a = AppM { runAppM :: ReaderT AppEnv Handler a }
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

newtype BackendM r s a = BackendM { runBackendM :: ReaderT (EventBackend (BackendM r s) r s) IO a }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadFail
    , MonadCatch
    , MonadMask
    , MonadThrow
    , MonadBaseControl IO
    , MonadBase IO
    , MonadUnliftIO
    )

instance MonadWith AppM where
  type WithException AppM = WithException (ExceptT ServerError IO)
  stateThreadingGeneralWith
    :: forall a b releaseReturn
     . GeneralAllocate AppM (WithException (ExceptT ServerError IO)) releaseReturn b a
    -> (a -> AppM b)
    -> AppM (b, releaseReturn)
  stateThreadingGeneralWith (GeneralAllocate allocA) go = AppM . ReaderT $ \r -> do
    let
      allocA' :: (forall x. ExceptT ServerError IO x -> ExceptT ServerError IO x) -> (ExceptT ServerError IO) (GeneralAllocated (ExceptT ServerError IO) (WithException (ExceptT ServerError IO)) releaseReturn b a)
      allocA' restore = do
        let
          restore' :: forall x. AppM x -> AppM x
          restore' mx = AppM . ReaderT $ coerce . restore . coerce . (runReaderT . runAppM) mx
        GeneralAllocated a releaseA <- (coerce . runReaderT . runAppM) (allocA restore') r
        let
          releaseA' relTy = (coerce . runReaderT . runAppM) (releaseA relTy) r
        pure $ GeneralAllocated a releaseA'
    coerce $ stateThreadingGeneralWith (GeneralAllocate allocA') (flip (coerce . runReaderT . runAppM) r . go)

instance MonadWith (BackendM r s) where
  type WithException (BackendM r s) = WithException IO
  stateThreadingGeneralWith
    :: forall a b releaseReturn
     . GeneralAllocate (BackendM r s) (WithException IO) releaseReturn b a
    -> (a -> BackendM r s b)
    -> BackendM r s (b, releaseReturn)
  stateThreadingGeneralWith (GeneralAllocate allocA) go = BackendM . ReaderT $ \r -> do
    let
      allocA' :: (forall x. IO x -> IO x) -> IO (GeneralAllocated IO (WithException IO) releaseReturn b a)
      allocA' restore = do
        let
          restore' :: forall x. BackendM r s x -> BackendM r s x
          restore' mx = BackendM . ReaderT $ restore . (runReaderT . runBackendM) mx
        GeneralAllocated a releaseA <- (runReaderT . runBackendM) (allocA restore') r
        let
          releaseA' relTy = (runReaderT . runBackendM) (releaseA relTy) r
        pure $ GeneralAllocated a releaseA'
    stateThreadingGeneralWith (GeneralAllocate allocA') (flip (runReaderT . runBackendM) r . go)

instance MonadEvent r s (BackendM r s) where
  askBackend = askBackendReaderT BackendM id
  localBackend = localBackendReaderT BackendM runBackendM id

data AppEnv = forall r s. AppEnv
  { _loadContractHeaders :: LoadContractHeaders (BackendM r s)
  , _loadContract :: LoadContract (BackendM r s)
  , _loadWithdrawals :: LoadWithdrawals (BackendM r s)
  , _loadWithdrawal :: LoadWithdrawal (BackendM r s)
  , _loadTransactions :: LoadTransactions (BackendM r s)
  , _loadTransaction :: LoadTransaction (BackendM r s)
  , _createContract :: CreateContract (BackendM r s)
  , _withdraw :: Withdraw (BackendM r s)
  , _applyInputs :: ApplyInputs (BackendM r s)
  , _submitContract :: ContractId -> Submit r (BackendM r s)
  , _submitTransaction :: ContractId -> TxId -> Submit r (BackendM r s)
  , _submitWithdrawal :: TxId -> Submit r (BackendM r s)
  , _eventBackend :: EventBackend (BackendM r s) r s
  , _requestParent :: r
  }

-- | Load a list of contract headers.
loadContractHeaders :: LoadContractHeaders AppM
loadContractHeaders cFilter range = do
  AppEnv { _eventBackend = backend, _loadContractHeaders = load } <- ask
  liftBackendM backend $ load cFilter range

-- | Load a contract.
loadContract :: LoadContract AppM
loadContract contractId = do
  AppEnv { _eventBackend = backend, _loadContract = load } <- ask
  liftBackendM backend $ load contractId

-- | Load a list of withdrawal headers.
loadWithdrawals :: LoadWithdrawals AppM
loadWithdrawals wFilter range = do
  AppEnv { _eventBackend = backend, _loadWithdrawals = load } <- ask
  liftBackendM backend $ load wFilter range

-- | Load a list of withdrawal headers.
loadWithdrawal :: LoadWithdrawal AppM
loadWithdrawal withdrawalId = do
  AppEnv { _eventBackend = backend, _loadWithdrawal = load } <- ask
  liftBackendM backend $ load withdrawalId

-- | Load a list of transactions for a contract.
loadTransactions :: LoadTransactions AppM
loadTransactions contractId range = do
  AppEnv { _eventBackend = backend, _loadTransactions = load } <- ask
  liftBackendM backend $ load contractId range

-- | Load a transaction for a contract.
loadTransaction :: LoadTransaction AppM
loadTransaction contractId txId = do
  AppEnv { _eventBackend = backend, _loadTransaction = load } <- ask
  liftBackendM backend $ load contractId txId

-- | Create a contract.
createContract :: CreateContract AppM
createContract stakeCredential version addresses roles metadata minUTxODeposit contract = do
  AppEnv { _eventBackend = backend, _createContract = create } <- ask
  liftBackendM backend $ create stakeCredential version addresses roles metadata minUTxODeposit contract

-- | Apply inputs to a contract.
applyInputs :: ApplyInputs AppM
applyInputs version addresses contractId metadata invalidBefore invalidHereafter inputs = do
  AppEnv { _eventBackend = backend, _applyInputs = apply } <- ask
  liftBackendM backend $ apply version addresses contractId metadata invalidBefore invalidHereafter inputs

-- | Withdraw funds from a role.
withdraw :: Withdraw AppM
withdraw version addresses contractId role = do
  AppEnv { _eventBackend = backend, _withdraw = _withdraw } <- ask
  liftBackendM backend $ _withdraw version addresses contractId role

-- | Submit a contract creation transaction to the node
submitContract :: ContractId -> Submit' AppM
submitContract contractId tx = do
  AppEnv { _eventBackend = backend, _requestParent, _submitContract = submit } <- ask
  liftBackendM backend $ submit contractId _requestParent tx

-- | Submit a withdrawal transaction to the node
submitWithdrawal :: TxId -> Submit' AppM
submitWithdrawal txId tx = do
  AppEnv { _eventBackend = backend, _requestParent, _submitWithdrawal = submit } <- ask
  liftBackendM backend $ submit txId _requestParent tx

-- | Submit an apply inputs transaction to the node
submitTransaction :: ContractId -> TxId -> Submit' AppM
submitTransaction contractId txId tx = do
  AppEnv { _eventBackend = backend, _requestParent, _submitTransaction = submit } <- ask
  liftBackendM backend $ submit contractId txId _requestParent tx

liftBackendM :: EventBackend (BackendM r s) r s ->  BackendM r s a -> AppM a
liftBackendM backend m = liftIO $ runReaderT (runBackendM m) backend
