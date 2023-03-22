{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Defines a custom Monad for the web server's handler functions to run in.

module Language.Marlowe.Runtime.Web.Server.Monad
  where

import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Catch.Pure (MonadMask)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT(..), asks)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.With (MonadWith(..))
import Data.Coerce (coerce)
import Data.GeneralAllocate (GeneralAllocate(..), GeneralAllocated(..))
import Language.Marlowe.Runtime.ChainSync.Api (TxId)
import Language.Marlowe.Runtime.Core.Api (ContractId)
import Language.Marlowe.Runtime.Web.Server.SyncClient
  (LoadContract, LoadContractHeaders, LoadTransaction, LoadTransactions, LoadWithdrawal, LoadWithdrawals)
import Language.Marlowe.Runtime.Web.Server.TxClient (ApplyInputs, CreateContract, Submit, Withdraw)
import Servant

newtype AppM r a = AppM { runAppM :: ReaderT (AppEnv r) Handler a }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader (AppEnv r)
    , MonadFail
    , MonadCatch
    , MonadMask
    , MonadThrow
    , MonadBaseControl IO
    , MonadError ServerError
    , MonadBase IO
    )

instance MonadWith (AppM r) where
  type WithException (AppM r) = WithException (ExceptT ServerError IO)
  stateThreadingGeneralWith
    :: forall a b releaseReturn
     . GeneralAllocate (AppM r) (WithException (ExceptT ServerError IO)) releaseReturn b a
    -> (a -> AppM r b)
    -> AppM r (b, releaseReturn)
  stateThreadingGeneralWith (GeneralAllocate allocA) go = AppM . ReaderT $ \r -> do
    let
      allocA' :: (forall x. ExceptT ServerError IO x -> ExceptT ServerError IO x) -> (ExceptT ServerError IO) (GeneralAllocated (ExceptT ServerError IO) (WithException (ExceptT ServerError IO)) releaseReturn b a)
      allocA' restore = do
        let
          restore' :: forall x. AppM r x -> AppM r x
          restore' mx = AppM . ReaderT $ coerce . restore . coerce . (runReaderT . runAppM) mx
        GeneralAllocated a releaseA <- (coerce . runReaderT . runAppM) (allocA restore') r
        let
          releaseA' relTy = (coerce . runReaderT . runAppM) (releaseA relTy) r
        pure $ GeneralAllocated a releaseA'
    coerce $ stateThreadingGeneralWith (GeneralAllocate allocA') (flip (coerce . runReaderT . runAppM) r . go)

data AppEnv r = AppEnv
  { _loadContractHeaders :: LoadContractHeaders IO
  , _loadContract :: LoadContract IO
  , _loadWithdrawals :: LoadWithdrawals IO
  , _loadWithdrawal :: LoadWithdrawal IO
  , _loadTransactions :: LoadTransactions IO
  , _loadTransaction :: LoadTransaction IO
  , _createContract :: CreateContract IO
  , _withdraw :: Withdraw IO
  , _applyInputs :: ApplyInputs IO
  , _submitContract :: ContractId -> Submit r IO
  , _submitTransaction :: ContractId -> TxId -> Submit r IO
  , _submitWithdrawal :: TxId -> Submit r IO
  }

-- | Load a list of contract headers.
loadContractHeaders :: LoadContractHeaders (AppM r)
loadContractHeaders cFilter range = do
  load <- asks _loadContractHeaders
  liftIO $ load cFilter range

-- | Load a contract.
loadContract :: LoadContract (AppM r)
loadContract contractId = do
  load <- asks _loadContract
  liftIO $ load contractId

-- | Load a list of withdrawal headers.
loadWithdrawals :: LoadWithdrawals (AppM r)
loadWithdrawals wFilter range = do
  load <- asks _loadWithdrawals
  liftIO $ load wFilter range

-- | Load a list of withdrawal headers.
loadWithdrawal :: LoadWithdrawal (AppM r)
loadWithdrawal withdrawalId = do
  load <- asks _loadWithdrawal
  liftIO $ load withdrawalId

-- | Load a list of transactions for a contract.
loadTransactions :: LoadTransactions (AppM r)
loadTransactions contractId range = do
  load <- asks _loadTransactions
  liftIO $ load contractId range

-- | Load a transaction for a contract.
loadTransaction :: LoadTransaction (AppM r)
loadTransaction contractId txId = do
  load <- asks _loadTransaction
  liftIO $ load contractId txId

-- | Create a contract.
createContract :: CreateContract (AppM r)
createContract stakeCredential version addresses roles metadata minUTxODeposit contract = do
  create <- asks _createContract
  liftIO $ create stakeCredential version addresses roles metadata minUTxODeposit contract

-- | Apply inputs to a contract.
applyInputs :: ApplyInputs (AppM r)
applyInputs version addresses contractId metadata invalidBefore invalidHereafter inputs = do
  apply <- asks _applyInputs
  liftIO $ apply version addresses contractId metadata invalidBefore invalidHereafter inputs

-- | Withdraw funds from a role.
withdraw :: Withdraw (AppM r)
withdraw version addresses contractId role = do
  _withdraw <- asks _withdraw
  liftIO $ _withdraw version addresses contractId role

-- | Submit a contract creation transaction to the node
submitContract :: ContractId -> Submit r (AppM r)
submitContract contractId backend tx = do
  submit <- asks _submitContract
  liftIO $ submit contractId backend tx

-- | Submit a withdrawal transaction to the node
submitWithdrawal :: TxId -> Submit r (AppM r)
submitWithdrawal txId backend tx = do
  submit <- asks _submitWithdrawal
  liftIO $ submit txId backend tx

-- | Submit an apply inputs transaction to the node
submitTransaction :: ContractId -> TxId -> Submit r (AppM r)
submitTransaction contractId txId backend tx = do
  submit <- asks _submitTransaction
  liftIO $ submit contractId txId backend tx
