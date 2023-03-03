{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Defines a custom Monad for the web server's handler functions to run in.

module Language.Marlowe.Runtime.Web.Server.Monad
  where

import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Catch.Pure (MonadMask)
import Control.Monad.Cleanup (MonadCleanup(..))
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, asks)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Coerce (coerce)
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

instance MonadCleanup (AppM r) where
  generalCleanup acquire release action = coerce $ generalCleanup
    (toTransformers acquire)
    (\a b -> toTransformers $ release a b)
    (\a -> toTransformers $ action a)
    where
      toTransformers :: AppM r a -> ReaderT (AppEnv r) (ExceptT ServerError IO) a
      toTransformers = coerce

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
loadContractHeaders range = do
  load <- asks _loadContractHeaders
  liftIO $ load range

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
submitContract contractId mods tx = do
  submit <- asks _submitContract
  liftIO $ submit contractId mods tx

-- | Submit a withdrawal transaction to the node
submitWithdrawal :: TxId -> Submit r (AppM r)
submitWithdrawal txId mods tx = do
  submit <- asks _submitWithdrawal
  liftIO $ submit txId mods tx

-- | Submit an apply inputs transaction to the node
submitTransaction :: ContractId -> TxId -> Submit r (AppM r)
submitTransaction contractId txId mods tx = do
  submit <- asks _submitTransaction
  liftIO $ submit contractId txId mods tx
