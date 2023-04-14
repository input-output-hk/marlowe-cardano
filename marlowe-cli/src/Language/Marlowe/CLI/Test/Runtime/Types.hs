{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Marlowe.CLI.Test.Runtime.Types
  where

import Cardano.Api (CardanoMode, LocalNodeConnectInfo, ScriptDataSupportedInEra)
import qualified Cardano.Api as C
import qualified Contrib.Data.Time.Units.Aeson as A
import Control.Concurrent.STM (TChan, TVar)
import Control.Lens (Lens', Traversal', makeLenses)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Class (MonadState)
import Data.Aeson (FromJSON)
import qualified Data.Aeson as A
import Data.Aeson.Types (ToJSON)
import Data.Map (Map)
import Data.Time.Units
import Data.Word (Word64)
import GHC.Generics (Generic)
import Language.Marlowe.CLI.Test.Contract (ContractNickname)
import qualified Language.Marlowe.CLI.Test.Contract as Contract
import Language.Marlowe.CLI.Test.Contract.ParametrizedMarloweJSON
import Language.Marlowe.CLI.Test.ExecutionMode (ExecutionMode)
import qualified Language.Marlowe.CLI.Test.Operation.Aeson as Operation
import Language.Marlowe.CLI.Test.Wallet.Types (Currencies, CurrencyNickname, WalletNickname, Wallets)
import qualified Language.Marlowe.CLI.Test.Wallet.Types as Wallet
import Language.Marlowe.CLI.Types (CliError)
import Language.Marlowe.Cardano.Thread (AnyMarloweThread, MarloweThread, anyMarloweThread)
import qualified Language.Marlowe.Core.V1.Semantics.Types as M
import qualified Language.Marlowe.Protocol.Client as Marlowe.Protocol
import Language.Marlowe.Runtime.Core.Api (ContractId)
import Ledger.Orphans ()
import qualified Network.Protocol.Connection as Network.Protocol

-- Curretly we don't use any extra execution information from the Runtime.
type RuntimeTxInfo = ()

type RuntimeMarloweThread lang era = MarloweThread RuntimeTxInfo lang era

type AnyRuntimeMarloweThread lang era = AnyMarloweThread RuntimeTxInfo lang era

anyRuntimeMarloweThread :: Maybe C.TxIn
                        -> [M.Input]
                        -> AnyRuntimeMarloweThread lang era
                        -> Maybe (AnyRuntimeMarloweThread lang era)
anyRuntimeMarloweThread = anyMarloweThread ()

data RuntimeError
  = RuntimeConnectionError
  | RuntimeExecutionFailure String
  | RuntimeContractNotFound ContractId
  | RuntimeRollbackError ContractNickname
  deriving stock (Eq, Generic, Show)

-- FIXME: Drop this
newtype RuntimeContractInfo lang era =
  RuntimeContractInfo { _rcMarloweThread :: AnyRuntimeMarloweThread lang era }

newtype RuntimeMonitorInput = RuntimeMonitorInput (TChan (ContractNickname, ContractId))

newtype RuntimeMonitorState lang era = RuntimeMonitorState (TVar (Map ContractNickname (RuntimeContractInfo lang era)))

newtype RuntimeMonitor = RuntimeMonitor { runMonitor :: IO RuntimeError }

defaultOperationTimeout :: Second
defaultOperationTimeout = 30

data RuntimeOperation =
    RuntimeAwaitCreated
    {
      roContractNickname :: ContractNickname
    , roTimeout :: Maybe A.Second -- ^ Submission timeout.
    }
  | RuntimeAwaitInputsApplied
    {
      roContractNickname :: ContractNickname
    , roTimeout :: Maybe A.Second
    }
  | RuntimeAwaitClosed
    {
      roContractNickname :: ContractNickname
    , roTimeout :: Maybe A.Second
    }
  | RuntimeCreateContract
    {
      roContractNickname :: ContractNickname
    , roSubmitter :: Maybe WalletNickname -- ^ A wallet which gonna submit the initial transaction.
    , roMinLovelace :: Word64
    , roRoleCurrency :: Maybe CurrencyNickname  -- ^ If contract uses roles then currency is required.
    , roContractSource :: Contract.Source         -- ^ The Marlowe contract to be created.
    , roAwaitConfirmed :: Maybe A.Second -- ^ How long to wait for the transaction to be confirmed in the Runtime. By default we don't wait.
    }
  | RuntimeApplyInputs
    {
      roContractNickname :: ContractNickname
    , roInputs :: [ParametrizedMarloweJSON]  -- ^ Inputs to the contract.
    , roSubmitter :: Maybe WalletNickname -- ^ A wallet which gonna submit the initial transaction.
    , roAwaitConfirmed :: Maybe A.Second -- ^ How long to wait for the transaction to be confirmed in the Runtime. By default we wait 30s.
    }
  deriving stock (Eq, Generic, Show)

instance FromJSON RuntimeOperation where
  parseJSON = do
    A.genericParseJSON $ Operation.genericJSONOptions "ro"

instance ToJSON RuntimeOperation where
  toJSON = do
    A.genericToJSON $ Operation.genericJSONOptions "ro"

data ContractInfo = ContractInfo
  { ciContractId :: ContractId
  , ciAppliedInputs :: [M.Input] -- ^ Inputs which we applied. Possibly unconfirmed yet.
  }

class HasInterpretState st lang era | st -> lang era where
  knownContractsL :: Lens' st (Map ContractNickname ContractInfo)
  walletsL :: Lens' st (Wallets era)
  currenciesL :: Lens' st Currencies

class HasInterpretEnv env lang era | env -> lang era where
  runtimeMonitorStateT :: Traversal' env (RuntimeMonitorState lang era)
  runtimeMonitorInputT :: Traversal' env RuntimeMonitorInput
  runtimeClientConnectorT :: Traversal' env (Network.Protocol.SomeClientConnector Marlowe.Protocol.MarloweRuntimeClient IO)
  executionModeL :: Lens' env ExecutionMode
  connectionT :: Traversal' env (LocalNodeConnectInfo CardanoMode)
  eraL :: Lens' env (ScriptDataSupportedInEra era)

type InterpretMonad env st m lang era =
  ( MonadState st m
  , HasInterpretState st lang era
  , MonadReader env m
  , HasInterpretEnv env lang era
  , Wallet.InterpretMonad env st m era
  , MonadError CliError m
  , MonadIO m
  )

makeLenses 'RuntimeContractInfo
