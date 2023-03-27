-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Types for testing Marlowe runtime contracts.
--
-----------------------------------------------------------------------------


{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
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
import Control.Concurrent.STM (TChan, TVar)
import Control.Lens (makeLenses)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Class (MonadState)
import Data.Aeson (FromJSON)
import qualified Data.Aeson as A
import Data.Aeson.Types (ToJSON)
import Data.Map (Map)
import Data.Time.Units (Second)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Language.Marlowe.CLI.Test.Contract (ContractNickname)
import Language.Marlowe.CLI.Test.Contract.ParametrizedMarloweJSON
import Language.Marlowe.CLI.Test.ExecutionMode (ExecutionMode)
import qualified Language.Marlowe.CLI.Test.Operation.Aeson as Operation
import Language.Marlowe.CLI.Test.Wallet.Types (Currencies(Currencies), WalletNickname(WalletNickname), Wallets(Wallets))
import Language.Marlowe.CLI.Types (CliError)
import Language.Marlowe.Cardano.Thread (AnyMarloweThread, MarloweThread, anyMarloweThread)
import qualified Language.Marlowe.Core.V1.Semantics.Types as M
import qualified Language.Marlowe.Protocol.Client as Marlowe.Protocol
import qualified Language.Marlowe.Runtime.ChainSync.Api as ChainSync
import Language.Marlowe.Runtime.Core.Api (ContractId)
import Ledger.Orphans ()
import qualified Network.Protocol.Connection as Network.Protocol
import qualified Network.Protocol.Driver as Network.Protocol
import qualified Network.Protocol.Handshake.Client as Network.Protocol

-- Curretly we don't need any extra information for the runtime thread.
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

newtype RuntimeContractInfo lang era =
  RuntimeContractInfo { _rcMarloweThread :: AnyRuntimeMarloweThread lang era }

newtype RuntimeMonitorInput = RuntimeMonitorInput (TChan (ContractNickname, ContractId))

newtype RuntimeMonitorState lang era = RuntimeMonitorState (TVar (Map ContractNickname (RuntimeContractInfo lang era)))

newtype RuntimeMonitor = RuntimeMonitor { runMonitor :: IO RuntimeError }

data RuntimeOperation =
    RuntimeAwaitCreated
    {
      roContractNickname :: ContractNickname
    , roTimeout :: Maybe Integer
    }
  | RuntimeAwaitInputsApplied
    {
      roContractNickname :: ContractNickname
    , roAllInputs :: [ParametrizedMarloweJSON] -- ^ Inputs to the contract.
    , roTimeout :: Maybe Integer
    }
  | RuntimeAwaitClosed
    {
      roContractNickname :: ContractNickname
    , roTimeout :: Maybe Integer
    }
  | RuntimeCreateContract
    {
      roContractNickname :: ContractNickname
    , roSubmitter :: Maybe WalletNickname -- ^ A wallet which gonna submit the initial transaction.
    , roMinLovelace :: Word64
    , roContract :: ParametrizedMarloweJSON -- ^ The Marlowe contract to be created.
    -- , roTimeout :: Maybe Integer
    }
  | RuntimeApplyInputs
    {
      roContractNickname :: ContractNickname
    , roInputs :: [ParametrizedMarloweJSON]  -- ^ Inputs to the contract.
    , roSubmitter :: Maybe WalletNickname -- ^ A wallet which gonna submit the initial transaction.
    -- , roTimeout :: Maybe Integer
    -- , coMinimumTime          :: SomeTimeout
    -- , coMaximumTime          :: SomeTimeout
    }

  deriving stock (Eq, Generic, Show)


instance FromJSON RuntimeOperation where
  parseJSON = do
    A.genericParseJSON $ Operation.genericParseJSONOptions "ro"

instance ToJSON RuntimeOperation where
  toJSON = do
    A.genericToJSON $ Operation.genericParseJSONOptions "ro"


data InterpretState era = InterpretState
  {
    _isKnownContracts :: Map ContractNickname ContractId
  , _isWallets :: Wallets era
  , _isCurrencies :: Currencies
  }

data InterpretEnv lang era = InterpretEnv
  {
    _ieRuntimeMonitorState :: RuntimeMonitorState lang era
  , _ieRuntimeMonitorInput :: RuntimeMonitorInput
  , _ieRuntimeClientConnector :: Network.Protocol.SomeClientConnector Marlowe.Protocol.MarloweClient IO
  , _ieExecutionMode :: ExecutionMode
  , _ieConnection :: LocalNodeConnectInfo CardanoMode
  , _ieEra :: ScriptDataSupportedInEra era
  }

type InterpretMonad m lang era =
  ( MonadState (InterpretState era) m
  , MonadReader (InterpretEnv lang era) m
  , MonadError CliError m
  , MonadIO m
  )

makeLenses 'RuntimeContractInfo
makeLenses 'InterpretState
makeLenses 'InterpretEnv

