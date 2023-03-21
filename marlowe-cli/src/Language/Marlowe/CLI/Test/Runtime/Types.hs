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
import GHC.Generics (Generic)
import Language.Marlowe.CLI.Test.Contract (ContractNickname)
import Language.Marlowe.CLI.Test.ExecutionMode (ExecutionMode)
import qualified Language.Marlowe.CLI.Test.Operation.Aeson as Operation
import Language.Marlowe.CLI.Types (CliError)
import Language.Marlowe.Cardano.Thread (AnyMarloweThread, MarloweThread, anyMarloweThread)
import qualified Language.Marlowe.Core.V1.Semantics.Types as M
import Language.Marlowe.Runtime.Core.Api (ContractId)
import Ledger.Orphans ()


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

newtype RuntimeContractInfo era lang =
  RuntimeContractInfo (AnyRuntimeMarloweThread era lang)

newtype RuntimeMonitorInput = RuntimeMonitorInput (TChan (ContractNickname, ContractId))

newtype RuntimeMonitorState era lang = RuntimeMonitorState (TVar (Map ContractNickname (RuntimeContractInfo era lang)))

newtype RuntimeMonitor = RuntimeMonitor { runMonitor :: IO RuntimeError }

data RuntimeOperation =
    RuntimeAwaitCreated
    {
      roContractNickname  :: ContractNickname
    , roTimeout :: Maybe Int
    }
  | RuntimeAwaitInputsApplied
    {
      roContractNickname  :: ContractNickname
    }
  | RuntimeAwaitClosed
    {
      roContractNickname  :: ContractNickname
    }
  deriving stock (Eq, Generic, Show)

instance FromJSON RuntimeOperation where
  parseJSON = do
    A.genericParseJSON $ Operation.genericParseJSONOptions "ro"


newtype InterpretState = InterpretState
  {
   _isKnownContracts :: Map ContractNickname ContractId
  }

data InterpretEnv lang era = InterpretEnv
  {
    _ieRuntimeMonitorState :: RuntimeMonitorState lang era
  , _ieRuntimeMonitorInput :: RuntimeMonitorInput
  , _ieExecutionMode :: ExecutionMode
  }

type InterpretMonad m lang era =
  ( MonadState InterpretState m
  , MonadReader (InterpretEnv lang era) m
  , MonadError CliError m
  , MonadIO m
  )

makeLenses 'InterpretState
makeLenses 'InterpretEnv

