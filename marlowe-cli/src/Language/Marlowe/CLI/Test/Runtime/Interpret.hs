-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Interpreter for Marlowe runtime testing operations.
--
-----------------------------------------------------------------------------


{-# LANGUAGE BlockArguments #-}
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

module Language.Marlowe.CLI.Test.Runtime.Interpret
  where

import Actus.Marlowe (getContractIdentifier)
import Contrib.Control.Concurrent.Async (timeoutIO)
import Control.Concurrent.STM (atomically, readTVar, retry)
import Control.Lens (use, view)
import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Data.Map.Strict as Map
import Data.Time.Units (TimeUnit(fromMicroseconds))
import Language.Marlowe.CLI.Test.Contract (ContractNickname(ContractNickname))
import Language.Marlowe.CLI.Test.ExecutionMode (skipInSimluationMode)
import Language.Marlowe.CLI.Test.Log (logLabeledMsg, throwLabeledError, throwTraceError)
import Language.Marlowe.CLI.Test.Runtime.Types
  ( InterpretMonad
  , RuntimeMonitorState(RuntimeMonitorState)
  , RuntimeOperation(..)
  , ieExecutionMode
  , ieRuntimeMonitorState
  , isKnownContracts
  )
import Language.Marlowe.CLI.Types (CliError(CliError))
import Language.Marlowe.Runtime.Core.Api (ContractId)

getContractId
  :: forall era lang m
   . InterpretMonad m lang era
  => ContractNickname
  -> m ContractId
getContractId nickname = do
  knownContracts <- use isKnownContracts
  case Map.lookup nickname knownContracts of
    Just contractId -> pure contractId
    Nothing ->
      throwTraceError "getContractId" $ "Contract instance not found: " <> show nickname

interpret
  :: forall era lang m
   . InterpretMonad m lang era
  => RuntimeOperation
  -> m ()
interpret ro@RuntimeAwaitCreated {..} = do
  view ieExecutionMode >>= skipInSimluationMode ro do
    contractId <- getContractId roContractNickname
    RuntimeMonitorState runtimeMonitorStateRef <- view ieRuntimeMonitorState
    let
      getContractInfo = do
        runtimeMonitorState <- readTVar runtimeMonitorStateRef
        maybe retry pure (Map.lookup roContractNickname runtimeMonitorState)
      timeout = maybe (fromMicroseconds 2000_000) (fromMicroseconds . (*) 1000_000 . toInteger) roTimeout

    (liftIO $ timeoutIO timeout (atomically getContractInfo)) >>= \case
      Just _ -> do
        logLabeledMsg ro $ "Contract instance created: " <> show roContractNickname
        pure ()
      _ -> throwLabeledError ro $ "Timeout reached while waiting for contract instance creation: " <> show roContractNickname
interpret ro =
  throwLabeledError ro $ "Interpreter not implemented for: " <> show ro

-- interpret so@RuntimeAwaitInputsApplied {..} = do
--   pure ()
-- --   view seExecutionMode >>= \case
-- --     SimulationMode -> do
-- --       logLabeledMsg so "[Skip] Skipping runtime check in the simulation mode."
-- --       pure ()
-- --     OnChainMode {} -> do
-- --       stream <- awaitContractStream roContractNickname
-- --       case stream of
-- --         ContractStreamContinued {..} -> pure ()
-- --         _ -> throwLabeledError so $ "Expecting contract stream continued but got: " <> show stream
-- interpret so@RuntimeAwaitClosed {..} = do
--   pure ()
-- --   view seExecutionMode >>= \case
-- --     SimulationMode -> do
-- --       logLabeledMsg so "[Skip] Skipping runtime check in the simulation mode."
-- --       pure ()
-- --     OnChainMode {} -> do
-- --       stream <- awaitContractStream roContractNickname
-- --       case stream of
-- --         ContractStreamFinish {..} -> pure ()
-- --         _ -> throwLabeledError so $ "Expecting contract stream finished but got: " <> show stream
