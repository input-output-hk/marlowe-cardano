-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Runtime monitoring thread for the test suite.
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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}


module Language.Marlowe.CLI.Test.Runtime.Monitor
  where
--  (mkRuntimeMonitor)
-- where

import Language.Marlowe.Extended.V1 as E (ChoiceId(ChoiceId), Contract(Close), Party, Value(Constant))
import Marlowe.Contracts (escrow, swap, trivial)
import Plutus.V1.Ledger.Api (CostModelParams, TokenName)

import qualified Cardano.Api as C
import Contrib.Control.Concurrent.Async (altIO)
import qualified Contrib.Data.Aeson.Traversals as A
import Control.Category ((>>>))
import Control.Concurrent (forkFinally)
import Control.Concurrent.Async (async, cancel, concurrently, race, waitCatch)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTChanIO, newTVarIO, readTChan, writeTChan, writeTVar)
import Control.Concurrent.STM.TVar (readTVar)
import Control.Error (note)
import Control.Exception (Exception(displayException))
import Control.Lens (assign, modifying, use, view)
import Control.Lens.Setter ((%=))
import Control.Monad (forever, unless, void)
import Control.Monad.Except (MonadError(throwError))
import Control.Monad.Extra (whenM)
import Control.Monad.Loops (untilJust)
import Control.Monad.RWS.Class (MonadReader)
import Control.Monad.Reader (ReaderT(runReaderT))
import Control.Monad.STM (STM)
import Control.Monad.Trans (MonadIO(liftIO), MonadTrans(lift))
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.OneLine as A
import Data.Default (Default(def))
import qualified Data.Fixed as F
import Data.Foldable (Foldable(fold), find, foldl')
import Data.Foldable.Extra (for_)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty((:|)), (<|))
import qualified Data.List.NonEmpty as L.NonEmpty
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import qualified Data.Set as S (singleton)
import qualified Data.Text as Text
import Data.Traversable (for)
import Data.Tuple.Extra (uncurry3)
import Language.Marlowe.CLI.Cardano.Api.PlutusScript (IsPlutusScriptLanguage)
import Language.Marlowe.CLI.Cardano.Api.Value (txOutValueValue)
import qualified Language.Marlowe.CLI.Cardano.Api.Value as CV
import Language.Marlowe.CLI.IO (liftCliMaybe, queryInEra)
import Language.Marlowe.CLI.Run
  ( autoRunTransactionImpl
  , autoWithdrawFundsImpl
  , initializeTransactionImpl
  , initializeTransactionUsingScriptRefsImpl
  , marloweAddressFromCardanoAddress
  , marloweAddressToCardanoAddress
  , prepareTransactionImpl
  )
import Language.Marlowe.CLI.Sync (classifyOutputs, isMarloweOut)
import Language.Marlowe.CLI.Sync.Types (MarloweOut(ApplicationOut, moTxIn))
import Language.Marlowe.CLI.Test.Contract (ContractNickname)
import Language.Marlowe.CLI.Test.Log (logLabeledMsg, logTraceMsg, throwLabeledError, throwTraceError)
import Language.Marlowe.CLI.Test.Runtime.Types
  ( AnyRuntimeMarloweThread
  , RuntimeContractInfo(RuntimeContractInfo)
  , RuntimeError(..)
  , RuntimeMonitor(RuntimeMonitor)
  , RuntimeMonitorInput(RuntimeMonitorInput)
  , RuntimeMonitorState(RuntimeMonitorState)
  , anyRuntimeMarloweThread
  )
import Language.Marlowe.CLI.Transaction
  (buildFaucetImpl, buildMintingImpl, findMarloweScriptsRefs, publishImpl, selectUtxosImpl)
import qualified Language.Marlowe.CLI.Types as T
import Language.Marlowe.Cardano.Thread (anyMarloweThreadCreated, isRunning)
import qualified Language.Marlowe.Client as Client
import qualified Language.Marlowe.Core.V1.Semantics as M
import qualified Language.Marlowe.Core.V1.Semantics.Types as M
import Language.Marlowe.Pretty (pretty)
import Language.Marlowe.Runtime.App.Channel (mkDetection)
import qualified Language.Marlowe.Runtime.App.Run as Apps
import qualified Language.Marlowe.Runtime.App.Run as Apps.Run
import Language.Marlowe.Runtime.App.Stream (ContractStream(..), ContractStreamError(..), streamAllContractSteps)
import Language.Marlowe.Runtime.App.Types (runClient)
import qualified Language.Marlowe.Runtime.App.Types as Apps
import qualified Language.Marlowe.Runtime.Cardano.Api as Runtime.Cardano.Api
import Language.Marlowe.Runtime.Core.Api (ContractId(ContractId))
import qualified Language.Marlowe.Runtime.Core.Api as R
import qualified Language.Marlowe.Runtime.Core.Api as Runtime.Core.Api
import qualified Language.Marlowe.Runtime.History.Api as RH
import Ledger.Tx.CardanoAPI (fromCardanoPolicyId)
import Observe.Event.Render.JSON (defaultRenderSelectorJSON)
import Observe.Event.Render.JSON.Handle (simpleJsonStderrBackend)
import Plutus.ApiCommon (ProtocolVersion)
import Plutus.V1.Ledger.SlotConfig (SlotConfig(..))
import Plutus.V1.Ledger.Value (mpsSymbol, valueOf)
import qualified Plutus.V1.Ledger.Value as P
import qualified Plutus.V1.Ledger.Value as Value
import PlutusPrelude (foldMapM)
import PlutusTx.Prelude (inv)
import qualified PlutusTx.Prelude as PTx
import System.IO.Temp (emptySystemTempFile, emptyTempFile)

mkRuntimeMonitor config = do
  -- Runtime intput channel and detection input channel differ:
  --  * test runner gonna put contract id only once into the runtime input channel
  --  * runtime gonna refeed the contract id to the detection thread as long as it is not found on the chain
  --  * runtime thread gonna expose finally the contract stream through the state
  runtimeContractInputChannel <- newTChanIO
  detectionInputChannel <- newTChanIO
  knownContractsRef <- newTVarIO Map.empty
  contractsRef <- newTVarIO Map.empty

  eventBackend <- liftIO $ simpleJsonStderrBackend defaultRenderSelectorJSON

  let
    pollingFrequency = 1000

  (contractStream, detection) <- mkDetection (const True) eventBackend config pollingFrequency detectionInputChannel

  let
    runtime = untilJust $ atomically do
      contractStreamEvent <- readTChan contractStream
      contracts <- readTVar contractsRef
      knownContracts <- readTVar knownContractsRef

      case processMarloweStreamEvent knownContracts contracts contractStreamEvent of
        Left (RuntimeContractNotFound contractId) -> do
          writeTChan detectionInputChannel contractId
          pure Nothing
        Left err -> pure $ Just err
        Right (Just (RuntimeContractUpdate contractId contractInfo)) -> do
          for_ (Map.lookup contractId knownContracts) \contractNickname ->
            modifyTVar' contractsRef (Map.insert contractNickname contractInfo)
          pure Nothing
        Right Nothing -> pure Nothing

    input = forever $ atomically do
      (contractNickname, contractId) <- readTChan runtimeContractInputChannel
      writeTChan detectionInputChannel contractId
      knownContracts <- readTVar knownContractsRef
      writeTVar knownContractsRef (Map.insert contractId contractNickname knownContracts)

  detection' <- do
    asyncDetection <- async detection
    pure do
      waitCatch asyncDetection >>= \case
        Left err -> pure $ RuntimeExecutionFailure $ show err
        Right _ -> pure $ RuntimeExecutionFailure "Detection thread finished unexpectedly"

  pure
    ( RuntimeMonitorInput runtimeContractInputChannel
    , RuntimeMonitorState contractsRef
    , RuntimeMonitor (runtime `altIO` detection' `altIO` input)
    )

data RuntimeContractUpdate era lang
  = RuntimeContractUpdate
      { rcuContractId :: ContractId
      , rcuContractInfo :: RuntimeContractInfo era lang
      }

processMarloweStreamEvent
  :: forall era lang v
   . M.Map ContractId ContractNickname
  -> M.Map ContractNickname (RuntimeContractInfo era lang)
  -> ContractStream v
  -> Either RuntimeError (Maybe (RuntimeContractUpdate era lang))
processMarloweStreamEvent knownContracts contracts = do
  let
    scriptOutputToCardanoTxIn R.TransactionScriptOutput{ R.utxo=utxo } =
      case Runtime.Cardano.Api.toCardanoTxIn utxo of
        Just txIn -> pure txIn
        Nothing -> throwError $ RuntimeExecutionFailure "Failed to convert runtime utxo to Cardano TxIn"

    lookupContractNickname :: ContractId -> Maybe ContractNickname
    lookupContractNickname contractId = M.lookup contractId knownContracts

    lookupContractInfo :: ContractId -> Maybe (RuntimeContractInfo era lang)
    lookupContractInfo contractId = do
      contractNickname <- lookupContractNickname contractId
      M.lookup contractNickname contracts

    getContractInfo :: ContractId -> Either RuntimeError (RuntimeContractInfo era lang)
    getContractInfo contractId = note (RuntimeContractNotFound contractId) $ lookupContractInfo contractId

    returnContractUpdate contractId contractInfo = do
      pure $ Just $ RuntimeContractUpdate contractId contractInfo

  \case
    ContractStreamStart{csContractId, csCreateStep=RH.CreateStep{ RH.createOutput=scriptOutput}} -> do
      txIn <- scriptOutputToCardanoTxIn scriptOutput
      case lookupContractInfo csContractId of
        Nothing -> do
          let th = anyMarloweThreadCreated () txIn
          returnContractUpdate csContractId (RuntimeContractInfo th)
        Just _ -> throwError $ RuntimeExecutionFailure $ "Contract id already running: " <> show csContractId
    ContractStreamContinued{csContractId, csContractStep=RH.RedeemPayout{}} -> pure Nothing
    ContractStreamContinued{csContractId, csContractStep=RH.ApplyTransaction R.Transaction {R.output=R.TransactionOutput{ scriptOutput=scriptOutput}}} -> do
      mTxIn <- for scriptOutput scriptOutputToCardanoTxIn
      RuntimeContractInfo th <- getContractInfo csContractId
      case anyRuntimeMarloweThread mTxIn [] th of
        Just th' -> returnContractUpdate csContractId (RuntimeContractInfo th')
        Nothing -> do
          throwError $ RuntimeExecutionFailure $ "Thread contination failed: " <> show csContractId
    ContractStreamRolledBack{csContractId} -> do
      -- Is it even possible that we have not yet started the thread?
      case lookupContractNickname csContractId of
        Just contractNickname -> throwError $ RuntimeRollbackError contractNickname
        Nothing -> pure Nothing
    ContractStreamWait {} -> pure Nothing
    ContractStreamFinish{csFinish=Nothing, csContractId} -> do
      RuntimeContractInfo th <- getContractInfo csContractId
      unless (isRunning th) do
        throwError $ RuntimeExecutionFailure $ "Thread already closed: " <> show csContractId
      pure Nothing
    ContractStreamFinish{csFinish=Just creationError, csContractId} -> do
      case creationError of
        ContractNotFound -> do
          throwError $ RuntimeContractNotFound csContractId
        err -> do
          throwError $ RuntimeExecutionFailure $ "Contract error: " <> show err

