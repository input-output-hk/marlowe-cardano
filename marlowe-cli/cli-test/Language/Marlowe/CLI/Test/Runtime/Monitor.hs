{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Marlowe.CLI.Test.Runtime.Monitor where

import Cardano.Api qualified as C
import Contrib.Control.Concurrent (threadDelay)
import Contrib.Control.Concurrent.Async (altIO)
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (async, waitCatch)
import Control.Concurrent.STM (atomically, modifyTVar', newTChanIO, newTVarIO, readTChan, writeTChan, writeTVar)
import Control.Concurrent.STM.TVar (readTVar)
import Control.Error (note)
import Control.Monad (forever, join, void)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Loops (untilJust)
import Data.Aeson (toJSON)
import Data.Aeson.Text qualified as A
import Data.Foldable.Extra (for_)
import Data.Functor ((<&>))
import Data.Map.Strict qualified as M
import Data.Map.Strict qualified as Map
import Data.Text.Lazy qualified as TL
import Data.Time.Units (Second)
import Data.Traversable (for)
import Language.Marlowe.CLI.Test.Contract (ContractNickname)
import Language.Marlowe.CLI.Test.Log (logLabeledMsg)
import Language.Marlowe.CLI.Test.Runtime.Types (
  RuntimeContractInfo (RuntimeContractInfo),
  RuntimeError (..),
  RuntimeMonitor (RuntimeMonitor),
  RuntimeMonitorInput (RuntimeMonitorInput),
  RuntimeMonitorState (RuntimeMonitorState),
  anyRuntimeMonitorMarloweThreadInputsApplied,
 )
import Language.Marlowe.Cardano.Thread (
  anyMarloweThreadCreated,
  anyMarloweThreadRedeemed,
  marloweThreadToJSON,
  overAnyMarloweThread,
 )
import Language.Marlowe.Runtime.App.Channel (mkDetection)
import Language.Marlowe.Runtime.App.Stream (ContractStream (..), ContractStreamError (..), EOF)
import Language.Marlowe.Runtime.App.Types (
  Config,
  FinishOnClose (FinishOnClose),
  FinishOnWait (FinishOnWait),
  PollingFrequency (PollingFrequency),
 )
import Language.Marlowe.Runtime.Cardano.Api qualified as RCA
import Language.Marlowe.Runtime.ChainSync.Api qualified as MCS
import Language.Marlowe.Runtime.Core.Api (ContractId, MarloweVersionTag (V1))
import Language.Marlowe.Runtime.Core.Api qualified as R
import Language.Marlowe.Runtime.History.Api qualified as RH
import Language.Marlowe.Runtime.Plutus.V2.Api qualified as RP
import Observe.Event.Backend (unitEventBackend)

mkRuntimeMonitor :: Config -> IO (RuntimeMonitorInput, RuntimeMonitorState, RuntimeMonitor)
mkRuntimeMonitor config = do
  -- Runtime input channel and detection input channel differ:
  --  * test runner gonna put contract id only once into the runtime input channel
  --  * runtime gonna re-feed the contract id to the detection thread as long as it is not found on the chain
  --  * runtime thread gonna expose finally the contract stream through the state
  runtimeContractInputChannel <- newTChanIO
  detectionInputChannel <- newTChanIO
  knownContractsRef <- newTVarIO Map.empty
  contractsRef <- newTVarIO Map.empty
  let -- FIXME: Unify all the logging.
      eventBackend = unitEventBackend

      pollingFrequencySeconds = 5 :: Second
      pollingFrequency = PollingFrequency pollingFrequencySeconds

      dontFinishOnClose = FinishOnClose False
      dontFinishOnWait = FinishOnWait False

  (contractStream, detection) <-
    mkDetection
      (const True)
      eventBackend
      config
      pollingFrequency
      dontFinishOnClose
      dontFinishOnWait
      detectionInputChannel
  let runtime = untilJust $ do
        join $ atomically do
          contractStreamEvent <- readTChan contractStream
          contracts <- readTVar contractsRef
          knownContracts <- readTVar knownContractsRef

          case processMarloweStreamEvent knownContracts contracts contractStreamEvent of
            Left (RuntimeContractNotFound contractId) -> pure $ do
              void . forkIO $ do
                threadDelay (10 :: Second)
                atomically $ writeTChan detectionInputChannel (Right contractId)
              pure Nothing
            Left err -> pure $ pure $ Just err
            Right (Just (Revisit contractId)) -> pure $ do
              void . forkIO $
                threadDelay pollingFrequencySeconds
                  >> atomically (writeTChan detectionInputChannel $ Right contractId)
              pure Nothing
            Right (Just (RuntimeContractUpdate contractId contractInfo)) -> do
              for_ (Map.lookup contractId knownContracts) \contractNickname ->
                modifyTVar' contractsRef (Map.insert contractNickname contractInfo)
              pure $ pure Nothing
            Right Nothing -> pure $ pure Nothing

      input = forever $ do
        possibleAddition <- atomically do
          c@(contractNickname, contractId) <- readTChan runtimeContractInputChannel
          knownContracts <- readTVar knownContractsRef
          case Map.lookup contractId knownContracts of
            Just _ -> pure Nothing
            Nothing -> do
              writeTChan detectionInputChannel (Right contractId)
              writeTVar knownContractsRef (Map.insert contractId contractNickname knownContracts)
              pure $ Just c
        for possibleAddition \c -> do
          logLabeledMsg ("RuntimeMonitor" :: String) $ "Contract " <> show c <> " added to the runtime monitor."

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

data RuntimeContractUpdate
  = RuntimeContractUpdate
      { rcuContractId :: ContractId
      , rcuContractInfo :: RuntimeContractInfo
      }
  | Revisit ContractId

processMarloweStreamEvent
  :: forall
   . M.Map ContractId ContractNickname
  -> M.Map ContractNickname RuntimeContractInfo
  -> Either EOF (ContractStream 'V1)
  -> Either RuntimeError (Maybe RuntimeContractUpdate)
processMarloweStreamEvent knownContracts contractsInfo = do
  let scriptOutputToCardanoTxIn R.TransactionScriptOutput{R.utxo = utxo} =
        case RCA.toCardanoTxIn utxo of
          Just txIn -> pure txIn
          Nothing -> throwError $ RuntimeExecutionFailure "Failed to convert runtime utxo to Cardano TxIn"

      lookupContractNickname :: ContractId -> Maybe ContractNickname
      lookupContractNickname contractId = M.lookup contractId knownContracts

      lookupContractInfo :: ContractId -> Maybe RuntimeContractInfo
      lookupContractInfo contractId = do
        contractNickname <- lookupContractNickname contractId
        M.lookup contractNickname contractsInfo

      getContractInfo :: ContractId -> Either RuntimeError RuntimeContractInfo
      getContractInfo contractId = note (RuntimeContractNotFound contractId) $ lookupContractInfo contractId

      returnContractUpdate contractId contractInfo = do
        pure $ Just $ RuntimeContractUpdate contractId contractInfo

  \case
    Left _ -> throwError $ RuntimeExecutionFailure "Detection thread finished unexpectedly.."
    Right ev -> case ev of
      ContractStreamStart{csContractId, csCreateStep = RH.CreateStep{RH.createOutput = scriptOutput}} -> do
        txIn@(C.TxIn txId _) <- scriptOutputToCardanoTxIn scriptOutput
        let th = anyMarloweThreadCreated txId txIn
        returnContractUpdate csContractId (RuntimeContractInfo th)
      ContractStreamContinued
        { csContractId
        , csContractStep = RH.RedeemPayout RH.RedeemStep{RH.redeemingTx = txId, datum = MCS.AssetId{tokenName}}
        } -> do
          RuntimeContractInfo th <- getContractInfo csContractId
          let tokenName' = RP.toPlutusTokenName tokenName
          case RCA.toCardanoTxId txId of
            Just txId' -> do
              let th' = anyMarloweThreadRedeemed txId' tokenName' th
              returnContractUpdate csContractId (RuntimeContractInfo th')
            Nothing -> throwError $ RuntimeExecutionFailure "Failed to convert runtime txId to Cardano TxId"
      ContractStreamContinued
        { csContractId
        , csContractStep =
          RH.ApplyTransaction
            R.Transaction{R.transactionId = txId, R.inputs = inputs, R.output = R.TransactionOutput{scriptOutput = scriptOutput}}
        } -> do
          possibleTxIn <- for scriptOutput scriptOutputToCardanoTxIn
          RuntimeContractInfo th <- getContractInfo csContractId
          let possibleTxIx = possibleTxIn <&> \(C.TxIn _ txIx) -> txIx
          case RCA.toCardanoTxId txId >>= \txId' -> anyRuntimeMonitorMarloweThreadInputsApplied txId' possibleTxIx inputs th of
            Just th' -> returnContractUpdate csContractId (RuntimeContractInfo th')
            Nothing -> do
              let threadJson = overAnyMarloweThread marloweThreadToJSON th
                  inputsJson = toJSON inputs
                  jsonStr = TL.unpack . A.encodeToLazyText
                  threadStr = jsonStr threadJson
                  inputsStr = jsonStr inputsJson

              throwError $
                RuntimeExecutionFailure $
                  "Thread continuation failed: " <> show csContractId <> ", " <> threadStr <> ", " <> inputsStr
      ContractStreamRolledBack{csContractId} -> do
        -- Is it even possible that we have not yet started the thread?
        for_ (lookupContractNickname csContractId) \contractNickname ->
          throwError $ RuntimeRollbackError contractNickname
        case lookupContractNickname csContractId of
          Just contractNickname -> throwError $ RuntimeRollbackError contractNickname
          Nothing -> pure Nothing
      ContractStreamWait{csContractId} -> pure $ Just (Revisit csContractId)
      ContractStreamFinish{csFinish = Nothing} -> pure Nothing
      ContractStreamFinish{csFinish = Just creationError, csContractId} -> do
        case creationError of
          ContractNotFound -> do
            throwError $ RuntimeContractNotFound csContractId
          err -> do
            throwError $ RuntimeExecutionFailure $ "Contract error: " <> show err
