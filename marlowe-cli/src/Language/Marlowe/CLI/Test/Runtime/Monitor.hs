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
{-# LANGUAGE TypeApplications #-}


module Language.Marlowe.CLI.Test.Runtime.Monitor
  where

import Contrib.Control.Concurrent.Async (altIO)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (async, waitCatch)
import Control.Concurrent.STM (atomically, modifyTVar', newTChanIO, newTVarIO, readTChan, writeTChan, writeTVar)
import Control.Concurrent.STM.TVar (readTVar)
import Control.Error (note)
import Control.Monad (forever, join, void)
import Control.Monad.Except (MonadError(throwError))
import Control.Monad.Loops (untilJust)
import Data.Foldable.Extra (for_)
import qualified Data.Map.Strict as M
import qualified Data.Map.Strict as Map
import Data.Time.Units (Second, TimeUnit(fromMicroseconds, toMicroseconds))
import Data.Traversable (for)
import Language.Marlowe.CLI.Test.Contract (ContractNickname)
import Language.Marlowe.CLI.Test.Log (logTraceMsg)
import Language.Marlowe.CLI.Test.Runtime.Types
  ( RuntimeContractInfo(RuntimeContractInfo)
  , RuntimeError(..)
  , RuntimeMonitor(RuntimeMonitor)
  , RuntimeMonitorInput(RuntimeMonitorInput)
  , RuntimeMonitorState(RuntimeMonitorState)
  , anyRuntimeMarloweThread
  )
import Language.Marlowe.Cardano.Thread (anyMarloweThreadCreated)
import Language.Marlowe.Runtime.App.Channel (mkDetection)
import Language.Marlowe.Runtime.App.Stream (ContractStream(..), ContractStreamError(..), EOF)
import Language.Marlowe.Runtime.App.Types (Config, PollingFrequency(PollingFrequency))
import qualified Language.Marlowe.Runtime.Cardano.Api as Runtime.Cardano.Api
import Language.Marlowe.Runtime.Core.Api (ContractId, MarloweVersionTag(V1))
import qualified Language.Marlowe.Runtime.Core.Api as R
import qualified Language.Marlowe.Runtime.History.Api as RH
import Observe.Event.Backend (unitEventBackend)

mkRuntimeMonitor ::Config -> IO (RuntimeMonitorInput, RuntimeMonitorState lang era, RuntimeMonitor)
mkRuntimeMonitor config = do
  -- Runtime intput channel and detection input channel differ:
  --  * test runner gonna put contract id only once into the runtime input channel
  --  * runtime gonna refeed the contract id to the detection thread as long as it is not found on the chain
  --  * runtime thread gonna expose finally the contract stream through the state
  runtimeContractInputChannel <- newTChanIO
  detectionInputChannel <- newTChanIO
  knownContractsRef <- newTVarIO Map.empty
  contractsRef <- newTVarIO Map.empty
  let
    -- FIXME: Unify all the logging.
    eventBackend = unitEventBackend

    pollingMicroseconds = fromMicroseconds $ toMicroseconds (5 :: Second)
    pollingFrequency = PollingFrequency pollingMicroseconds

  (contractStream, detection) <- mkDetection (const True) eventBackend config pollingFrequency detectionInputChannel

  let
    runtime = untilJust $ do
      join $ atomically do
        contractStreamEvent <- readTChan contractStream
        contracts <- readTVar contractsRef
        knownContracts <- readTVar knownContractsRef

        case processMarloweStreamEvent knownContracts contracts contractStreamEvent of
          Left (RuntimeContractNotFound contractId) -> pure $ do
            void . forkIO $ do
              threadDelay . fromIntegral . toMicroseconds $ (10 :: Second)
              atomically $ writeTChan detectionInputChannel (Right contractId)
            pure Nothing
          Left err -> pure $ pure $ Just err
          Right (Just (Revisit contractId)) -> pure $ do
              void . forkIO
                $ threadDelay (fromIntegral pollingMicroseconds)
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
        logTraceMsg "RuntimeMonitor" $ "Contract " <> show c <> " added to the runtime monitor."

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
  | Revisit ContractId

processMarloweStreamEvent
  :: forall era lang
   . M.Map ContractId ContractNickname
  -> M.Map ContractNickname (RuntimeContractInfo era lang)
  -> Either EOF (ContractStream 'V1)
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
    Left _ -> throwError $ RuntimeExecutionFailure "Detection thread finished unexpectedly.."
    Right ev -> case ev of
      ContractStreamStart{csContractId, csCreateStep=RH.CreateStep{ RH.createOutput=scriptOutput}} -> do
        txIn <- scriptOutputToCardanoTxIn scriptOutput
        let th = anyMarloweThreadCreated () txIn
        returnContractUpdate csContractId (RuntimeContractInfo th)
      ContractStreamContinued{csContractStep=RH.RedeemPayout{}} -> pure Nothing
      ContractStreamContinued{csContractId, csContractStep=RH.ApplyTransaction R.Transaction {R.inputs=inputs, R.output=R.TransactionOutput{ scriptOutput=scriptOutput}}} -> do
        mTxIn <- for scriptOutput scriptOutputToCardanoTxIn
        RuntimeContractInfo th <- getContractInfo csContractId
        case anyRuntimeMarloweThread mTxIn inputs th of
          Just th' -> returnContractUpdate csContractId (RuntimeContractInfo th')
          Nothing -> do
            throwError $ RuntimeExecutionFailure $ "Thread continuation failed: " <> show csContractId
      ContractStreamRolledBack{csContractId} -> do
        -- Is it even possible that we have not yet started the thread?
        case lookupContractNickname csContractId of
          Just contractNickname -> throwError $ RuntimeRollbackError contractNickname
          Nothing -> pure Nothing
      ContractStreamWait {csContractId} -> pure $ Just (Revisit csContractId)
      ContractStreamFinish{csFinish=Nothing} -> do
        -- We are ignoring this event because we are closing the thread
        -- from the `ContractStreamContinued` handler.
        pure Nothing
      ContractStreamFinish{csFinish=Just creationError, csContractId} -> do
        case creationError of
          ContractNotFound -> do
            throwError $ RuntimeContractNotFound csContractId
          err -> do
            throwError $ RuntimeExecutionFailure $ "Contract error: " <> show err

