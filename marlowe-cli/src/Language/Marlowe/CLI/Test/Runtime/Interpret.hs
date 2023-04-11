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
import Cardano.Api (CardanoMode, Tx, toAddressAny)
import qualified Cardano.Api as C
import Contrib.Control.Concurrent.Async (timeoutIO)
import Control.Concurrent.STM (TVar, atomically, readTVar, retry, writeTChan)
import Control.Lens (modifying, preview, use, view)
import Control.Monad (join, when)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.STM (STM)
import Control.Monad.Trans.Marlowe (runMarloweT)
import qualified Control.Monad.Trans.Marlowe.Class as Marlowe.Class
import qualified Data.Aeson as A
import qualified Data.Aeson.OneLine as A
import Data.Coerce (coerce)
import Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import qualified Data.Text as Text
import Data.Time.Units (Microsecond, Second, TimeUnit(fromMicroseconds, toMicroseconds))
import Data.Traversable (for)
import Language.Marlowe.CLI.Cardano.Api (withShelleyBasedEra)
import Language.Marlowe.CLI.IO (liftCliMaybe)
import Language.Marlowe.CLI.Test.Contract (ContractNickname(ContractNickname), Source(InlineContract, UseTemplate))
import Language.Marlowe.CLI.Test.Contract.ParametrizedMarloweJSON
  (ParametrizedMarloweJSON(ParametrizedMarloweJSON), decodeParametrizedContractJSON, decodeParametrizedInputJSON)
import Language.Marlowe.CLI.Test.Contract.Source (useTemplate)
import Language.Marlowe.CLI.Test.ExecutionMode (skipInSimluationMode)
import Language.Marlowe.CLI.Test.Log (logLabeledMsg, throwLabeledError, throwTraceError)
import Language.Marlowe.CLI.Test.Runtime.Types
  ( InterpretMonad
  , RuntimeContractInfo(RuntimeContractInfo)
  , RuntimeMonitorInput(RuntimeMonitorInput)
  , RuntimeMonitorState(RuntimeMonitorState)
  , RuntimeOperation(..)
  , connectionT
  , currenciesL
  , eraL
  , executionModeL
  , knownContractsL
  , rcMarloweThread
  , runtimeClientConnectorT
  , runtimeMonitorInputT
  , runtimeMonitorStateT
  , walletsL
  )
import Language.Marlowe.CLI.Test.Wallet.Interpret
  (decodeContractJSON, decodeInputJSON, findWallet, getFaucet, updateWallet)
import Language.Marlowe.CLI.Test.Wallet.Types
  (SomeTxBody(BabbageTxBody), Wallet(..), WalletNickname(WalletNickname), Wallets(Wallets), faucetNickname)
import Language.Marlowe.CLI.Types (CliError(CliError), somePaymentsigningKeyToTxWitness)
import Language.Marlowe.Cardano (marloweNetworkFromLocalNodeConnectInfo)
import Language.Marlowe.Cardano.Thread (overAnyMarloweThread)
import qualified Language.Marlowe.Cardano.Thread as Marlowe.Cardano.Thread
import qualified Language.Marlowe.Core.V1.Semantics.Types as M
import qualified Language.Marlowe.Protocol.Client as Marlowe.Protocol
import qualified Language.Marlowe.Runtime.ChainSync.Api as ChainSync
import Language.Marlowe.Runtime.Core.Api
  (ContractId, MarloweVersion(MarloweV1), MarloweVersionTag(V1), emptyMarloweTransactionMetadata)
import Language.Marlowe.Runtime.Transaction.Api
  (RoleTokensConfig(RoleTokensNone), WalletAddresses(WalletAddresses, changeAddress, collateralUtxos, extraAddresses))
import qualified Language.Marlowe.Runtime.Transaction.Api as Transaction
import qualified Network.Protocol.Connection as ChainSync
import qualified Network.Protocol.Connection as Network.Protocol

-- connectionP :: Prism' env (LocalNodeConnectInfo CardanoMode)
getConnection
  :: forall env st m lang era
   . InterpretMonad env st m lang era
  => m (C.LocalNodeConnectInfo CardanoMode)
getConnection = do
  preview connectionT >>= \case
    Just conn -> pure conn
    Nothing -> throwTraceError "getConnection" "Connection not found"

-- runtimeClientConnectorP :: Prism' env (Network.Protocol.SomeClientConnector Marlowe.Protocol.MarloweRuntimeClient IO)
getConnector
  :: forall env st m lang era
   . InterpretMonad env st m lang era
  => m (Network.Protocol.SomeClientConnector Marlowe.Protocol.MarloweRuntimeClient IO)
getConnector = do
  preview runtimeClientConnectorT >>= \case
    Just conn -> pure conn
    Nothing -> throwTraceError "getConnector" "Connector not found"

-- runtimeMonitorStateP :: Prism' env (RuntimeMonitorState lang era)
getMonitorState
  :: forall env st m lang era
   . InterpretMonad env st m lang era
  => m (RuntimeMonitorState lang era)
getMonitorState = do
  preview runtimeMonitorStateT >>= \case
    Just state -> pure state
    Nothing -> throwTraceError "getMonitorState" "Monitor state not found"

--runtimeMonitorInputP :: Prism' env RuntimeMonitorInput
getMonitorInput
  :: forall env st m lang era
   . InterpretMonad env st m lang era
  => m RuntimeMonitorInput
getMonitorInput = do
  preview runtimeMonitorInputT >>= \case
    Just input -> pure input
    Nothing -> throwTraceError "getMonitorInput" "Monitor input not found"

getContractId
  :: forall era env lang m st
   . InterpretMonad env st m lang era
  => ContractNickname
  -> m ContractId
getContractId nickname = do
  knownContracts <- use knownContractsL
  case Map.lookup nickname knownContracts of
    Just contractId -> pure contractId
    Nothing ->
      throwTraceError "getContractId" $ "Contract instance not found: " <> show nickname

-- It is probably feasible to achieve this conversion without `Maybe`.
toChainSyncAddress :: C.AddressInEra era -> Maybe ChainSync.Address
toChainSyncAddress (C.AddressInEra C.ByronAddressInAnyEra _) = do
  Nothing
toChainSyncAddress (C.AddressInEra (C.ShelleyAddressInEra _) address') = do
  pure $ ChainSync.fromCardanoShelleyAddress address'


startMonitoring contractNickname = do
  contractId <- getContractId contractNickname
  RuntimeMonitorInput runtimeMonitorInput <- getMonitorInput
  liftIO $ atomically $ do
    writeTChan runtimeMonitorInput (contractNickname, contractId)

awaitContractInfo :: RuntimeMonitorState lang era -> ContractNickname -> (Maybe (RuntimeContractInfo lang era) -> Bool) -> STM (Maybe (RuntimeContractInfo lang era))
awaitContractInfo (RuntimeMonitorState rmsRef) contractNickname check = do
  runtimeMonitorState <- readTVar rmsRef
  let
    info = Map.lookup contractNickname runtimeMonitorState
  if check info
  then pure info
  else retry

awaitNonEmptyContractInfo :: RuntimeMonitorState lang era -> ContractNickname -> (RuntimeContractInfo lang era -> Bool) -> STM (RuntimeContractInfo lang era)
awaitNonEmptyContractInfo (RuntimeMonitorState rmsRef) contractNickname check = do
  runtimeMonitorState <- readTVar rmsRef
  case Map.lookup contractNickname runtimeMonitorState of
    Nothing -> retry
    Just info -> if check info
      then pure info
      else retry

anyMarloweThreadToJSON = overAnyMarloweThread Marlowe.Cardano.Thread.marloweThreadToJSON

operationTimeout :: Second -> RuntimeOperation -> Microsecond
operationTimeout seconds ro = do
  let
    seconds' = maybe seconds (fromInteger :: Integer -> Second) . roTimeout $ ro
  fromMicroseconds . (toMicroseconds :: Second -> Integer) $ seconds'

operationTimeoutLogged
  :: InterpretMonad env st m lang era
  => Second -> RuntimeOperation ->  m Microsecond
operationTimeoutLogged seconds ro = do
  let
    seconds' = maybe seconds (fromInteger :: Integer -> Second) . roTimeout $ ro
  logLabeledMsg ro $ "Timeout: " <> show seconds'
  pure $ fromMicroseconds . (toMicroseconds :: Second -> Integer) $ seconds'

interpret
  :: forall era env lang m st
   . InterpretMonad env st m lang era
  => C.IsShelleyBasedEra era
  => RuntimeOperation
  -> m ()
interpret ro@RuntimeAwaitCreated {..} = do
  view executionModeL >>= skipInSimluationMode ro do
    startMonitoring roContractNickname
    rms <- getMonitorState
    let
      getContractInfo = awaitNonEmptyContractInfo rms roContractNickname (const True)
    logLabeledMsg ro $ "Waiting for contract instance: " <> show (coerce roContractNickname :: String)
    timeout <- operationTimeoutLogged 60 ro
    (liftIO $ timeoutIO timeout (atomically getContractInfo)) >>= \case
      Just (RuntimeContractInfo thread) -> do
        logLabeledMsg ro $ "Contract instance created: " <> show roContractNickname
      _ -> throwLabeledError ro $ "Timeout reached while waiting for contract instance creation: " <> show roContractNickname

interpret ro@RuntimeAwaitInputsApplied {..} = do
  view executionModeL >>= skipInSimluationMode ro do
    startMonitoring roContractNickname
    rms <- getMonitorState
    timeout <- operationTimeoutLogged 60 ro
    let
      joinInputs thread = do
        case overAnyMarloweThread Marlowe.Cardano.Thread.marloweThreadInputs thread of
          [] -> Nothing
          inputs -> pure $ join inputs
      inputsApplied expected thread = do
        let
          actual = joinInputs thread
        case actual of
          Nothing -> False
          Just actual' -> actual' == expected
    expectedInputs <- for roAllInputs decodeInputJSON
    logLabeledMsg ro "Waiting till inputs are applied..."
    res <- liftIO $ timeoutIO timeout $ atomically (awaitNonEmptyContractInfo rms roContractNickname $ inputsApplied expectedInputs . view rcMarloweThread)
    when (isNothing res) do
      let
        getContractInfo = awaitContractInfo rms roContractNickname (const True)
      thread <- liftIO $ atomically getContractInfo
      let
        contractState = maybe "<empty>" (Text.unpack . A.renderValue . anyMarloweThreadToJSON . view rcMarloweThread) thread
      throwLabeledError ro $ "Timeout reached while waiting for contract instance to be closed: " <> show roContractNickname <> ". Contract info: " <> contractState


interpret ro@RuntimeAwaitClosed {..} = do
  view executionModeL >>= skipInSimluationMode ro do
    startMonitoring roContractNickname
    rms <- getMonitorState
    logLabeledMsg ro $ "Waiting till contract instance is closed: " <> show (coerce roContractNickname :: String)
    timeout <- operationTimeoutLogged 60 ro
    let
      closedL :: RuntimeContractInfo lang era -> Bool
      closedL = not . Marlowe.Cardano.Thread.isRunning . view rcMarloweThread

    res <- liftIO $ timeoutIO timeout $ atomically (awaitNonEmptyContractInfo rms roContractNickname closedL)
    when (isNothing res) do
      let
        getContractInfo = awaitContractInfo rms roContractNickname (const True)
      thread <- liftIO $ atomically getContractInfo
      let
        contractState = maybe "<empty>" (Text.unpack . A.renderValue . anyMarloweThreadToJSON . view rcMarloweThread) thread
      throwLabeledError ro $ "Timeout reached while waiting for contract instance to be closed: " <> show roContractNickname <> ". Contract info: " <> contractState

interpret ro@RuntimeCreateContract {..} = do
  view executionModeL >>= skipInSimluationMode ro do
    Wallet { waAddress, waSigningKey } <- maybe getFaucet findWallet roSubmitter
    connector <- getConnector
    contract <- case roContractSource of
      InlineContract json -> decodeContractJSON json
      UseTemplate setup   -> useTemplate roRoleCurrency setup
    let
      possibleChangeAddress = toChainSyncAddress waAddress
    changeAddress <- liftCliMaybe "Failed to create change address" possibleChangeAddress

    logLabeledMsg ro $ "Invoking contract creation: " <> show roContractNickname
    result <- liftIO $ flip runMarloweT connector do
      let
        stakeCredential = Nothing
        roleTokensConfig = RoleTokensNone
        minLovelace = ChainSync.Lovelace roMinLovelace
        walletAddresses = WalletAddresses
          { changeAddress = changeAddress
          , extraAddresses = mempty
          , collateralUtxos = mempty
          }
      Marlowe.Class.createContract Nothing MarloweV1 walletAddresses roleTokensConfig emptyMarloweTransactionMetadata minLovelace contract
    era <- view eraL
    case result of
      Right Transaction.ContractCreated { txBody, contractId } -> do
        logLabeledMsg ro $ "Creating contract: " <> show contractId
        let
          witness = somePaymentsigningKeyToTxWitness waSigningKey
          tx = withShelleyBasedEra era . C.signShelleyTransaction txBody $ [witness]
          submitterNickname = fromMaybe faucetNickname roSubmitter
        res <- liftIO $ flip runMarloweT connector do
          Marlowe.Class.submitAndWait tx

        case res of
          Right _ -> do
            logLabeledMsg ro $ "Contract created: " <> show tx
            modifying knownContractsL $ Map.insert roContractNickname contractId
            updateWallet submitterNickname \submitter@Wallet {..} -> do
              submitter { waSubmittedTransactions = BabbageTxBody txBody : waSubmittedTransactions }
            pure ()
          Left err ->
            throwLabeledError ro $ "Failed to submit contract: " <> show err
        pure ()
      Left err ->
        throwLabeledError ro $ "Failed to create contract: " <> show err

interpret ro@RuntimeApplyInputs {..} = do
  view executionModeL >>= skipInSimluationMode ro do
    Wallet { waAddress, waSigningKey } <- maybe getFaucet findWallet roSubmitter
    connector <- getConnector
    inputs <- for roInputs decodeInputJSON
    contractId <- getContractId roContractNickname
    let
      possibleChangeAddress = toChainSyncAddress waAddress
    changeAddress <- liftCliMaybe "Failed to create change address" possibleChangeAddress
    result <- liftIO $ flip runMarloweT connector do
      let
        stakeCredential = Nothing
        roleTokensConfig = RoleTokensNone
        walletAddresses = WalletAddresses
          { changeAddress = changeAddress
          , extraAddresses = mempty
          , collateralUtxos = mempty
          }
      Marlowe.Class.applyInputs' MarloweV1 walletAddresses contractId emptyMarloweTransactionMetadata Nothing Nothing inputs
    era <- view eraL
    case result of
      Right Transaction.InputsApplied { txBody, contractId } -> do
        -- logLabeledMsg ro $ "Contract created: " <> show res
        let
          witness = somePaymentsigningKeyToTxWitness waSigningKey
          tx = withShelleyBasedEra era . C.signShelleyTransaction txBody $ [witness]
          submitterNickname = fromMaybe faucetNickname roSubmitter

        logLabeledMsg ro "Submitting.."
        res <- liftIO $ flip runMarloweT connector do
          Marlowe.Class.submitAndWait tx
        logLabeledMsg ro "Submited and confirmed.."

        case res of
          Right tx -> do
            logLabeledMsg ro $ "Inputs applied: " <> show tx
            modifying knownContractsL $ Map.insert roContractNickname contractId
            updateWallet submitterNickname \submitter@Wallet {..} -> do
              submitter { waSubmittedTransactions = BabbageTxBody txBody : waSubmittedTransactions }
            pure ()
          Left err ->
            throwLabeledError ro $ "Failed to submit contract: " <> show err
        pure ()
      Left err ->
        throwLabeledError ro $ "Failed to create contract: " <> show err

