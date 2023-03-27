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
import Cardano.Api (toAddressAny)
import qualified Cardano.Api as C
import Contrib.Control.Concurrent.Async (timeoutIO)
import Control.Concurrent.STM (TVar, atomically, readTVar, retry, writeTChan)
import Control.Lens (modifying, use, view)
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
import Language.Marlowe.CLI.Test.Contract (ContractNickname(ContractNickname))
import Language.Marlowe.CLI.Test.Contract.ParametrizedMarloweJSON
  (ParametrizedMarloweJSON(ParametrizedMarloweJSON), decodeParametrizedContractJSON, decodeParametrizedInputJSON)
import Language.Marlowe.CLI.Test.ExecutionMode (skipInSimluationMode)
import Language.Marlowe.CLI.Test.Log (logLabeledMsg, throwLabeledError, throwTraceError)
import Language.Marlowe.CLI.Test.Runtime.Types
  ( InterpretMonad
  , RuntimeContractInfo(RuntimeContractInfo)
  , RuntimeMonitorInput(RuntimeMonitorInput)
  , RuntimeMonitorState(RuntimeMonitorState)
  , RuntimeOperation(..)
  , ieConnection
  , ieEra
  , ieExecutionMode
  , ieRuntimeClientConnector
  , ieRuntimeMonitorInput
  , ieRuntimeMonitorState
  , isCurrencies
  , isKnownContracts
  , isWallets
  , rcMarloweThread
  )
import Language.Marlowe.CLI.Test.Wallet.Types
  (Wallet(..), WalletNickname(WalletNickname), Wallets(Wallets), faucetNickname)
import Language.Marlowe.CLI.Types (CliError(CliError), somePaymentsigningKeyToTxWitness)
import Language.Marlowe.Cardano (marloweNetworkFromLocalNodeConnectInfo)
import Language.Marlowe.Cardano.Thread (overAnyMarloweThread)
import qualified Language.Marlowe.Cardano.Thread as Marlowe.Cardano.Thread
import qualified Language.Marlowe.Core.V1.Semantics.Types as M
import qualified Language.Marlowe.Runtime.ChainSync.Api as ChainSync
import Language.Marlowe.Runtime.Core.Api
  (ContractId, MarloweVersion(MarloweV1), MarloweVersionTag(V1), emptyMarloweTransactionMetadata)
import Language.Marlowe.Runtime.Transaction.Api
  (RoleTokensConfig(RoleTokensNone), WalletAddresses(WalletAddresses, changeAddress, collateralUtxos, extraAddresses))
import qualified Language.Marlowe.Runtime.Transaction.Api as Transaction


findWallet
  :: InterpretMonad m lang era
  => WalletNickname
  -> m (Wallet era)
findWallet nickname =
  use isWallets >>= \(Wallets wallets) ->
  liftCliMaybe ("[findWallet] Unable to find wallet:" <> show nickname) $ Map.lookup nickname wallets

getFaucet
  :: InterpretMonad m lang era
  => m (Wallet era)
getFaucet =
  findWallet faucetNickname

decodeContractJSON
  :: InterpretMonad m lang era
  => ParametrizedMarloweJSON
  -> m M.Contract
decodeContractJSON json = do
  currencies <- use isCurrencies
  wallets <- use isWallets
  network <- view ieConnection <&> marloweNetworkFromLocalNodeConnectInfo
  case decodeParametrizedContractJSON network wallets currencies json of
    Left err -> throwError $ CliError $ "Failed to decode contract: " <> show err
    Right c  -> pure c

decodeInputJSON
  :: InterpretMonad m lang era
  => ParametrizedMarloweJSON
  -> m M.Input
decodeInputJSON json = do
  currencies <- use isCurrencies
  wallets <- use isWallets
  network <- view ieConnection <&> marloweNetworkFromLocalNodeConnectInfo
  case decodeParametrizedInputJSON network wallets currencies json of
    Left err -> throwError $ CliError $ "Failed to decode inputs: " <> show err
    Right c  -> pure c

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

-- It is probably feasible to achieve this conversion without `Maybe`.
toChainSyncAddress :: C.AddressInEra era -> Maybe ChainSync.Address
toChainSyncAddress (C.AddressInEra C.ByronAddressInAnyEra _) = do
  Nothing
toChainSyncAddress (C.AddressInEra (C.ShelleyAddressInEra _) address') = do
  pure $ ChainSync.fromCardanoShelleyAddress address'


startMonitoring contractNickname = do
  contractId <- getContractId contractNickname
  RuntimeMonitorInput runtimeMonitorInput <- view ieRuntimeMonitorInput
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
  :: InterpretMonad m lang era
  => Second -> RuntimeOperation ->  m Microsecond
operationTimeoutLogged seconds ro = do
  let
    seconds' = maybe seconds (fromInteger :: Integer -> Second) . roTimeout $ ro
  logLabeledMsg ro $ "Timeout: " <> show seconds'
  pure $ fromMicroseconds . (toMicroseconds :: Second -> Integer) $ seconds'

interpret
  :: forall era lang m
   . InterpretMonad m lang era
  => C.IsShelleyBasedEra era
  => RuntimeOperation
  -> m ()
interpret ro@RuntimeAwaitCreated {..} = do
  view ieExecutionMode >>= skipInSimluationMode ro do
    startMonitoring roContractNickname
    rms <- view ieRuntimeMonitorState
    let
      getContractInfo = awaitNonEmptyContractInfo rms roContractNickname (const True)
    logLabeledMsg ro $ "Waiting for contract instance: " <> show (coerce roContractNickname :: String)
    timeout <- operationTimeoutLogged 60 ro
    (liftIO $ timeoutIO timeout (atomically getContractInfo)) >>= \case
      Just (RuntimeContractInfo thread) -> do
        logLabeledMsg ro $ "Contract instance created: " <> show roContractNickname
      _ -> throwLabeledError ro $ "Timeout reached while waiting for contract instance creation: " <> show roContractNickname

interpret ro@RuntimeAwaitInputsApplied {..} = do
  view ieExecutionMode >>= skipInSimluationMode ro do
    startMonitoring roContractNickname
    rms <- view ieRuntimeMonitorState
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
  view ieExecutionMode >>= skipInSimluationMode ro do
    startMonitoring roContractNickname
    rms <- view ieRuntimeMonitorState
    logLabeledMsg ro $ "Waiting till contract instance is closed: " <> show (coerce roContractNickname :: String)
    timeout <- operationTimeoutLogged 60 ro
    let
      isClosed :: RuntimeContractInfo lang era -> Bool
      isClosed = not . Marlowe.Cardano.Thread.isRunning . view rcMarloweThread

    res <- liftIO $ timeoutIO timeout $ atomically (awaitNonEmptyContractInfo rms roContractNickname isClosed)
    when (isNothing res) do
      let
        getContractInfo = awaitContractInfo rms roContractNickname (const True)
      thread <- liftIO $ atomically getContractInfo
      let
        contractState = maybe "<empty>" (Text.unpack . A.renderValue . anyMarloweThreadToJSON . view rcMarloweThread) thread
      throwLabeledError ro $ "Timeout reached while waiting for contract instance to be closed: " <> show roContractNickname <> ". Contract info: " <> contractState

interpret ro@RuntimeCreateContract {..} = do
  view ieExecutionMode >>= skipInSimluationMode ro do
    Wallet { waAddress, waSigningKey } <- maybe getFaucet findWallet roSubmitter
    connector <- view ieRuntimeClientConnector
    contract <- decodeContractJSON roContract
    let
      possibleChangeAddress = toChainSyncAddress waAddress
    changeAddress <- liftCliMaybe "Failed to create change address" possibleChangeAddress

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
      -- createContract
      --   :: MonadMarlowe m
      --   => Maybe StakeCredential
      --   -- ^ A reference to the stake address to use for script addresses.
      --   -> MarloweVersion v
      --   -- ^ The Marlowe version to use
      --   -> WalletAddresses
      --   -- ^ The wallet addresses to use when constructing the transaction
      --   -> RoleTokensConfig
      --   -- ^ How to initialize role tokens
      --   -> MarloweTransactionMetadata
      --   -- ^ Optional metadata to attach to the transaction
      --   -> Lovelace
      --   -- ^ Min Lovelace which should be used for the contract output.
      --   -> Contract v
      --   -- ^ The contract to run
      --   -> m (Either (CreateError v) (ContractCreated BabbageEra v))
      Marlowe.Class.createContract Nothing MarloweV1 walletAddresses roleTokensConfig emptyMarloweTransactionMetadata minLovelace contract
    era <- view ieEra
    case result of
      -- data ContractCreated era v = ContractCreated
      --   { contractId :: ContractId
      --   , rolesCurrency :: PolicyId
      --   , metadata :: MarloweTransactionMetadata
      --   , marloweScriptHash :: ScriptHash
      --   , marloweScriptAddress :: Address
      --   , payoutScriptHash :: ScriptHash
      --   , payoutScriptAddress :: Address
      --   , version :: MarloweVersion v
      --   , datum :: Datum v
      --   , assets :: Assets
      --   , txBody :: TxBody era
      --   }
      Right Transaction.ContractCreated { txBody, contractId } -> do
        -- logLabeledMsg ro $ "Contract created: " <> show res
        let
          witness = somePaymentsigningKeyToTxWitness waSigningKey
          tx = withShelleyBasedEra era . C.signShelleyTransaction txBody $ [witness]
        res <- liftIO $ flip runMarloweT connector do
          Marlowe.Class.submitAndWait tx

        case res of
          Right _ -> do
            logLabeledMsg ro $ "Contract created: " <> show tx
            modifying isKnownContracts $ Map.insert roContractNickname contractId
            pure ()
          Left err ->
            throwLabeledError ro $ "Failed to submit contract: " <> show err
        pure ()
      Left err ->
        throwLabeledError ro $ "Failed to create contract: " <> show err

interpret ro@RuntimeApplyInputs {..} = do
  view ieExecutionMode >>= skipInSimluationMode ro do
    Wallet { waAddress, waSigningKey } <- maybe getFaucet findWallet roSubmitter
    connector <- view ieRuntimeClientConnector
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

      -- -- ^ Apply inputs to a contract, with custom validity interval bounds.
      -- applyInputs'
      --   :: MonadMarlowe m
      --   => MarloweVersion v
      --   -- ^ The Marlowe version to use
      --   -> WalletAddresses
      --   -- ^ The wallet addresses to use when constructing the transaction
      --   -> ContractId
      --   -- ^ The ID of the contract to apply the inputs to.
      --   -> MarloweTransactionMetadata
      --   -- ^ Optional metadata to attach to the transaction
      --   -> Maybe UTCTime
      --   -- ^ The "invalid before" bound of the validity interval. If omitted, this
      --   -- is computed from the contract.
      --   -> Maybe UTCTime
      --   -- ^ The "invalid hereafter" bound of the validity interval. If omitted, this
      --   -- is computed from the contract.
      --   -> Inputs v
      --   -- ^ The inputs to apply.
      --   -> m (Either (ApplyInputsError v) (InputsApplied BabbageEra v))

      Marlowe.Class.applyInputs' MarloweV1 walletAddresses contractId emptyMarloweTransactionMetadata Nothing Nothing inputs
    era <- view ieEra
    case result of
      --  data InputsApplied era v = InputsApplied
      --    { version :: MarloweVersion v
      --    , contractId :: ContractId
      --    , metadata :: MarloweTransactionMetadata
      --    , input :: TransactionScriptOutput v
      --    , output :: Maybe (TransactionScriptOutput v)
      --    , invalidBefore :: UTCTime
      --    , invalidHereafter :: UTCTime
      --    , inputs :: Inputs v
      --    , txBody :: TxBody era
      --    }
      Right Transaction.InputsApplied { txBody, contractId } -> do
        -- logLabeledMsg ro $ "Contract created: " <> show res
        let
          witness = somePaymentsigningKeyToTxWitness waSigningKey
          tx = withShelleyBasedEra era . C.signShelleyTransaction txBody $ [witness]
        logLabeledMsg ro "Submitting.."
        res <- liftIO $ flip runMarloweT connector do
          Marlowe.Class.submitAndWait tx
        logLabeledMsg ro "Submited and confirmed.."

        case res of
          Right _ -> do
            logLabeledMsg ro $ "Inputs applied: " <> show tx
            modifying isKnownContracts $ Map.insert roContractNickname contractId
            pure ()
          Left err ->
            throwLabeledError ro $ "Failed to submit contract: " <> show err
        pure ()
      Left err ->
        throwLabeledError ro $ "Failed to create contract: " <> show err

