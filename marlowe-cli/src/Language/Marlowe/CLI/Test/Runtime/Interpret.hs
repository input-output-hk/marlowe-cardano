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

import Cardano.Api (CardanoMode)
import qualified Cardano.Api as C
import Contrib.Control.Concurrent.Async (timeoutIO)
import qualified Contrib.Data.Time.Units.Aeson as A
import Control.Concurrent.STM (atomically, readTVar, retry, writeTChan)
import Control.Lens (modifying, preview, use, view)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.STM (STM)
import Control.Monad.Trans.Marlowe (runMarloweT)
import qualified Control.Monad.Trans.Marlowe.Class as Marlowe.Class
import qualified Data.Aeson as A
import qualified Data.Aeson.OneLine as A
import Data.Coerce (coerce)
import Data.Foldable (for_)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Text as Text
import Data.Time.Units (Microsecond, Second, TimeUnit(fromMicroseconds, toMicroseconds))
import Data.Traversable (for)
import Language.Marlowe.CLI.Cardano.Api (withShelleyBasedEra)
import Language.Marlowe.CLI.IO (liftCliMaybe)
import Language.Marlowe.CLI.Test.Contract (ContractNickname(ContractNickname), Source(InlineContract, UseTemplate))
import Language.Marlowe.CLI.Test.Contract.Source (useTemplate)
import Language.Marlowe.CLI.Test.ExecutionMode (skipInSimluationMode)
import Language.Marlowe.CLI.Test.Log (Label, logLabeledMsg, throwLabeledError, throwTraceError)
import Language.Marlowe.CLI.Test.Runtime.Types
  ( ContractInfo(ContractInfo, ciContractId, ciMarloweThread, ciRoleCurrency)
  , InterpretMonad
  , RuntimeContractInfo
  , RuntimeMonitorInput(RuntimeMonitorInput)
  , RuntimeMonitorState(RuntimeMonitorState)
  , RuntimeOperation(..)
  , RuntimeTxInfo(RuntimeTxInfo)
  , anyRuntimeInterpreterMarloweThreadInputsApplied
  , connectionT
  , defaultOperationTimeout
  , eraL
  , executionModeL
  , knownContractsL
  , rcMarloweThread
  , runtimeClientConnectorT
  , runtimeMonitorInputT
  , runtimeMonitorStateT
  )
import Language.Marlowe.CLI.Test.Wallet.Interpret
  (decodeContractJSON, decodeInputJSON, findCurrency, findWallet, findWalletsByCurrencyTokens, getFaucet, updateWallet)
import Language.Marlowe.CLI.Test.Wallet.Types
  (Currency(Currency, ccPolicyId), SomeTxBody(BabbageTxBody), Wallet(..), WalletNickname, faucetNickname)
import Language.Marlowe.CLI.Types (somePaymentsigningKeyToTxWitness)
import Language.Marlowe.Cardano.Thread (anyMarloweThreadCreated, marloweThreadTxInfos, overAnyMarloweThread)
import qualified Language.Marlowe.Cardano.Thread as Marlowe.Cardano.Thread
import qualified Language.Marlowe.Protocol.Client as Marlowe.Protocol
<<<<<<< HEAD
import qualified Language.Marlowe.Protocol.Types as Marlowe.Protocol
=======
import qualified Language.Marlowe.Runtime.Cardano.Api as MRCA
import qualified Language.Marlowe.Runtime.Cardano.Api as RCA
>>>>>>> 910aca7a0 ( Implement pooc role based runtime test and add withdraw to the marlowe thread structure)
import qualified Language.Marlowe.Runtime.ChainSync.Api as ChainSync
import Language.Marlowe.Runtime.Core.Api (ContractId, MarloweVersion(MarloweV1), emptyMarloweTransactionMetadata)
import qualified Language.Marlowe.Runtime.Core.Api as R
import qualified Language.Marlowe.Runtime.Plutus.V2.Api as MRPA
import Language.Marlowe.Runtime.Transaction.Api
  ( RoleTokensConfig(RoleTokensNone, RoleTokensUsePolicy)
  , WalletAddresses(WalletAddresses, changeAddress, collateralUtxos, extraAddresses)
  )
import qualified Language.Marlowe.Runtime.Transaction.Api as Transaction
import qualified Network.Protocol.Connection as Network.Protocol
<<<<<<< HEAD
import Network.Protocol.Handshake.Types (Handshake)
=======
import qualified Plutus.V2.Ledger.Api as P
>>>>>>> 910aca7a0 ( Implement pooc role based runtime test and add withdraw to the marlowe thread structure)

-- connectionP :: Prism' env (LocalNodeConnectInfo CardanoMode)
getConnection
  :: forall env era st m
   . InterpretMonad env st m era
  => m (C.LocalNodeConnectInfo CardanoMode)
getConnection = do
  preview connectionT >>= \case
    Just conn -> pure conn
    Nothing -> throwTraceError "getConnection" "Connection not found"

-- runtimeClientConnectorP :: Prism' env (Network.Protocol.SomeClientConnector Marlowe.Protocol.MarloweRuntimeClient IO)
getConnector
<<<<<<< HEAD
  :: forall env st m lang era
   . InterpretMonad env st m lang era
  => m (Network.Protocol.ClientConnector (Handshake Marlowe.Protocol.MarloweRuntime) Marlowe.Protocol.MarloweRuntimeClient IO)
=======
  :: forall env era st m
   . InterpretMonad env st m era
  => m (Network.Protocol.SomeClientConnector Marlowe.Protocol.MarloweRuntimeClient IO)
>>>>>>> 910aca7a0 ( Implement pooc role based runtime test and add withdraw to the marlowe thread structure)
getConnector = do
  preview runtimeClientConnectorT >>= \case
    Just conn -> pure conn
    Nothing -> throwTraceError "getConnector" "Connector not found"

-- runtimeMonitorStateP :: Prism' env (RuntimeMonitorState)
getMonitorState
  :: forall env era st m
   . InterpretMonad env st m era
  => m RuntimeMonitorState
getMonitorState = do
  preview runtimeMonitorStateT >>= \case
    Just state -> pure state
    Nothing -> throwTraceError "getMonitorState" "Monitor state not found"

--runtimeMonitorInputP :: Prism' env RuntimeMonitorInput
getMonitorInput
  :: forall env era st m
   . InterpretMonad env st m era
  => m RuntimeMonitorInput
getMonitorInput = do
  preview runtimeMonitorInputT >>= \case
    Just input -> pure input
    Nothing -> throwTraceError "getMonitorInput" "Monitor input not found"

getContractInfo
  :: forall era env m st
   . InterpretMonad env st m era
  => ContractNickname
  -> m ContractInfo
getContractInfo nickname = do
  knownContracts <- use knownContractsL
  case Map.lookup nickname knownContracts of
    Just ci -> pure ci
    Nothing -> do
      throwTraceError "getContractInfo" $ "Contract info not found: " <> show nickname

getContractId
  :: forall era env m st
   . InterpretMonad env st m era
  => ContractNickname
  -> m ContractId
getContractId nickname = do
  ci <- getContractInfo nickname
  pure $ ciContractId ci

-- It is probably feasible to achieve this conversion without `Maybe`.
toChainSyncAddress :: C.AddressInEra era -> Maybe ChainSync.Address
toChainSyncAddress (C.AddressInEra C.ByronAddressInAnyEra _) = do
  Nothing
toChainSyncAddress (C.AddressInEra (C.ShelleyAddressInEra _) address') = do
  pure $ ChainSync.fromCardanoShelleyAddress address'

startMonitoring
  :: forall env era st m
   . InterpretMonad env st m era
  => ContractNickname
  -> m ()
startMonitoring contractNickname = do
  contractId <- getContractId contractNickname
  RuntimeMonitorInput runtimeMonitorInput <- getMonitorInput
  liftIO $ atomically $ do
    writeTChan runtimeMonitorInput (contractNickname, contractId)

awaitRuntimeContractInfo
  :: RuntimeMonitorState
  -> ContractNickname
  -> (Maybe RuntimeContractInfo -> Bool)
  -> STM (Maybe RuntimeContractInfo)
awaitRuntimeContractInfo (RuntimeMonitorState rmsRef) contractNickname check = do
  runtimeMonitorState <- readTVar rmsRef
  let
    info = Map.lookup contractNickname runtimeMonitorState
  if check info
  then pure info
  else retry

awaitNonEmptyContractInfo :: RuntimeMonitorState -> ContractNickname -> (RuntimeContractInfo -> Bool) -> STM RuntimeContractInfo
awaitNonEmptyContractInfo (RuntimeMonitorState rmsRef) contractNickname check = do
  runtimeMonitorState <- readTVar rmsRef
  case Map.lookup contractNickname runtimeMonitorState of
    Nothing -> retry
    Just info -> if check info
      then pure info
      else retry

anyMarloweThreadToJSON :: Marlowe.Cardano.Thread.AnyMarloweThread txInfo -> A.Value
anyMarloweThreadToJSON = overAnyMarloweThread Marlowe.Cardano.Thread.marloweThreadToJSON

operationTimeout :: Second -> RuntimeOperation -> Microsecond
operationTimeout seconds ro = do
  let
    seconds' = maybe seconds A.toSecond . roTimeout $ ro
  fromMicroseconds . (toMicroseconds :: Second -> Integer) $ seconds'

operationTimeout' :: RuntimeOperation -> Microsecond
operationTimeout' = operationTimeout defaultOperationTimeout

operationTimeoutLogged
  :: InterpretMonad env st m era
  => Second -> RuntimeOperation ->  m Microsecond
operationTimeoutLogged seconds ro = do
  let
    seconds' = maybe seconds A.toSecond . roTimeout $ ro
  logLabeledMsg ro $ "Timeout: " <> show seconds'
  pure $ fromMicroseconds . (toMicroseconds :: Second -> Integer) $ seconds'

operationTimeoutLogged'
  :: InterpretMonad env st m era
  => RuntimeOperation ->  m Microsecond
operationTimeoutLogged' = operationTimeoutLogged defaultOperationTimeout

<<<<<<< HEAD
-- We want to avoid recursive calls in the `interpret` function but reuse this functionality.
awaitInputsApplied
  :: forall era env lang m st
   . InterpretMonad env st m lang era
  => RuntimeOperation
=======
runtimeAwaitTxsConfirmed
  :: forall era env l m st
   . InterpretMonad env st m era
  => Label l
  => l
>>>>>>> 910aca7a0 ( Implement pooc role based runtime test and add withdraw to the marlowe thread structure)
  -> ContractNickname
  -> Microsecond
  -> m ()
runtimeAwaitTxsConfirmed ro contractNickname timeout = do
  startMonitoring contractNickname
  ContractInfo { ciMarloweThread=interpreterMarloweThread } <- getContractInfo contractNickname
  let
    txInfos :: [RuntimeTxInfo]
    txInfos = overAnyMarloweThread marloweThreadTxInfos interpreterMarloweThread

    check runtimeMarloweThread = do
      let
        runtimeConfirmedTxs :: [C.TxId]
        runtimeConfirmedTxs = overAnyMarloweThread marloweThreadTxInfos runtimeMarloweThread

        allTxIds = map (\(RuntimeTxInfo _ txId) -> txId) txInfos
      all (`elem` runtimeConfirmedTxs) allTxIds

  rms <- getMonitorState
  res <- liftIO $ timeoutIO timeout $ atomically (awaitNonEmptyContractInfo rms contractNickname $ check . view rcMarloweThread)
  when (isNothing res) do
    let
      getRuntimeContractInfo = awaitRuntimeContractInfo rms contractNickname (const True)
    thread <- liftIO $ atomically getRuntimeContractInfo
    let
      contractState = maybe "<empty>" (Text.unpack . A.renderValue . anyMarloweThreadToJSON . view rcMarloweThread) thread
    throwLabeledError ro $ "Timeout reached while waiting for txs confirmation: " <> show contractNickname <> ". Contract info: " <> contractState

withdraw
  :: forall env era st m
   . InterpretMonad env st m era
  => RuntimeOperation
  -> ContractId
  -> P.TokenName
  -> WalletNickname
  -> Wallet era
  -> m ()
withdraw ro contractId tokenName walletNickname Wallet { waAddress, waSigningKey } = do
  connector <- getConnector
  let
    tokenName' = MRPA.fromPlutusTokenName tokenName
    possibleChangeAddress = toChainSyncAddress waAddress
  changeAddress <- liftCliMaybe "Failed to create change address" possibleChangeAddress
  result <- liftIO $ flip runMarloweT connector do
    let
      walletAddresses = WalletAddresses
        { changeAddress = changeAddress
        , extraAddresses = mempty
        , collateralUtxos = mempty
        }
    Marlowe.Class.withdraw MarloweV1 walletAddresses contractId tokenName'
  era <- view eraL
  case result of
    Right txBody -> do
      let
        witness = somePaymentsigningKeyToTxWitness waSigningKey
        tx = withShelleyBasedEra era . C.signShelleyTransaction txBody $ [witness]
      res <- liftIO $ flip runMarloweT connector do
        Marlowe.Class.submitAndWait tx
      case res of
        Right bl -> do
          logLabeledMsg ro $ "Withdrawal submitted and confirmed: " <> show bl
          updateWallet walletNickname \wallet'@Wallet {waSubmittedTransactions} -> do
            wallet' { waSubmittedTransactions = BabbageTxBody txBody : waSubmittedTransactions }
        Left err ->
          throwLabeledError ro $ "Failed to submit withdrawal: " <> show err
    Left err ->
      throwLabeledError ro $ "Failed to create the withdrawal: " <> show err

interpret
  :: forall era env m st
   . InterpretMonad env st m era
  => RuntimeOperation
  -> m ()
interpret ro@RuntimeAwaitTxsConfirmed {..} = do
  let
    microseconds = fromMicroseconds . toMicroseconds $ maybe defaultOperationTimeout A.toSecond roTimeout
  runtimeAwaitTxsConfirmed ro roContractNickname microseconds

interpret ro@RuntimeWithdraw {..} = do
  ContractInfo{ciContractId=contractId, ciRoleCurrency=possibleRoleCurrecy} <- getContractInfo roContractNickname
  case possibleRoleCurrecy of
    Nothing -> throwLabeledError ro $ "Unable to withdraw from contract: " <> show roContractNickname <> ". Role currency is not set."
    Just roleCurrency -> do
      allWallets <- findWalletsByCurrencyTokens roleCurrency Nothing
      let
        wallets = case roWallets of
          Nothing -> allWallets
          Just nicknames -> [ (nickname, wallet, tokenNames) | (nickname, wallet, tokenNames) <- allWallets, nickname `elem` nicknames ]

      for_ wallets \(walletNickname, wallet, tokenNames) -> do
        for_ tokenNames \tokenName -> do
          withdraw ro contractId tokenName walletNickname wallet
      case roAwaitConfirmed of
        Nothing -> pure ()
        Just timeout -> do
          let
            microseconds = fromMicroseconds . toMicroseconds . A.toSecond $ timeout
          runtimeAwaitTxsConfirmed ro roContractNickname microseconds

interpret ro@RuntimeAwaitClosed {..} = do
  view executionModeL >>= skipInSimluationMode ro do
    startMonitoring roContractNickname
    rms <- getMonitorState
    logLabeledMsg ro $ "Waiting till contract instance is closed: " <> show (coerce roContractNickname :: String)
    timeout <- operationTimeoutLogged' ro
    let
      closedL :: RuntimeContractInfo -> Bool
      closedL = not . Marlowe.Cardano.Thread.isRunning . view rcMarloweThread

    res <- liftIO $ timeoutIO timeout $ atomically (awaitNonEmptyContractInfo rms roContractNickname closedL)
    when (isNothing res) do
      let
        getRuntimeContractInfo = awaitRuntimeContractInfo rms roContractNickname (const True)
      thread <- liftIO $ atomically getRuntimeContractInfo
      let
        contractState = maybe "<empty>" (Text.unpack . A.renderValue . anyMarloweThreadToJSON . view rcMarloweThread) thread
      throwLabeledError ro $ "Timeout reached while waiting for contract instance to be closed: " <> show roContractNickname <> ". Contract info: " <> contractState

interpret ro@RuntimeCreateContract {..} = do
  view executionModeL >>= skipInSimluationMode ro do
    Wallet { waAddress, waSigningKey } <- maybe getFaucet findWallet roSubmitter
    connector <- getConnector
    -- Verify that the role currency actually exists
    contract <- case roContractSource of
      InlineContract json -> decodeContractJSON json
      UseTemplate setup   -> useTemplate roRoleCurrency setup
    let
      possibleChangeAddress = toChainSyncAddress waAddress
    changeAddress <- liftCliMaybe "Failed to create change address" possibleChangeAddress

    logLabeledMsg ro $ "Invoking contract creation: " <> show roContractNickname

    roleTokensConfig <- case roRoleCurrency of
      Just roleCurrency -> do
        Currency{ ccPolicyId=cardanoPolicyId } <- findCurrency roleCurrency
        let
          policyId = MRCA.fromCardanoPolicyId cardanoPolicyId
        pure $ RoleTokensUsePolicy policyId
      Nothing -> pure RoleTokensNone
    result <- liftIO $ flip runMarloweT connector do
      let
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
            let
              txId = C.getTxId txBody
              possibleTxIn = do
                let
                  R.ContractId utxo = contractId
                RCA.toCardanoTxIn utxo
            case possibleTxIn of
              Nothing -> logLabeledMsg ro $ "Failed to convert TxIx for a contract: " <> show contractId
              Just txIn -> do
                let
                  runtimeTxInfo = RuntimeTxInfo False txId
                  anyMarloweThread = anyMarloweThreadCreated runtimeTxInfo txIn
                  contractInfo = ContractInfo
                    { ciContractId=contractId
                    , ciRoleCurrency=roRoleCurrency
                    , ciMarloweThread=anyMarloweThread
                    }
                modifying knownContractsL $ Map.insert roContractNickname contractInfo
                updateWallet submitterNickname \submitter@Wallet {waSubmittedTransactions} -> do
                  submitter { waSubmittedTransactions = BabbageTxBody txBody : waSubmittedTransactions }
                case roAwaitConfirmed of
                  Nothing -> pure ()
                  Just timeout -> do
                    let
                      microseconds = fromMicroseconds . toMicroseconds . A.toSecond $ timeout
                    runtimeAwaitTxsConfirmed ro roContractNickname microseconds
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
    logLabeledMsg ro $ "Applying inputs:" <> show inputs
    result <- liftIO $ flip runMarloweT connector do
      let
        walletAddresses = WalletAddresses
          { changeAddress = changeAddress
          , extraAddresses = mempty
          , collateralUtxos = mempty
          }
      Marlowe.Class.applyInputs' MarloweV1 walletAddresses contractId emptyMarloweTransactionMetadata Nothing Nothing inputs
    era <- view eraL
    case result of
      Right Transaction.InputsApplied { output=possibleMarloweOutput, txBody } -> do
        logLabeledMsg ro "Successful application."
        let
          witness = somePaymentsigningKeyToTxWitness waSigningKey
          tx = withShelleyBasedEra era . C.signShelleyTransaction txBody $ [witness]
          submitterNickname = fromMaybe faucetNickname roSubmitter

        logLabeledMsg ro "Submitting..."
        res <- liftIO $ flip runMarloweT connector do
          Marlowe.Class.submitAndWait tx
        logLabeledMsg ro "Submitted and confirmed."

        case res of
          Right bl -> do
            logLabeledMsg ro $ "Inputs applied: " <> show bl
            knownContracts <- use knownContractsL
            case Map.lookup roContractNickname knownContracts of
              Nothing -> throwLabeledError ro $ "Contract not found: " <> show roContractNickname
              Just contractInfo@ContractInfo { ciMarloweThread=th } -> do
                let
                  txId = C.getTxId txBody
                  possibleTxIx = do
                    R.TransactionScriptOutput { R.utxo=utxo } <- possibleMarloweOutput
                    C.TxIn _ txIx <- RCA.toCardanoTxIn utxo
                    pure txIx

                case anyRuntimeInterpreterMarloweThreadInputsApplied txId possibleTxIx inputs th of
                  Nothing -> throwLabeledError ro
                    $ "Failed to extend the marlowe thread with the applied inputs: "
                    <> (Text.unpack . A.renderValue . anyMarloweThreadToJSON $ th)
                  Just th' -> do
                    modifying knownContractsL $ Map.insert roContractNickname $ contractInfo { ciMarloweThread=th' }
            updateWallet submitterNickname \submitter@Wallet {waSubmittedTransactions} -> do
              submitter { waSubmittedTransactions = BabbageTxBody txBody : waSubmittedTransactions }

            case roAwaitConfirmed of
              Nothing -> pure ()
              Just timeout -> do
                let
                  microseconds = fromMicroseconds . toMicroseconds . A.toSecond $ timeout
                runtimeAwaitTxsConfirmed ro roContractNickname microseconds
          Left err ->
            throwLabeledError ro $ "Failed to submit contract: " <> show err
      Left err ->
        throwLabeledError ro $ "Failed to create contract: " <> show err
