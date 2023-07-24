{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Marlowe.CLI.Test.Runtime.Interpret where

import Cardano.Api (CardanoMode, ScriptDataSupportedInEra (ScriptDataInBabbageEra))
import Cardano.Api qualified as C
import Contrib.Control.Concurrent.Async (timeoutIO)
import Contrib.Control.Monad.Except (note)
import Contrib.Data.List.Random (combinationWithRepetitions)
import Contrib.Data.Time.Units.Aeson qualified as A
import Control.Concurrent.STM (atomically, readTVar, retry, writeTChan)
import Control.Lens (modifying, preview, use, view)
import Control.Monad (when)
import Control.Monad.Except (MonadError (catchError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Loops (untilJust)
import Control.Monad.STM (STM)
import Control.Monad.Trans.Marlowe (runMarloweT)
import Control.Monad.Trans.Marlowe.Class qualified as Marlowe.Class
import Control.Monad.Writer.Lazy (runWriter)
import Data.Aeson (toJSON)
import Data.Aeson qualified as A
import Data.Aeson.OneLine qualified as A
import Data.Bifunctor qualified as Bifunctor
import Data.Coerce (coerce)
import Data.Foldable (for_)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text qualified as Text
import Data.Time.Units (Microsecond, Second, TimeUnit (fromMicroseconds, toMicroseconds))
import Data.Traversable (for)
import Language.Marlowe.CLI.Cardano.Api (withShelleyBasedEra)
import Language.Marlowe.CLI.Test.Contract (ContractNickname (ContractNickname), Source (InlineContract, UseTemplate))
import Language.Marlowe.CLI.Test.Contract.Source (useTemplate)
import Language.Marlowe.CLI.Test.ExecutionMode (skipInSimluationMode)
import Language.Marlowe.CLI.Test.InterpreterError (runtimeOperationFailed', testExecutionFailed', timeoutReached')
import Language.Marlowe.CLI.Test.Log (Label, logStoreLabeledMsg, logStoreMsgWith, throwLabeledError)
import Language.Marlowe.CLI.Test.Runtime.Types (
  ContractInfo (ContractInfo, _ciContinuations, _ciContract, _ciContractId, _ciMarloweThread, _ciRoleCurrency),
  DoMerkleize (ClientSide, RuntimeSide),
  InterpretMonad,
  RuntimeContractInfo,
  RuntimeMonitorInput (RuntimeMonitorInput),
  RuntimeMonitorState (RuntimeMonitorState),
  RuntimeOperation (..),
  RuntimeTxInfo (RuntimeTxInfo),
  anyRuntimeInterpreterMarloweThreadInputsApplied,
  connectionT,
  defaultOperationTimeout,
  eraL,
  executionModeL,
  knownContractsL,
  rcMarloweThread,
  runtimeClientConnectorT,
  runtimeMonitorInputT,
  runtimeMonitorStateT,
 )
import Language.Marlowe.CLI.Test.Wallet.Interpret (
  decodeContractJSON,
  decodeInputJSON,
  findCurrency,
  findWallet,
  findWalletsByCurrencyTokens,
  getFaucet,
  updateWallet,
 )
import Language.Marlowe.CLI.Test.Wallet.Types (
  Currency (Currency, ccPolicyId),
  SomeTxBody (SomeTxBody),
  Wallet (..),
  WalletNickname,
  faucetNickname,
 )
import Language.Marlowe.CLI.Types (somePaymentsigningKeyToTxWitness)
import Language.Marlowe.Cardano.Thread (
  anyMarloweThreadCreated,
  anyMarloweThreadRedeemed,
  marloweThreadTxInfos,
  overAnyMarloweThread,
 )
import Language.Marlowe.Cardano.Thread qualified as Marlowe.Cardano.Thread
import Language.Marlowe.Core.V1.Merkle (deepMerkleize)
import Language.Marlowe.Protocol.Client qualified as Marlowe.Protocol
import Language.Marlowe.Runtime.Cardano.Api qualified as MRCA
import Language.Marlowe.Runtime.Cardano.Api qualified as RCA
import Language.Marlowe.Runtime.ChainSync.Api qualified as ChainSync
import Language.Marlowe.Runtime.Core.Api (ContractId, MarloweVersion (MarloweV1), emptyMarloweTransactionMetadata)
import Language.Marlowe.Runtime.Core.Api qualified as R
import Language.Marlowe.Runtime.Plutus.V2.Api qualified as MRPA
import Language.Marlowe.Runtime.Transaction.Api (
  RoleTokensConfig (RoleTokensNone, RoleTokensUsePolicy),
  WalletAddresses (WalletAddresses, changeAddress, collateralUtxos, extraAddresses),
  WithdrawTx (..),
 )
import Language.Marlowe.Runtime.Transaction.Api qualified as Transaction
import Network.Protocol.Connection qualified as Network.Protocol
import Plutus.V2.Ledger.Api qualified as P
import System.IO (hPutStrLn, stderr)

-- connectionP :: Prism' env (LocalNodeConnectInfo CardanoMode)
getConnection
  :: forall env era st m
   . (InterpretMonad env st m era)
  => m (C.LocalNodeConnectInfo CardanoMode)
getConnection = do
  preview connectionT >>= \case
    Just conn -> pure conn
    Nothing -> throwLabeledError ("getConnection" :: String) (testExecutionFailed' "Connection not found")

-- runtimeClientConnectorP :: Prism' env (Network.Protocol.SomeClientConnector Marlowe.Protocol.MarloweRuntimeClient IO)
getConnector
  :: forall env era st m
   . (InterpretMonad env st m era)
  => m (Network.Protocol.Connector Marlowe.Protocol.MarloweRuntimeClient IO)
getConnector = do
  preview runtimeClientConnectorT >>= \case
    Just conn -> pure conn
    Nothing -> throwLabeledError ("getConnector" :: String) (testExecutionFailed' "Connector not found")

-- runtimeMonitorStateP :: Prism' env (RuntimeMonitorState)
getMonitorState
  :: forall env era st m
   . (InterpretMonad env st m era)
  => m RuntimeMonitorState
getMonitorState = do
  preview runtimeMonitorStateT >>= \case
    Just state -> pure state
    Nothing -> throwLabeledError ("getMonitorState" :: String) (testExecutionFailed' "Monitor state not found")

-- runtimeMonitorInputP :: Prism' env RuntimeMonitorInput
getMonitorInput
  :: forall env era st m
   . (InterpretMonad env st m era)
  => m RuntimeMonitorInput
getMonitorInput = do
  preview runtimeMonitorInputT >>= \case
    Just input -> pure input
    Nothing -> throwLabeledError ("getMonitorInput" :: String) (testExecutionFailed' "Monitor input not found")

getContractInfo
  :: forall era env m st
   . (InterpretMonad env st m era)
  => Maybe ContractNickname
  -> m (ContractNickname, ContractInfo)
getContractInfo (Just nickname) = do
  knownContracts <- use knownContractsL
  case Map.lookup nickname knownContracts of
    Just ci -> pure (nickname, ci)
    Nothing -> do
      throwLabeledError ("getContractInfo" :: String) $ testExecutionFailed' $ "Contract info not found: " <> show nickname
getContractInfo Nothing = do
  knownContracts <- use knownContractsL
  case Map.toList knownContracts of
    [pair] -> pure pair
    _ -> do
      throwLabeledError ("getContractInfo" :: String) $
        testExecutionFailed' $
          "Contract info is ambiguous: " <> show (Map.keys knownContracts)

getContractInfo'
  :: forall era env m st
   . (InterpretMonad env st m era)
  => Maybe ContractNickname
  -> m ContractInfo
getContractInfo' possibleNickname = snd <$> getContractInfo possibleNickname

getContractId
  :: forall era env m st
   . (InterpretMonad env st m era)
  => ContractNickname
  -> m ContractId
getContractId nickname = do
  ci <- getContractInfo' (Just nickname)
  pure $ _ciContractId ci

-- It is probably feasible to achieve this conversion without `Maybe`.
toChainSyncAddress :: C.AddressInEra era -> Maybe ChainSync.Address
toChainSyncAddress (C.AddressInEra C.ByronAddressInAnyEra _) = do
  Nothing
toChainSyncAddress (C.AddressInEra (C.ShelleyAddressInEra _) address') = do
  pure $ ChainSync.fromCardanoShelleyAddress address'

startMonitoring
  :: forall env era st m
   . (InterpretMonad env st m era)
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
  let info = Map.lookup contractNickname runtimeMonitorState
  if check info
    then pure info
    else retry

awaitNonEmptyContractInfo
  :: RuntimeMonitorState -> ContractNickname -> (RuntimeContractInfo -> Bool) -> STM RuntimeContractInfo
awaitNonEmptyContractInfo (RuntimeMonitorState rmsRef) contractNickname check = do
  runtimeMonitorState <- readTVar rmsRef
  case Map.lookup contractNickname runtimeMonitorState of
    Nothing -> retry
    Just info ->
      if check info
        then pure info
        else retry

anyMarloweThreadToJSON :: Marlowe.Cardano.Thread.AnyMarloweThread txInfo -> A.Value
anyMarloweThreadToJSON = overAnyMarloweThread Marlowe.Cardano.Thread.marloweThreadToJSON

operationTimeout :: Second -> RuntimeOperation -> Microsecond
operationTimeout seconds ro = do
  let seconds' = maybe seconds A.toSecond . roTimeout $ ro
  fromMicroseconds . (toMicroseconds :: Second -> Integer) $ seconds'

operationTimeout' :: RuntimeOperation -> Microsecond
operationTimeout' = operationTimeout defaultOperationTimeout

operationTimeoutLogged
  :: (InterpretMonad env st m era)
  => Second
  -> RuntimeOperation
  -> m Microsecond
operationTimeoutLogged seconds ro = do
  let seconds' = maybe seconds A.toSecond . roTimeout $ ro
  logStoreLabeledMsg ro $ "Timeout: " <> show seconds'
  pure $ fromMicroseconds . (toMicroseconds :: Second -> Integer) $ seconds'

operationTimeoutLogged'
  :: (InterpretMonad env st m era)
  => RuntimeOperation
  -> m Microsecond
operationTimeoutLogged' = operationTimeoutLogged defaultOperationTimeout

runtimeAwaitTxsConfirmed
  :: forall era env l m st t
   . (InterpretMonad env st m era)
  => (TimeUnit t)
  => (Label l)
  => l
  -> Maybe ContractNickname
  -> t
  -> m ()
runtimeAwaitTxsConfirmed ro possibleContractNickname timeout = do
  (contractNickname, ContractInfo{_ciMarloweThread = interpreterMarloweThread}) <-
    getContractInfo possibleContractNickname
  startMonitoring contractNickname
  let txInfos :: [RuntimeTxInfo]
      txInfos = overAnyMarloweThread marloweThreadTxInfos interpreterMarloweThread

      check runtimeMarloweThread = do
        let runtimeConfirmedTxs :: [C.TxId]
            runtimeConfirmedTxs = overAnyMarloweThread marloweThreadTxInfos runtimeMarloweThread

            allTxIds = map (\(RuntimeTxInfo txId) -> txId) txInfos
        all (`elem` runtimeConfirmedTxs) allTxIds

  rms <- getMonitorState
  res <-
    liftIO $ timeoutIO timeout $ atomically (awaitNonEmptyContractInfo rms contractNickname $ check . view rcMarloweThread)
  when (isNothing res) do
    let getRuntimeContractInfo = awaitRuntimeContractInfo rms contractNickname (const True)
    thread <- liftIO $ atomically getRuntimeContractInfo
    let monitorContractInfo = maybe "<empty>" (Text.unpack . A.renderValue . anyMarloweThreadToJSON . view rcMarloweThread) thread
        interpreterContractInfo = Text.unpack . A.renderValue . anyMarloweThreadToJSON $ interpreterMarloweThread
    throwLabeledError ro $
      timeoutReached' $
        "Timeout reached while waiting for txs confirmation: "
          <> show contractNickname
          <> ". Monitor contract info: "
          <> monitorContractInfo
          <> ". Interpreter contract info: "
          <> interpreterContractInfo

withdraw
  :: forall env era st m
   . (InterpretMonad env st m era)
  => RuntimeOperation
  -> ContractId
  -> P.TokenName
  -> WalletNickname
  -> Wallet era
  -> m C.TxId
withdraw ro contractId tokenName walletNickname Wallet{_waAddress, _waSigningKey} = do
  connector <- getConnector
  let tokenName' = MRPA.fromPlutusTokenName tokenName
      possibleChangeAddress = toChainSyncAddress _waAddress
  changeAddress <- note (testExecutionFailed' "Failed to create change address") possibleChangeAddress
  result <- liftIO $ flip runMarloweT connector do
    let walletAddresses =
          WalletAddresses
            { changeAddress = changeAddress
            , extraAddresses = mempty
            , collateralUtxos = mempty
            }
    Marlowe.Class.withdraw MarloweV1 walletAddresses contractId tokenName'
  era <- view eraL
  case result of
    Right WithdrawTx{..} -> do
      let witness = somePaymentsigningKeyToTxWitness _waSigningKey
          tx = withShelleyBasedEra era . C.signShelleyTransaction txBody $ [witness]
      res <- liftIO $ flip runMarloweT connector do
        Marlowe.Class.submitAndWait tx
      case res of
        Right bl -> do
          logStoreLabeledMsg ro $ "Withdrawal submitted and confirmed: " <> show bl

          updateWallet walletNickname \wallet'@Wallet{_waSubmittedTransactions} -> do
            wallet'{_waSubmittedTransactions = SomeTxBody ScriptDataInBabbageEra txBody : _waSubmittedTransactions}
          pure $ C.getTxId txBody
        Left err ->
          throwLabeledError ro $ runtimeOperationFailed' $ "Failed to submit withdrawal: " <> show err
    Left err ->
      throwLabeledError ro $ runtimeOperationFailed' $ "Failed to create the withdrawal: " <> show err

interpret
  :: forall era env m st
   . (InterpretMonad env st m era)
  => RuntimeOperation
  -> m ()
interpret ro@RuntimeAwaitTxsConfirmed{..} = do
  let timeout = maybe defaultOperationTimeout A.toSecond roTimeout
  runtimeAwaitTxsConfirmed ro roContractNickname timeout
interpret ro@RuntimeWithdraw{..} = do
  ( contractNickname
    , contractInfo@ContractInfo{_ciContractId = contractId, _ciRoleCurrency = possibleRoleCurrecy, _ciMarloweThread = th}
    ) <-
    getContractInfo roContractNickname
  case possibleRoleCurrecy of
    Nothing ->
      throwLabeledError ro $
        testExecutionFailed' $
          "Unable to withdraw from contract: " <> show roContractNickname <> ". Role currency is not set."
    Just roleCurrency -> do
      allWallets <- findWalletsByCurrencyTokens roleCurrency Nothing
      let wallets = case roWallets of
            Nothing -> allWallets
            Just nicknames -> [(nickname, wallet, tokenNames) | (nickname, wallet, tokenNames) <- allWallets, nickname `elem` nicknames]

      for_ wallets \(walletNickname, wallet, tokenNames) -> do
        for_ tokenNames \tokenName -> do
          txId <- withdraw ro contractId tokenName walletNickname wallet
          let th' = anyMarloweThreadRedeemed (RuntimeTxInfo txId) tokenName th
          modifying knownContractsL $ \contracts -> do
            let contractInfo' = do
                  let c = fromMaybe contractInfo (Map.lookup contractNickname contracts)
                  c{_ciMarloweThread = th'}
            Map.insert contractNickname contractInfo' contracts

      case roAwaitConfirmed of
        Nothing -> pure ()
        Just timeout -> do
          runtimeAwaitTxsConfirmed ro roContractNickname (A.toSecond timeout)
interpret ro@RuntimeAwaitClosed{..} = do
  view executionModeL >>= skipInSimluationMode ro do
    (contractNickname, _) <- getContractInfo roContractNickname
    startMonitoring contractNickname
    rms <- getMonitorState
    logStoreLabeledMsg ro $ "Waiting till contract instance is closed: " <> show (coerce contractNickname :: String)
    timeout <- operationTimeoutLogged' ro
    let closedL :: RuntimeContractInfo -> Bool
        closedL = not . Marlowe.Cardano.Thread.isRunning . view rcMarloweThread

    res <- liftIO $ timeoutIO timeout $ atomically (awaitNonEmptyContractInfo rms contractNickname closedL)
    when (isNothing res) do
      let getRuntimeContractInfo = awaitRuntimeContractInfo rms contractNickname (const True)
      thread <- liftIO $ atomically getRuntimeContractInfo
      let contractState = maybe "<empty>" (Text.unpack . A.renderValue . anyMarloweThreadToJSON . view rcMarloweThread) thread
      throwLabeledError ro $
        timeoutReached' $
          "Timeout reached while waiting for contract instance to be closed: "
            <> show roContractNickname
            <> ". Contract info: "
            <> contractState
interpret ro@RuntimeCreateContract{..} = do
  let mkContractNickname = do
        contracts <- use knownContractsL
        case roContractNickname of
          Nothing -> do
            liftIO $ untilJust do
              suffix <- combinationWithRepetitions 8 ['a' .. 'z']
              let nickname = ContractNickname $ "contract-" <> suffix
              if isJust $ Map.lookup nickname contracts
                then pure Nothing
                else pure $ Just nickname
          Just nickname -> do
            when (isJust . Map.lookup nickname $ contracts) do
              throwLabeledError ro $ testExecutionFailed' "Contract with a given nickname already exist."
            pure nickname

  view executionModeL >>= skipInSimluationMode ro do
    Wallet{_waAddress, _waSigningKey} <- maybe getFaucet findWallet roSubmitter
    connector <- getConnector
    -- Verify that the role currency actually exists
    origContract <- case roContractSource of
      InlineContract json -> decodeContractJSON json
      UseTemplate setup -> useTemplate roRoleCurrency setup
    let possibleChangeAddress = toChainSyncAddress _waAddress
    changeAddress <-
      note
        (testExecutionFailed' "Failed to create change address")
        possibleChangeAddress

    contractNickname <- mkContractNickname

    logStoreLabeledMsg ro $ "Invoking contract creation: " <> show contractNickname
    logStoreMsgWith ro "Contract source:" [("source", toJSON origContract)]

    roleTokensConfig <- case roRoleCurrency of
      Just roleCurrency -> do
        Currency{ccPolicyId = cardanoPolicyId} <- findCurrency roleCurrency
        let policyId = MRCA.fromCardanoPolicyId cardanoPolicyId
        pure $ RoleTokensUsePolicy policyId
      Nothing -> pure RoleTokensNone

    let (contract, continuations) = case roMerkleize of
          Nothing -> (origContract, Nothing)
          Just RuntimeSide -> (origContract, Nothing)
          Just ClientSide ->
            let (c, cnt) = runWriter . deepMerkleize $ origContract
             in (c, Just cnt)

    result <- liftIO $ flip runMarloweT connector do
      let minLovelace = ChainSync.Lovelace roMinLovelace
          walletAddresses =
            WalletAddresses
              { changeAddress = changeAddress
              , extraAddresses = mempty
              , collateralUtxos = mempty
              }
      possibleRuntimeContract <-
        if roMerkleize == Just RuntimeSide
          then do
            possibleDatumHash <-
              Marlowe.Class.loadContract contract `catchError` \err -> do
                liftIO $ hPutStrLn stderr $ "Failed to load contract: " <> show err
                pure Nothing

            for possibleDatumHash \datumHash -> do
              pure $ Right datumHash
          else pure $ Just $ Left contract
      case possibleRuntimeContract of
        Just runtimeContract ->
          Bifunctor.first Just
            <$> Marlowe.Class.createContract
              Nothing
              MarloweV1
              walletAddresses
              roleTokensConfig
              emptyMarloweTransactionMetadata
              minLovelace
              runtimeContract
        Nothing -> pure (Left Nothing)
    era <- view eraL
    case result of
      Right Transaction.ContractCreated{txBody, contractId} -> do
        logStoreLabeledMsg ro $ "Creating contract: " <> show contractId
        let witness = somePaymentsigningKeyToTxWitness _waSigningKey
            tx = withShelleyBasedEra era . C.signShelleyTransaction txBody $ [witness]
            submitterNickname = fromMaybe faucetNickname roSubmitter
        res <- liftIO $ flip runMarloweT connector do
          Marlowe.Class.submitAndWait tx

        case res of
          Right _ -> do
            logStoreLabeledMsg ro $ "Contract created: " <> show tx
            let txId = C.getTxId txBody
                possibleTxIn = do
                  let R.ContractId utxo = contractId
                  RCA.toCardanoTxIn utxo
            case possibleTxIn of
              Nothing -> logStoreLabeledMsg ro $ "Failed to convert TxIx for a contract: " <> show contractId
              Just txIn -> do
                let runtimeTxInfo = RuntimeTxInfo txId
                    anyMarloweThread = anyMarloweThreadCreated runtimeTxInfo txIn
                    contractInfo =
                      ContractInfo
                        { _ciContractId = contractId
                        , _ciRoleCurrency = roRoleCurrency
                        , _ciMarloweThread = anyMarloweThread
                        , _ciContract = contract
                        , _ciContinuations = continuations
                        }
                modifying knownContractsL $ Map.insert contractNickname contractInfo
                updateWallet submitterNickname \submitter@Wallet{_waSubmittedTransactions} -> do
                  submitter{_waSubmittedTransactions = SomeTxBody ScriptDataInBabbageEra txBody : _waSubmittedTransactions}
                case roAwaitConfirmed of
                  Nothing -> pure ()
                  Just timeout -> do
                    runtimeAwaitTxsConfirmed ro roContractNickname $ A.toSecond timeout
          Left err ->
            throwLabeledError ro $ runtimeOperationFailed' $ "Failed to submit contract: " <> show err
      Left Nothing ->
        throwLabeledError ro $ runtimeOperationFailed' "Failed to load contract to the store."
      Left (Just err) ->
        throwLabeledError ro $ runtimeOperationFailed' $ "Failed to create contract: " <> show err
interpret ro@RuntimeApplyInputs{..} = do
  view executionModeL >>= skipInSimluationMode ro do
    Wallet{_waAddress, _waSigningKey} <- maybe getFaucet findWallet roSubmitter
    connector <- getConnector
    inputs <- for roInputs decodeInputJSON
    (contractNickname, _) <- getContractInfo roContractNickname
    contractId <- getContractId contractNickname
    let possibleChangeAddress = toChainSyncAddress _waAddress
    changeAddress <- note (testExecutionFailed' "Failed to create change address") possibleChangeAddress
    logStoreLabeledMsg ro $ "Applying inputs:" <> show inputs
    result <- liftIO $ flip runMarloweT connector do
      let walletAddresses =
            WalletAddresses
              { changeAddress = changeAddress
              , extraAddresses = mempty
              , collateralUtxos = mempty
              }
      Marlowe.Class.applyInputs' MarloweV1 walletAddresses contractId emptyMarloweTransactionMetadata Nothing Nothing inputs
    era <- view eraL
    case result of
      Right Transaction.InputsApplied{output = possibleMarloweOutput, txBody} -> do
        logStoreLabeledMsg ro "Successful application."
        let witness = somePaymentsigningKeyToTxWitness _waSigningKey
            tx = withShelleyBasedEra era . C.signShelleyTransaction txBody $ [witness]
            submitterNickname = fromMaybe faucetNickname roSubmitter

        logStoreLabeledMsg ro "Submitting..."
        res <- liftIO $ flip runMarloweT connector do
          Marlowe.Class.submitAndWait tx
        logStoreLabeledMsg ro "Submitted and confirmed."

        case res of
          Right bl -> do
            logStoreLabeledMsg ro $ "Inputs applied: " <> show bl
            knownContracts <- use knownContractsL
            case Map.lookup contractNickname knownContracts of
              Nothing -> throwLabeledError ro $ testExecutionFailed' ("Contract not found: " <> show contractNickname)
              Just contractInfo@ContractInfo{_ciMarloweThread = th} -> do
                let txId = C.getTxId txBody
                    possibleTxIx = do
                      R.TransactionScriptOutput{R.utxo = utxo} <- possibleMarloweOutput
                      C.TxIn _ txIx <- RCA.toCardanoTxIn utxo
                      pure txIx

                case anyRuntimeInterpreterMarloweThreadInputsApplied txId possibleTxIx inputs th of
                  Nothing ->
                    throwLabeledError ro $
                      testExecutionFailed' $
                        "Failed to extend the marlowe thread with the applied inputs: "
                          <> (Text.unpack . A.renderValue . anyMarloweThreadToJSON $ th)
                  Just th' -> do
                    modifying knownContractsL \contracts -> do
                      let contractInfo' = do
                            let c = fromMaybe contractInfo $ Map.lookup contractNickname contracts
                            c{_ciMarloweThread = th'}
                      Map.insert contractNickname contractInfo' contracts
            updateWallet submitterNickname \submitter@Wallet{_waSubmittedTransactions} -> do
              submitter{_waSubmittedTransactions = SomeTxBody ScriptDataInBabbageEra txBody : _waSubmittedTransactions}

            case roAwaitConfirmed of
              Nothing -> pure ()
              Just timeout -> do
                runtimeAwaitTxsConfirmed ro roContractNickname $ A.toSecond timeout
          Left err ->
            throwLabeledError ro $ runtimeOperationFailed' $ "Failed to submit contract: " <> show err
      Left err ->
        throwLabeledError ro $ runtimeOperationFailed' $ "Failed to apply input: " <> show err
