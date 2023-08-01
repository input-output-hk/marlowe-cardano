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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Marlowe.CLI.Test.CLI.Interpret where

import Language.Marlowe.CLI.Transaction (buildPayFromScript, findMarloweScriptsRefs, publishImpl, selectUtxosImpl)
import Language.Marlowe.CLI.Types (
  AnUTxO (..),
  CoinSelectionStrategy (CoinSelectionStrategy),
  MarloweScriptsRefs (..),
  MarloweTransaction (..),
  OutputQuery (..),
  OutputQueryResult (..),
  PayFromScript,
  PrintStats (PrintStats),
  PublishingStrategy (PublishAtAddress, PublishPermanently),
  ValidatorInfo (..),
  fromUTxO,
  toSlotRoundedPlutusPOSIXTime,
  unAnUTxO,
  validatorInfoScriptOrReference,
 )
import Language.Marlowe.Client qualified as Client
import Language.Marlowe.Core.V1.Semantics qualified as M
import Language.Marlowe.Core.V1.Semantics.Types qualified as M
import Language.Marlowe.Pretty (pretty)
import Plutus.V1.Ledger.Value qualified as P

import Cardano.Api (
  IsShelleyBasedEra,
  LocalNodeConnectInfo (LocalNodeConnectInfo),
  Lovelace (Lovelace),
  StakeAddressReference (NoStakeAddress),
 )
import Cardano.Api qualified as C
import Contrib.Control.Monad.Except (note)
import Contrib.Data.Foldable (anyFlipped, foldMapFlipped, foldMapMFlipped)
import Contrib.Data.List.Random (combinationWithRepetitions)
import Control.Lens (assign, coerced, modifying, use, view)
import Control.Monad (foldM, when)
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Lens (assign, coerced, modifying, use, view)
import Data.Foldable (fold)
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import Data.Map.Strict qualified as Map
import Data.Traversable (for)
import Language.Marlowe.CLI.Test.Wallet.Types (
  Currency (..),
  CurrencyNickname,
  SomeTxBody (..),
  Wallet (Wallet, _waAddress, _waBalanceCheckBaseline, _waSigningKey, _waSubmittedTransactions),
  WalletNickname (WalletNickname),
  faucetNickname,
 )
import Ledger.Orphans ()

import Contrib.Control.Monad.Except (note)
import Contrib.Data.Foldable (anyFlipped, foldMapFlipped, foldMapMFlipped)
import Contrib.Data.List.Random (combinationWithRepetitions)
import Control.Monad (foldM, void, when)
import Control.Monad.Error.Class (MonadError (throwError), liftEither)
import Data.Aeson qualified as A
import Data.Aeson.OneLine qualified as A
import Data.Bifunctor qualified as Bifunctor
import Data.Coerce (coerce)
import Data.Foldable (fold)
import Data.Foldable qualified as Foldable
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import Data.List.NonEmpty qualified as List.NonEmpty
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Text qualified as Text
import Data.Traversable (for)
import Language.Marlowe.CLI.Cardano.Api.PlutusScript (IsPlutusScriptLanguage)
import Language.Marlowe.CLI.Run (
  autoRunTransactionImpl,
  autoWithdrawFundsImpl,
  initializeTransactionImpl,
  initializeTransactionUsingScriptRefsImpl,
  marloweAddressFromCardanoAddress,
  marloweAddressToCardanoAddress,
  prepareTransactionImpl,
 )
import Language.Marlowe.CLI.Sync (classifyOutputs, isMarloweOut)
import Language.Marlowe.CLI.Sync.Types (MarloweOut (ApplicationOut, moTxIn))
import Language.Marlowe.CLI.Test.CLI.Monad (runCli, runLabeledCli)
import Language.Marlowe.CLI.Test.CLI.Types (
  CLIContractInfo (CLIContractInfo, _ciContract, _ciCurrency, _ciPlan, _ciSubmitter, _ciThread, _ciWithdrawalsCheckPoints),
  CLIContracts (CLIContracts),
  CLIOperation (..),
  InterpretMonad,
  MarloweValidators (..),
  anyCLIMarloweThreadInputsApplied,
  connectionL,
  contractsL,
  costModelParamsL,
  eraL,
  executionModeL,
  getCLIMarloweThreadTransaction,
  getCLIMarloweThreadTxBody,
  printStatsL,
  protocolVersionL,
  publishedScriptsL,
  slotConfigL,
 )
import Language.Marlowe.CLI.Test.Contract (ContractNickname (..))
import Language.Marlowe.CLI.Test.Contract.Source (Source (InlineContract, UseTemplate), useTemplate)
import Language.Marlowe.CLI.Test.ExecutionMode (
  ExecutionMode (OnChainMode, SimulationMode),
  skipInSimluationMode,
  toSubmitMode,
 )
import Language.Marlowe.CLI.Test.InterpreterError (rethrowCliError, testExecutionFailed')
import Language.Marlowe.CLI.Test.Log (logStoreLabeledMsg, logStoreMsgWith, throwLabeledError)
import Language.Marlowe.CLI.Test.Wallet.Interpret (
  decodeContractJSON,
  decodeInputJSON,
  fetchWalletValue,
  findWalletByUniqueToken,
  getCurrency,
  getFaucet,
  getSingletonCurrency,
  getWallet,
  getWalletByAddress,
  getWalletByUniqueToken,
  updateWallet,
 )
import Language.Marlowe.CLI.Test.Wallet.Types (
  Currency (Currency, ccCurrencySymbol),
  CurrencyNickname,
  SomeTxBody (..),
  Wallet (Wallet, _waAddress, _waBalanceCheckBaseline, _waSigningKey, _waSubmittedTransactions),
  WalletNickname (WalletNickname),
  faucetNickname,
 )
import Language.Marlowe.CLI.Types (
  AnUTxO (AnUTxO, unAnUTxO),
  CoinSelectionStrategy (CoinSelectionStrategy),
  MarlowePlutusVersion,
  MarloweScriptsRefs (MarloweScriptsRefs),
  MarloweTransaction (mtState),
  PrintStats (PrintStats),
  PublishingStrategy (PublishAtAddress, PublishPermanently),
  ValidatorInfo (ValidatorInfo),
  toSlotRoundedPlutusPOSIXTime,
 )
import Language.Marlowe.Cardano.Thread (
  anyMarloweThreadCreated,
  foldrMarloweThread,
  marloweThreadTxIn,
  overAnyMarloweThread,
 )
import Language.Marlowe.Core.V1.Semantics.Types (AccountId, State (..))
import Language.Marlowe.Extended.V1 (ada)
import Ledger.Tx.CardanoAPI (toCardanoAssetName)
import Plutus.V2.Ledger.Api qualified as P
import PlutusTx.AssocMap qualified as AM

-- | Build the initial Marlowe state.
initialMarloweState :: AccountId -> Integer -> State
initialMarloweState party minAda =
  State
    { accounts = AM.singleton (party, ada) minAda
    , choices = AM.empty
    , boundValues = AM.empty
    , minTime = 1
    }

findCLIContractInfo
  :: (InterpretMonad env st m lang era)
  => Maybe ContractNickname
  -> m (ContractNickname, CLIContractInfo lang era)
findCLIContractInfo (Just nickname) = do
  CLIContracts contracts <- use contractsL
  note
    ( testExecutionFailed' $
        "[findCLIContractInfo] Marlowe contract structure was not found for a given nickname "
          <> show (coerce nickname :: String)
          <> "."
    )
    ((nickname,) <$> Map.lookup nickname contracts)
findCLIContractInfo Nothing = do
  CLIContracts contracts <- use contractsL
  let fnName :: String
      fnName = "findCLIContractInfo"
  case Map.toList contracts of
    [] -> throwLabeledError fnName (testExecutionFailed' "No contracts found.")
    [pair] -> pure pair
    _ -> throwLabeledError fnName (testExecutionFailed' "Multiple contracts found. Please specify a contract nickname.")

toOneLineJSON :: forall a. (A.ToJSON a) => a -> String
toOneLineJSON = Text.unpack . A.renderValue . A.toJSON

autoRunTransaction
  :: forall era env lang m st
   . (IsShelleyBasedEra era)
  => (IsPlutusScriptLanguage lang)
  => (InterpretMonad env st m lang era)
  => Maybe CurrencyNickname
  -> WalletNickname
  -> Maybe (MarloweTransaction lang era, C.TxIn)
  -> MarloweTransaction lang era
  -> Bool
  -> m (C.TxBody era, Maybe C.TxIn)
autoRunTransaction currency defaultSubmitter prev curr@MarloweTransaction{..} invalid = do
  let log' = logStoreLabeledMsg ("autoRunTransaction" :: String)

      getInputContentParty = \case
        M.IDeposit _ party _ _ -> Just party
        M.IChoice (M.ChoiceId _ party) _ -> Just party
        M.INotify -> Nothing

      getInputParty = \case
        M.NormalInput inputContent -> getInputContentParty inputContent
        M.MerkleizedInput inputContent _ _ -> getInputContentParty inputContent

      step :: Maybe M.Party -> M.Input -> m (Maybe M.Party)
      step Nothing input = pure $ getInputParty input
      step reqParty input = case getInputParty input of
        Nothing -> pure reqParty
        reqParty'
          | reqParty /= reqParty' ->
              throwError $
                testExecutionFailed' $
                  "[autoRunTransaction] can handle only inputs which can be executed by a single party: "
                    <> show reqParty
                    <> " /= "
                    <> show reqParty'
        _ -> pure reqParty

  logStoreMsgWith
    ("autoRunTransaction" :: String)
    "Applying marlowe inputs: "
    [ ("inputs", A.toJSON mtInputs)
    , ("state", A.toJSON mtState)
    , ("contract", A.toJSON mtContract)
    ]

  connection <- view connectionL
  era <- view eraL
  (submitterNickname, Wallet address _ skey _, possiblePayFromOpenRole) <-
    foldM step Nothing mtInputs >>= \case
      Nothing -> do
        wallet <- getWallet defaultSubmitter
        pure (defaultSubmitter, wallet, Nothing)
      Just (M.Address network address) -> do
        addr <- rethrowCliError (marloweAddressToCardanoAddress network address)
        (wn, w) <- getWalletByAddress addr
        pure (wn, w, Nothing)
      Just (M.Role rn) -> case currency of
        Just cn ->
          findWalletByUniqueToken cn rn >>= \case
            --  We have not found owner of the token. It seems that it should belong to open role validator.
            --  So we need to:
            --    * search for utxo under open role script which contains role token
            --    * if found, use it as an extra input
            --    * extract reference input to the open role script from the context and pass it up
            Left err -> do
              -- assign publishedScriptsL (Just marloweScriptRefs)
              Currency{ccPolicyId = policyId} <- getCurrency cn
              use publishedScriptsL >>= \case
                Nothing ->
                  throwError $
                    testExecutionFailed' $
                      "[autoRunTransaction] Contract requires role token but open role script was not published yet "
                        <> "and role token is not possed the open role script and was not found between wallets: "
                        <> err
                Just MarloweScriptsRefs{mrOpenRoleValidator = (_, openRoleValidatorInfo)} -> do
                  roleAssetName <-
                    liftEither $ Bifunctor.first (const $ testExecutionFailed' "TokenName conversion error") (toCardanoAssetName rn)
                  let openRoleAddress = viAddress openRoleValidatorInfo
                      roleTokenAssetId = C.AssetId policyId roleAssetName
                  runCli era "[AutoRun]" (selectUtxosImpl connection openRoleAddress (AssetOnly roleTokenAssetId)) >>= \case
                    OutputQueryResult{oqrMatching = fromUTxO -> (AnUTxO (txIn, _) : _)} -> do
                      let scriptOrReference = validatorInfoScriptOrReference openRoleValidatorInfo
                          datum = P.Datum $ P.toBuiltinData ()
                          redeemer = P.Redeemer $ P.toBuiltinData ()

                          payFromOpenRole :: PayFromScript lang
                          payFromOpenRole = buildPayFromScript scriptOrReference datum redeemer txIn

                      wallet <- getWallet defaultSubmitter
                      pure (defaultSubmitter, wallet, Just payFromOpenRole)
                    _ ->
                      throwError $
                        testExecutionFailed'
                          "[autoRunTransaction] Contract requires role token but neither open role script or wallet contains it."
            Right (wn, w) -> do
              pure (wn, w, Nothing)
        Nothing -> throwError $ testExecutionFailed' "[autoRunTransaction] Contract requires a role currency which was not specified."

  log' $ "Submitter: " <> case submitterNickname of WalletNickname n -> n

  submitMode <- view executionModeL <&> toSubmitMode
  txBody <-
    runCli era "[AutoRun] " $
      autoRunTransactionImpl
        connection
        prev
        curr
        (Foldable.toList possiblePayFromOpenRole)
        address
        [skey]
        C.TxMetadataNone
        submitMode
        True
        invalid

  log' $ "TxBody:" <> show txBody
  let C.TxBody C.TxBodyContent{..} = txBody
      mTxId = C.getTxId txBody

  log' $ "TxId:" <> show mTxId

  updateWallet submitterNickname \submitter@Wallet{..} ->
    submitter{_waSubmittedTransactions = SomeTxBody era txBody : _waSubmittedTransactions}

  let meOuts = classifyOutputs mTxId txOuts
  case filter isMarloweOut meOuts of
    [ApplicationOut{moTxIn}] -> do
      log' $ "Marlowe output:" <> show moTxIn
      pure (txBody, Just moTxIn)
    [] -> pure (txBody, Nothing)
    _ ->
      throwError $ testExecutionFailed' "[AutoRun] Multiple Marlowe outputs detected - unable to handle them yet."

buildParty
  :: (InterpretMonad env st m lang era)
  => Maybe CurrencyNickname
  -> PartyRef
  -> m M.Party
buildParty mRoleCurrency = \case
  WalletRef nickname -> do
    wallet <- getWallet nickname
    uncurry M.Address <$> rethrowCliError (marloweAddressFromCardanoAddress (_waAddress wallet))
  RoleRef token -> do
    -- Cosistency check
    currency <- case mRoleCurrency of
      Nothing -> fst <$> getSingletonCurrency
      Just cn -> pure cn
    void $ getWalletByUniqueToken currency token
    -- We are allowed to use this M.Role
    pure $ M.Role token

publishCurrentValidators
  :: forall env era lang st m
   . (C.IsShelleyBasedEra era)
  => (IsPlutusScriptLanguage lang)
  => (InterpretMonad env st m lang era)
  => Maybe Bool
  -> Maybe WalletNickname
  -> m (MarloweScriptsRefs lang era)
publishCurrentValidators publishPermanently possiblePublisher = do
  Wallet{_waAddress, _waSigningKey} <- maybe getFaucet getWallet possiblePublisher
  let publishingStrategy = case publishPermanently of
        Just True -> PublishPermanently NoStakeAddress
        _ -> PublishAtAddress _waAddress
  connection <- view connectionL
  printStats <- view printStatsL
  era <- view eraL
  let fnName :: String
      fnName = "publishCurrentValidators"
      logTraceMsg' = logStoreLabeledMsg fnName
  runCli era fnName (findMarloweScriptsRefs @lang connection publishingStrategy printStats) >>= \case
    Just marloweScriptRefs@(MarloweScriptsRefs (AnUTxO (mTxIn, _), mv) (AnUTxO (pTxIn, _), pv) (AnUTxO (orTxIn, _), orv)) -> do
      let logValidatorInfo ValidatorInfo{..} =
            logTraceMsg' $ Text.unpack (C.serialiseAddress viAddress)

      logTraceMsg' "Found already published scripts so using them."
      logTraceMsg' $ "Marlowe reference: " <> show mTxIn
      logValidatorInfo mv
      logTraceMsg' $ "Payout reference: " <> show pTxIn
      logValidatorInfo pv
      logTraceMsg' $ "Open role reference: " <> show orTxIn
      logValidatorInfo orv
      pure marloweScriptRefs
    Nothing ->
      view executionModeL >>= \case
        SimulationMode -> throwLabeledError fnName $ testExecutionFailed' "Can't perform on chain script publishing in simulation mode"
        OnChainMode timeout -> do
          logStoreLabeledMsg fnName "Scripts not found so publishing them."
          runCli era fnName $
            publishImpl
              connection
              _waSigningKey
              Nothing
              _waAddress
              publishingStrategy
              (CoinSelectionStrategy False False [])
              timeout
              (PrintStats True)

interpret
  :: forall env era lang st m
   . (C.IsShelleyBasedEra era)
  => (IsPlutusScriptLanguage lang)
  => (InterpretMonad env st m lang era)
  => CLIOperation
  -> m ()
interpret co@Initialize{..} = do
  let submitterNickname = fromMaybe faucetNickname coSubmitter
      merkleize = fromMaybe False coMerkleize

  marloweContract <- case coContractSource of
    InlineContract json -> decodeContractJSON json
    UseTemplate setup -> useTemplate coRoleCurrency setup

  logStoreLabeledMsg co $ "Contract: " <> show (pretty marloweContract)

  let submitterNickname = fromMaybe faucetNickname coSubmitter
  address <- _waAddress <$> getWallet submitterNickname
  submitterParty <- uncurry M.Address <$> rethrowCliError (marloweAddressFromCardanoAddress address)

  currencySymbol <- case coRoleCurrency of
    Nothing -> pure P.adaSymbol
    Just nickname -> do
      Currency{ccCurrencySymbol} <- getCurrency nickname
      pure ccCurrencySymbol

  let Lovelace minAda = coMinLovelace
      marloweState = initialMarloweState submitterParty minAda
      marloweParams = Client.marloweParams currencySymbol

  era <- view eraL
  slotConfig <- view slotConfigL
  costModelParams <- view costModelParamsL
  protocolVersion <- view protocolVersionL
  LocalNodeConnectInfo{localNodeNetworkId} <- view connectionL

  let marloweValidators = fromMaybe (ReferenceCurrentValidators Nothing Nothing) coMarloweValidators
      mkContractNickname = do
        CLIContracts contracts <- use contractsL
        case coContractNickname of
          Nothing -> do
            liftIO $ untilJust do
              suffix <- combinationWithRepetitions 8 ['a' .. 'z']
              let nickname = ContractNickname $ "contract-" <> suffix
              if isJust $ Map.lookup nickname contracts
                then pure Nothing
                else pure $ Just nickname
          Just nickname -> do
            when (isJust . Map.lookup nickname $ contracts) do
              throwLabeledError co $ testExecutionFailed' "Contract with a given nickname already exist."
            pure nickname

  marloweTransaction <- case marloweValidators of
    ReferenceCurrentValidators publishPermanently possiblePublisher -> do
      logStoreLabeledMsg co "Using reference scripts to the current Marlowe validator."
      refs <-
        use publishedScriptsL >>= \case
          Nothing -> do
            logStoreLabeledMsg co "Publishing the scripts."
            marloweScriptRefs <- publishCurrentValidators publishPermanently possiblePublisher
            assign publishedScriptsL (Just marloweScriptRefs)
            pure marloweScriptRefs
          Just refs -> do
            logStoreLabeledMsg co "Scripts were already published so using them."
            pure refs

      runLabeledCli era co $
        initializeTransactionUsingScriptRefsImpl
          marloweParams
          slotConfig
          refs
          NoStakeAddress
          marloweContract
          marloweState
          merkleize
          True
    InTxCurrentValidators -> do
      logStoreLabeledMsg co "Using in Tx scripts embeding strategy to initialize Marlowe contract."
      runLabeledCli era co $
        initializeTransactionImpl
          marloweParams
          slotConfig
          protocolVersion
          costModelParams
          localNodeNetworkId
          NoStakeAddress
          marloweContract
          marloweState
          Nothing
          merkleize
          True
    ReferenceRuntimeValidators -> do
      throwLabeledError co $ testExecutionFailed' "Usage of reference runtime scripts is not supported yet."

  contractNickname <- mkContractNickname

  logStoreLabeledMsg co $ "Saving initialized contract " <> show (coerce contractNickname :: String)
  modifying (contractsL . coerced) $
    Map.insert contractNickname $
      CLIContractInfo
        { _ciContract = marloweContract
        , _ciPlan = marloweTransaction :| []
        , _ciThread = Nothing
        , _ciWithdrawalsCheckPoints = mempty
        , _ciCurrency = coRoleCurrency
        , _ciSubmitter = submitterNickname
        }
interpret co@Prepare{..} = do
  (contractNickname, marloweContract@CLIContractInfo{..}) <- findCLIContractInfo coContractNickname
  let curr = List.NonEmpty.head _ciPlan
  inputs <- for coInputs decodeInputJSON
  slotConfig <- view slotConfigL
  minimumTime <- toSlotRoundedPlutusPOSIXTime slotConfig coMinimumTime
  maximumTime <- toSlotRoundedPlutusPOSIXTime slotConfig coMaximumTime
  era <- view eraL
  logStoreLabeledMsg co $ "Inputs: " <> show (pretty inputs)
  new <-
    runLabeledCli era co $
      prepareTransactionImpl
        curr
        inputs
        minimumTime
        maximumTime
        True

  let new' = case coOverrideMarloweState of
        Just customState -> new{mtState = customState}
        Nothing -> new
      plan = new' <| _ciPlan
      marloweContract' = marloweContract{_ciPlan = plan}

  modifying (contractsL . coerced) $ Map.insert contractNickname marloweContract'
interpret co@Publish{..} = do
  logStoreLabeledMsg co "Using reference scripts to the current Marlowe validator."
  use publishedScriptsL >>= \case
    Nothing -> do
      logStoreLabeledMsg co "Publishing the scripts."
      marloweScriptRefs <- publishCurrentValidators coPublishPermanently coPublisher
      assign publishedScriptsL (Just marloweScriptRefs)
    Just _ -> do
      throwLabeledError co $ testExecutionFailed' "Scripts were already published during this test scenario."
interpret AutoRun{..} = do
  executionMode <- view executionModeL
  case executionMode of
    OnChainMode{} -> do
      (contractNickname, marloweContract@CLIContractInfo{..}) <- findCLIContractInfo coContractNickname
      let plan = do
            let whole = reverse $ List.NonEmpty.toList _ciPlan
                threadLength = foldrMarloweThread (const (+ 1)) 0
            case _ciThread of
              Just thread -> do
                let l = overAnyMarloweThread threadLength thread
                drop l whole
              Nothing -> whole
          step mTh mt = do
            let prev :: Maybe (MarloweTransaction lang era, C.TxIn)
                prev = do
                  pmt <- overAnyMarloweThread getCLIMarloweThreadTransaction <$> mTh
                  txIn <- overAnyMarloweThread marloweThreadTxIn =<< mTh
                  pure (pmt, txIn)
                invalid = Just True == coInvalid
                defaultSubmitter = fromMaybe _ciSubmitter coSubmitter

            (txBody, mTxIn) <- autoRunTransaction _ciCurrency defaultSubmitter prev mt invalid
            case (mTh, mTxIn) of
              (Nothing, Nothing) -> throwError $ testExecutionFailed' "[AutoRun] Creation of the Marlowe thread failed."
              (Nothing, Just txIn) -> pure $ Just $ anyMarloweThreadCreated (mt, txBody) txIn
              (Just th, _) -> case anyCLIMarloweThreadInputsApplied (mt, txBody) mTxIn th of
                Just th' -> pure $ Just th'
                Nothing -> throwError $ testExecutionFailed' "[AutoRun] Extending of the Marlowe thread failed."

      thread' <- foldM step _ciThread plan
      let marloweContract' = marloweContract{_ciThread = thread'}
      modifying (contractsL . coerced) $ Map.insert contractNickname marloweContract'
    SimulationMode -> do
      -- TODO: We should be able to run balancing in here even in the simulation mode
      pure ()
interpret co@Withdraw{..} =
  view executionModeL >>= skipInSimluationMode co do
    (contractNickname, marloweContract@CLIContractInfo{..}) <- findCLIContractInfo coContractNickname

    marloweThread <- case _ciThread of
      Just marloweThread -> pure marloweThread
      Nothing -> throwLabeledError co $ testExecutionFailed' "Contract is not on the chain yet so there are not payouts as well."
    wallet@Wallet{_waAddress, _waSigningKey} <- getWallet coWalletNickname
    Currency{ccCurrencySymbol} <- maybe (snd <$> getSingletonCurrency) getCurrency _ciCurrency
    onChainValue <- fetchWalletValue wallet
    let roles =
          P.flattenValue onChainValue `foldMapFlipped` \(cs, tn, _) ->
            if cs == ccCurrencySymbol
              then [tn]
              else mempty
    when (roles == mempty) $
      throwLabeledError co $
        testExecutionFailed' $
          fold
            [ "Provided wallet "
            , show coWalletNickname
            , "has no roles associated with the given contract "
            , show coContractNickname
            ]

    submitMode <- view executionModeL <&> toSubmitMode
    connection <- view connectionL
    txBodies <- foldMapMFlipped roles \role -> do
      let lastWithdrawalCheckPoint = Map.lookup role _ciWithdrawalsCheckPoints
          threadTransactions :: [(MarloweTransaction lang era, C.TxId)]
          threadTransactions = do
            let step item acc = (getCLIMarloweThreadTransaction item, C.getTxId . getCLIMarloweThreadTxBody $ item) : acc
            overAnyMarloweThread (foldrMarloweThread step []) marloweThread

          possibleWithdrawals = takeWhile ((/=) lastWithdrawalCheckPoint . Just . snd) threadTransactions

          paymentRole (M.Payment _ (M.Party (M.Role r)) _ _) = Just r
          paymentRole _ = Nothing

          -- Sometimes we reuse the same currency across multiple tests (when Faucet is an issuer) so we
          -- need to filter out payouts which are really associated with this particular test
          -- case. We can identify them by matching them against a set of submitted transaction ids.
          filterPayoutsUTxOs utxos = do
            let txIds = map snd possibleWithdrawals
                txInId (C.TxIn txId _) = txId
            filter (flip elem txIds . txInId . fst . unAnUTxO) utxos

      let anyWithdrawalsExist =
            possibleWithdrawals `anyFlipped` \(MarloweTransaction{..}, _) ->
              elem role . mapMaybe paymentRole $ mtPayments

      if anyWithdrawalsExist
        then do
          let roleToken = M.Token ccCurrencySymbol role
              MarloweTransaction{mtRoleValidator} :| _ = _ciPlan

          logStoreLabeledMsg co $
            "Withdrawing funds for role " <> show role <> " after application of inputs: " <> do
              let inputs = foldMapFlipped possibleWithdrawals \(MarloweTransaction{mtInputs}, _) -> mtInputs
              show inputs
          era <- view eraL
          txBody <-
            runLabeledCli era co $
              autoWithdrawFundsImpl
                connection
                roleToken
                mtRoleValidator
                Nothing
                _waAddress
                [_waSigningKey]
                (Just filterPayoutsUTxOs)
                C.TxMetadataNone
                submitMode
                (PrintStats True)
                False
          pure [txBody]
        else pure []

    era <- view eraL
    updateWallet coWalletNickname \w@Wallet{_waSubmittedTransactions} ->
      w{_waSubmittedTransactions = map (SomeTxBody era) txBodies <> _waSubmittedTransactions}

    let newWithdrawals = foldMapFlipped roles \role ->
          Map.singleton role (C.getTxId . overAnyMarloweThread getCLIMarloweThreadTxBody $ marloweThread)
        marloweContract' = marloweContract{_ciWithdrawalsCheckPoints = newWithdrawals <> _ciWithdrawalsCheckPoints}
    modifying (contractsL . coerced) $ Map.insert contractNickname marloweContract'
