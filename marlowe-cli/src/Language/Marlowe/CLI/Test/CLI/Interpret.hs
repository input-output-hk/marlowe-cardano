{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
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
{-# LANGUAGE TupleSections #-}

module Language.Marlowe.CLI.Test.CLI.Interpret
  where

import Language.Marlowe.CLI.Transaction (findMarloweScriptsRefs, publishImpl)
import qualified Language.Marlowe.CLI.Types as T
import qualified Language.Marlowe.Client as Client
import qualified Language.Marlowe.Core.V1.Semantics as M
import qualified Language.Marlowe.Core.V1.Semantics.Types as M
import Language.Marlowe.Pretty (pretty)
import qualified Plutus.V1.Ledger.Value as P

import Cardano.Api
  ( IsShelleyBasedEra
  , LocalNodeConnectInfo(LocalNodeConnectInfo)
  , Lovelace(Lovelace)
  , StakeAddressReference(NoStakeAddress)
  )
import qualified Cardano.Api as C
import Control.Lens (assign, coerced, modifying, use, view)
import Data.Foldable (fold)
import Data.List.NonEmpty (NonEmpty((:|)), (<|))
import qualified Data.Map.Strict as Map
import Data.Traversable (for)
import Language.Marlowe.CLI.Test.Wallet.Types
  ( Currency(Currency, ccCurrencySymbol)
  , CurrencyNickname
  , SomeTxBody(..)
  , Wallet(Wallet, waAddress, waBalanceCheckBaseline, waMintedTokens, waSigningKey, waSubmittedTransactions)
  , WalletNickname(WalletNickname)
  , faucetNickname
  )
import Language.Marlowe.CLI.Types
  ( AnUTxO(AnUTxO, unAnUTxO)
  , CliError(CliError)
  , CoinSelectionStrategy(CoinSelectionStrategy)
  , MarlowePlutusVersion
  , MarloweScriptsRefs(MarloweScriptsRefs)
  , MarloweTransaction(mtState)
  , PrintStats(PrintStats)
  , PublishingStrategy(PublishAtAddress, PublishPermanently)
  , ValidatorInfo(ValidatorInfo)
  , toPOSIXTime
  )
import Ledger.Orphans ()

import Contrib.Data.Foldable (anyFlipped, foldMapFlipped, foldMapMFlipped)
import Control.Monad (foldM, void, when)
import Control.Monad.Error.Class (MonadError(throwError))
import Data.Coerce (coerce)
import Data.Functor ((<&>))
import qualified Data.List.NonEmpty as List.NonEmpty
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import qualified Data.Text as Text
import Language.Marlowe.CLI.Cardano.Api.PlutusScript (IsPlutusScriptLanguage)
import Language.Marlowe.CLI.Command.Template (initialMarloweState)
import Language.Marlowe.CLI.IO (liftCliMaybe)
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
import Language.Marlowe.CLI.Test.CLI.Monad (runCli, runLabeledCli, runTraceCli)
import Language.Marlowe.CLI.Test.CLI.Types
  ( CLIContractInfo(CLIContractInfo, ciContract, ciCurrency, ciPlan, ciSubmitter, ciThread, ciWithdrawalsCheckPoints)
  , CLIContracts(CLIContracts)
  , CLIOperation(..)
  , InterpretMonad
  , MarloweValidators(..)
  , PartyRef(RoleRef, WalletRef)
  , anyCLIMarloweThread
  , connectionL
  , contractsL
  , costModelParamsL
  , eraL
  , executionModeL
  , getCLIMarloweThreadTransaction
  , getCLIMarloweThreadTxBody
  , printStatsL
  , protocolVersionL
  , publishedScriptsL
  , slotConfigL
  )
import Language.Marlowe.CLI.Test.Contract (ContractNickname(..))
import Language.Marlowe.CLI.Test.Contract.Source (Source(InlineContract, UseTemplate), useTemplate)
import Language.Marlowe.CLI.Test.ExecutionMode
  (ExecutionMode(OnChainMode, SimulationMode), skipInSimluationMode, toSubmitMode)
import Language.Marlowe.CLI.Test.Log (logLabeledMsg, logTraceMsg, throwLabeledError, throwTraceError)
import Language.Marlowe.CLI.Test.Wallet.Interpret
  ( decodeContractJSON
  , decodeInputJSON
  , findCurrency
  , findWallet
  , findWalletByAddress
  , findWalletByUniqueToken
  , getFaucet
  , getSingletonCurrency
  , updateWallet
  )
import Language.Marlowe.Cardano.Thread
  (anyMarloweThreadCreated, foldrMarloweThread, marloweThreadTxIn, overAnyMarloweThread)

findCLIContractInfo
  :: InterpretMonad env st m lang era
  => ContractNickname
  -> m (CLIContractInfo lang era)
findCLIContractInfo nickname = do
  CLIContracts contracts <- use contractsL
  liftCliMaybe
    ("[findCLIContractInfo] Marlowe contract structure was not found for a given nickname " <> show (coerce nickname :: String) <> ".")
    $ Map.lookup nickname contracts

autoRunTransaction :: forall era env lang m st
                    . IsShelleyBasedEra era
                   => IsPlutusScriptLanguage lang
                   => InterpretMonad env st m lang era
                   => Maybe CurrencyNickname
                   -> WalletNickname
                   -> Maybe (MarloweTransaction lang era, C.TxIn)
                   -> MarloweTransaction lang era
                   -> Bool
                   -> m (C.TxBody era, Maybe C.TxIn)
autoRunTransaction currency defaultSubmitter prev curr@T.MarloweTransaction {..} invalid = do
  let
    log' = logTraceMsg "autoRunTransaction"

    getNormalInputParty = \case
      M.IDeposit _ party _ _         -> Just party
      M.IChoice (M.ChoiceId _ party) _ -> Just party
      M.INotify                      -> Nothing

    getInputParty :: Maybe M.Party -> M.Input -> m (Maybe M.Party)
    getInputParty Nothing (M.NormalInput input) = pure $ getNormalInputParty input
    getInputParty reqParty (M.NormalInput input) = case getNormalInputParty input of
      Nothing -> pure reqParty
      reqParty' | reqParty /= reqParty' ->
        throwError . CliError $
          "[autoRunTransaction] can handle only inputs which can be executed by a single party: " <> show reqParty <> " /= " <> show reqParty'
      _ -> pure reqParty
    getInputParty _ M.MerkleizedInput {} =
      throwError "[autoRunTransaction] merkleized input handling is not implemented yet."

  log' $ "Applying marlowe inputs: " <> show mtInputs
  log' $ "Output contract: " <> show (pretty mtContract)
  log' $ "Output state: " <> show mtState

  (submitterNickname, Wallet address _ _ skey _) <- foldM getInputParty Nothing mtInputs >>= \case
    Nothing                          -> (defaultSubmitter,) <$> findWallet defaultSubmitter
    Just (M.Address network address) -> (findWalletByAddress =<< marloweAddressToCardanoAddress network address)
    Just (M.Role rn)                 -> case currency of
      Just cn -> findWalletByUniqueToken cn rn
      Nothing -> throwError "[autoRunTransaction] Contract requires a role currency which was not specified."

  log' $ "Submitter: " <> case submitterNickname of WalletNickname n -> n

  connection <- view connectionL
  submitMode <- view executionModeL <&> toSubmitMode
  era <- view eraL
  txBody <- runCli era "[AutoRun] " $ autoRunTransactionImpl
      connection
      prev
      curr
      address
      [skey]
      C.TxMetadataNone
      submitMode
      True
      invalid

  log' $ "TxBody:" <> show txBody
  let
    C.TxBody C.TxBodyContent{..} = txBody
    mTxId = C.getTxId txBody

  log' $ "TxId:" <> show mTxId

  updateWallet submitterNickname \submitter@Wallet {..} ->
    submitter { waSubmittedTransactions = SomeTxBody txBody : waSubmittedTransactions }

  let meOuts = classifyOutputs mTxId txOuts
  case filter isMarloweOut meOuts of
    [ApplicationOut {moTxIn}] -> do
      log' $ "Marlowe output:" <> show moTxIn
      pure (txBody, Just moTxIn)
    []                        -> pure (txBody, Nothing)
    _                         -> throwError "[AutoRun] Multiple Marlowe outputs detected - unable to handle them yet."

buildParty
  :: InterpretMonad env st m lang era
  => Maybe CurrencyNickname
  -> PartyRef
  -> m M.Party
buildParty mRoleCurrency = \case
  WalletRef nickname -> do
      wallet <- findWallet nickname
      uncurry M.Address <$> marloweAddressFromCardanoAddress (waAddress wallet)
  RoleRef token -> do
    -- Cosistency check
    currency <- case mRoleCurrency of
      Nothing -> fst <$> getSingletonCurrency
      Just cn -> pure cn
    void $ findWalletByUniqueToken currency token
    -- We are allowed to use this M.Role
    pure $ M.Role token

publishCurrentValidators
  :: forall env era lang st m
   . C.IsShelleyBasedEra era
  => InterpretMonad env st m lang era
  => Maybe Bool
  -> Maybe WalletNickname
  -> m (MarloweScriptsRefs MarlowePlutusVersion era)
publishCurrentValidators publishPermanently possiblePublisher = do
  Wallet { waAddress, waSigningKey } <- maybe getFaucet findWallet possiblePublisher
  let
    publishingStrategy = case publishPermanently of
      Just True -> PublishPermanently NoStakeAddress
      _         -> PublishAtAddress waAddress
  connection <- view connectionL
  printStats <- view printStatsL
  era <- view eraL
  let
    fnName = "publishCurrentValidators"
    logTraceMsg' = logTraceMsg fnName
  runTraceCli era fnName (findMarloweScriptsRefs connection publishingStrategy printStats) >>= \case
    Just marloweScriptRefs@(MarloweScriptsRefs (AnUTxO (mTxIn, _), mv) (AnUTxO (pTxIn, _), pv)) -> do
      let
        logValidatorInfo ValidatorInfo {..} =
          logTraceMsg' $ Text.unpack (C.serialiseAddress viAddress)

      logTraceMsg' "Found already published scripts so using them."
      logTraceMsg' $ "Marlowe reference: " <> show mTxIn
      logValidatorInfo mv
      logTraceMsg' $ "Payout reference: " <> show pTxIn
      logValidatorInfo pv
      pure marloweScriptRefs

    Nothing -> view executionModeL >>= \case
      SimulationMode -> throwTraceError fnName "Can't perform on chain script publishing in simulation mode"
      OnChainMode timeout -> do
        logTraceMsg fnName "Scripts not found so publishing them."
        runTraceCli era fnName $ publishImpl
          connection
          waSigningKey
          Nothing
          waAddress
          publishingStrategy
          (CoinSelectionStrategy False False [])
          timeout
          (PrintStats True)

interpret
  :: forall env era lang st m
   . C.IsShelleyBasedEra era
  => IsPlutusScriptLanguage lang
  => InterpretMonad env st m lang era
  => CLIOperation
  -> m ()
interpret co@Initialize {..} = do
  marloweContract <- case coContractSource of
    InlineContract json -> decodeContractJSON json
    UseTemplate setup   -> useTemplate coRoleCurrency setup

  logLabeledMsg co $ "Contract: " <> show (pretty marloweContract)

  let
    submitterNickname = fromMaybe faucetNickname coSubmitter
  address <- waAddress <$> findWallet submitterNickname
  submitterParty <- uncurry M.Address <$> marloweAddressFromCardanoAddress address

  currencySymbol <- case coRoleCurrency of
    Nothing -> pure P.adaSymbol
    Just nickname -> do
      Currency { ccCurrencySymbol } <- findCurrency nickname
      pure ccCurrencySymbol

  let
    Lovelace minAda = coMinLovelace
    marloweState = initialMarloweState submitterParty minAda
    marloweParams = Client.marloweParams currencySymbol

  era <- view eraL
  slotConfig <- view slotConfigL
  costModelParams <- view costModelParamsL
  protocolVersion <- view protocolVersionL
  LocalNodeConnectInfo { localNodeNetworkId } <- view connectionL

  marloweTransaction <- case coMarloweValidators of
    ReferenceCurrentValidators publishPermanently possiblePublisher -> do
      logLabeledMsg co "Using reference scripts to the current Marlowe validator."
      refs <- use publishedScriptsL >>= \case
        Nothing -> do
          logLabeledMsg co "Publishing the scripts."
          marloweScriptRefs <- publishCurrentValidators publishPermanently possiblePublisher
          assign publishedScriptsL (Just marloweScriptRefs)
          pure marloweScriptRefs
        Just refs -> do
          logLabeledMsg co "Scripts were already published so using them."
          pure refs

      runLabeledCli era co $ initializeTransactionUsingScriptRefsImpl
        marloweParams
        slotConfig
        refs
        NoStakeAddress
        marloweContract
        marloweState
        False
        True

    InTxCurrentValidators -> do
      logLabeledMsg co "Using in Tx scripts embeding strategy to initialize Marlowe contract."
      runLabeledCli era co $ initializeTransactionImpl
        marloweParams
        slotConfig
        protocolVersion
        costModelParams
        localNodeNetworkId
        NoStakeAddress
        marloweContract
        marloweState
        Nothing
        False
        True

    ReferenceRuntimeValidators -> do
      throwLabeledError co "Usage of reference runtime scripts is not supported yet."

  CLIContracts contracts <- use contractsL
  when (isJust . Map.lookup coContractNickname $ contracts) do
    throwLabeledError co "Contract with a given nickname already exist."

  logLabeledMsg co $ "Saving initialized contract " <> show (coerce coContractNickname :: String)
  modifying (contractsL . coerced) $ Map.insert coContractNickname $ CLIContractInfo
    {
      ciContract = marloweContract
    , ciPlan = marloweTransaction :| []
    , ciThread = Nothing
    , ciWithdrawalsCheckPoints = mempty
    , ciCurrency = coRoleCurrency
    , ciSubmitter = submitterNickname
    }

interpret co@Prepare {..} = do
  marloweContract@CLIContractInfo {..} <- findCLIContractInfo coContractNickname
  let
    curr = List.NonEmpty.head ciPlan
  inputs <- for coInputs decodeInputJSON
  minimumTime <- toPOSIXTime coMinimumTime
  maximumTime <- toPOSIXTime coMaximumTime
  era <- view eraL
  logLabeledMsg co $ "Inputs: " <> show (pretty inputs)
  new <- runLabeledCli era co $ prepareTransactionImpl
    curr
    inputs
    minimumTime
    maximumTime
    True

  let
    new' = case coOverrideMarloweState of
      Just customState -> new { mtState = customState }
      Nothing          -> new
    plan = new' <| ciPlan
    marloweContract' = marloweContract{ ciPlan = plan }

  modifying (contractsL . coerced) $ Map.insert coContractNickname marloweContract'

interpret co@Publish {..} = do
  logLabeledMsg co "Using reference scripts to the current Marlowe validator."
  use publishedScriptsL >>= \case
    Nothing -> do
      logLabeledMsg co "Publishing the scripts."
      marloweScriptRefs <- publishCurrentValidators coPublishPermanently coPublisher
      assign publishedScriptsL (Just marloweScriptRefs)
    Just _ -> do
      throwLabeledError co "Scripts were already published during this test scenario."

interpret AutoRun {..} = do
  executionMode <- view executionModeL
  case executionMode of
    OnChainMode {} -> do
      marloweContract@CLIContractInfo {..} <- findCLIContractInfo coContractNickname
      let
        plan = do
          let
            whole = reverse $ List.NonEmpty.toList ciPlan
            threadLength = foldrMarloweThread (const (+ 1)) 0
          case ciThread of
            Just thread -> do
              let
                l = overAnyMarloweThread threadLength thread
              drop l whole
            Nothing -> whole
        step mTh mt = do
          let
            prev :: Maybe (MarloweTransaction lang era, C.TxIn)
            prev = do
              pmt <- overAnyMarloweThread getCLIMarloweThreadTransaction <$> mTh
              txIn <- overAnyMarloweThread marloweThreadTxIn =<< mTh
              pure (pmt, txIn)
            invalid = Just True == coInvalid

          (txBody, mTxIn) <- autoRunTransaction ciCurrency ciSubmitter prev mt invalid
          case (mTh, mTxIn) of
            (Nothing, Nothing) -> throwError "[AutoRun] Creation of the Marlowe thread failed."
            (Nothing, Just txIn) -> pure $ Just $ anyMarloweThreadCreated (mt, txBody) txIn
            (Just th, _) -> case anyCLIMarloweThread (mt, txBody) mTxIn th of
              Just th' -> pure $ Just th'
              Nothing  -> throwError "[AutoRun] Extending of the Marlowe thread failed."

      thread' <- foldM step ciThread plan
      let
        marloweContract' = marloweContract { ciThread = thread' }
      modifying (contractsL . coerced)  $ Map.insert coContractNickname marloweContract'
    SimulationMode -> do
      -- TODO: We should be able to run balancing in here even in the simulation mode
      pure ()

interpret co@Withdraw {..} =
  view executionModeL >>= skipInSimluationMode co do
  marloweContract@CLIContractInfo {..} <- findCLIContractInfo coContractNickname

  marloweThread <- case ciThread of
    Just marloweThread -> pure marloweThread
    Nothing -> throwLabeledError co "Contract is not on the chain yet so there are not payouts as well."
  Wallet{waAddress, waSigningKey, waMintedTokens} <- findWallet coWalletNickname
  Currency { ccCurrencySymbol } <- maybe (snd <$> getSingletonCurrency) findCurrency ciCurrency
  let
    roles = P.flattenValue waMintedTokens `foldMapFlipped` \(cs, tn, _) ->
      if cs == ccCurrencySymbol
        then [tn]
        else mempty
  when (roles == mempty) $
    throwLabeledError co $ fold
    [ "Provided wallet "
    , show coWalletNickname
    , "has no roles associated with the given contract "
    , show coContractNickname
    ]

  submitMode <- view executionModeL <&> toSubmitMode
  connection <- view connectionL
  txBodies <- foldMapMFlipped roles \role -> do
    let
      lastWithdrawalCheckPoint = Map.lookup role ciWithdrawalsCheckPoints
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
        let
          txIds = map snd possibleWithdrawals
          txInId (C.TxIn txId _) = txId
        filter (flip elem txIds . txInId . fst . unAnUTxO) utxos

    let
      anyWithdrawalsExist = possibleWithdrawals `anyFlipped` \(T.MarloweTransaction{..}, _) ->
        elem role . mapMaybe paymentRole $ mtPayments

    if anyWithdrawalsExist
      then do
        let
          roleToken = M.Token ccCurrencySymbol role
          T.MarloweTransaction { mtRoleValidator } :| _ = ciPlan

        logLabeledMsg co $ "Withdrawing funds for role " <> show role <> " after application of inputs: " <> do
          let
            inputs = foldMapFlipped possibleWithdrawals \(T.MarloweTransaction { mtInputs }, _) -> mtInputs
          show inputs
        era <- view eraL
        txBody <- runLabeledCli era co $ autoWithdrawFundsImpl
          connection
          roleToken
          mtRoleValidator
          Nothing
          waAddress
          [waSigningKey]
          (Just filterPayoutsUTxOs)
          C.TxMetadataNone
          submitMode
          (PrintStats True)
          False
        pure [txBody]
      else
        pure []

  updateWallet coWalletNickname \wallet@Wallet {waSubmittedTransactions} ->
    wallet { waSubmittedTransactions = map SomeTxBody txBodies <> waSubmittedTransactions }

  let
    newWithdrawals = foldMapFlipped roles \role ->
      Map.singleton role (C.getTxId . overAnyMarloweThread getCLIMarloweThreadTxBody $ marloweThread)
    marloweContract' = marloweContract{ ciWithdrawalsCheckPoints = newWithdrawals <> ciWithdrawalsCheckPoints }
  modifying (contractsL . coerced) $ Map.insert coContractNickname marloweContract'

