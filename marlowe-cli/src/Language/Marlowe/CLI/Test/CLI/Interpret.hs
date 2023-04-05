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
{-# LANGUAGE ViewPatterns #-}

module Language.Marlowe.CLI.Test.CLI.Interpret
  where

import Control.Category ((>>>))
import Control.Concurrent (forkFinally)
import Control.Concurrent.Async (async, cancel, concurrently, race, waitCatch)
import Control.Concurrent.STM
  (TChan, TVar, atomically, modifyTVar', newTChanIO, newTVarIO, readTChan, writeTChan, writeTVar)
import Control.Concurrent.STM.TVar (readTVar)
import Control.Exception (Exception(displayException))
import Control.Lens.Setter ((%=))
import Control.Monad.Loops (untilJust)
import Control.Monad.STM (STM)
import Control.Monad.Trans (MonadTrans(lift))
import Data.Default (Default(def))
import qualified Data.Map.Strict as M
import Language.Marlowe.CLI.Transaction
  (buildFaucetImpl, buildMintingImpl, findMarloweScriptsRefs, publishImpl, queryUtxos, selectUtxosImpl)
import qualified Language.Marlowe.CLI.Types as T
import qualified Language.Marlowe.Client as Client
import qualified Language.Marlowe.Core.V1.Semantics as M
import qualified Language.Marlowe.Core.V1.Semantics.Types as M
import Language.Marlowe.Pretty (pretty)
import Language.Marlowe.Runtime.App.Channel (mkDetection)
import qualified Language.Marlowe.Runtime.App.Run as Apps
import qualified Language.Marlowe.Runtime.App.Run as Apps.Run
import Language.Marlowe.Runtime.App.Stream (ContractStream(..), ContractStreamError(..), streamAllContractSteps)
import Language.Marlowe.Runtime.App.Types (Client, runClient)
import qualified Language.Marlowe.Runtime.App.Types as Apps
import qualified Language.Marlowe.Runtime.History.Api as RH
import Ledger.Tx.CardanoAPI (fromCardanoPolicyId)
import Observe.Event.Render.JSON (defaultRenderSelectorJSON)
import Plutus.V1.Ledger.Value (valueOf)
import qualified Plutus.V1.Ledger.Value as P
import qualified Plutus.V1.Ledger.Value as Value
import PlutusPrelude (foldMapM)
import PlutusTx.Prelude (inv)
import qualified PlutusTx.Prelude as PTx
import System.IO.Temp (emptySystemTempFile, emptyTempFile)

import Cardano.Api
  ( AddressInEra
  , AsType(AsPaymentKey)
  , CardanoMode
  , IsShelleyBasedEra
  , Key(getVerificationKey, verificationKeyHash)
  , LocalNodeConnectInfo(LocalNodeConnectInfo)
  , Lovelace(Lovelace)
  , NetworkId
  , PaymentCredential(PaymentCredentialByKey)
  , PolicyId
  , ScriptDataSupportedInEra
  , StakeAddressReference(NoStakeAddress)
  , TxBody
  , generateSigningKey
  , makeShelleyAddressInEra
  )
import qualified Cardano.Api as C
import Control.Lens (assign, coerced, makeLenses, modifying, use, view)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (MonadReader)
import Data.Aeson (FromJSON(..), ToJSON(..), (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as A
import qualified Data.Fixed as F
import qualified Data.Fixed as Fixed
import Data.Foldable (find, fold, for_)
import Data.List.NonEmpty (NonEmpty((:|)), (<|))
import qualified Data.List.NonEmpty as List
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as S (singleton)
import Data.String (IsString(fromString))
import qualified Data.Text as T
import Data.Traversable (for)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import GHC.Num (Natural)
import Language.Marlowe.CLI.Cardano.Api.Value (lovelaceToPlutusValue, toPlutusValue, txOutValueValue)
import Language.Marlowe.CLI.Test.Wallet.Types
  ( AssetId(AdaAsset, AssetId)
  , Assets(Assets)
  , Currencies(Currencies)
  , Currency(Currency, ccCurrencySymbol, ccIssuer)
  , CurrencyNickname
  , TokenAssignment(TokenAssignment)
  , Wallet(Wallet, waAddress, waBalanceCheckBaseline, waMintedTokens, waSigningKey, waSubmittedTransactions)
  , WalletNickname(WalletNickname)
  , WalletOperation(BurnAll, CheckBalance, CreateWallet, FundWallets, Mint, SplitWallet, woBalance, woCurrencyNickname, woIssuer, woMetadata, woMinLovelace, woTokenDistribution, woValues, woWalletNickname, woWalletNicknames)
  , Wallets(Wallets)
  , emptyWallet
  , faucetNickname
  )
import Language.Marlowe.CLI.Types
  ( AnUTxO(AnUTxO, unAnUTxO)
  , CliEnv
  , CliError(CliError)
  , CoinSelectionStrategy(CoinSelectionStrategy)
  , CurrencyIssuer(CurrencyIssuer)
  , MarlowePlutusVersion
  , MarloweScriptsRefs(MarloweScriptsRefs)
  , MarloweTransaction(MarloweTransaction, mtInputs, mtState)
  , PrintStats(PrintStats)
  , PublishingStrategy(PublishAtAddress, PublishPermanently)
  , Seconds
  , SomePaymentSigningKey
  , SomeTimeout
  , ValidatorInfo(ValidatorInfo)
  , defaultCoinSelectionStrategy
  , toMarloweTimeout
  , toPOSIXTime
  )
import qualified Language.Marlowe.Extended.V1 as E
import qualified Language.Marlowe.Runtime.Cardano.Api as Runtime.Cardano.Api
import qualified Language.Marlowe.Runtime.Core.Api as Runtime.Core.Api
import Ledger.Orphans ()
import Plutus.ApiCommon (ProtocolVersion)
import Plutus.V1.Ledger.Api (CostModelParams, CurrencySymbol, TokenName)
import Plutus.V1.Ledger.SlotConfig (SlotConfig)
import Text.Read (readMaybe)

import Contrib.Data.Foldable (anyFlipped, foldMapFlipped, foldMapMFlipped)
import Control.Monad (foldM, forM, forM_, void, when)
import Control.Monad.Error.Class (MonadError(throwError))
import Control.Monad.Extra (whenM)
import Control.Monad.Reader.Class (asks)
import Control.Monad.State.Class (MonadState, gets, modify)
import Data.Coerce (coerce)
import Data.Functor ((<&>))
import Data.Has (Has(getter), modifier)
import qualified Data.List.NonEmpty as List.NonEmpty
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Proxy (Proxy(Proxy))
import Data.Set (Set)
import qualified Data.Text as Text
import Data.Tuple.Extra (uncurry3)
import qualified Language.Marlowe as Marlowe
import Language.Marlowe.CLI.Cardano.Api.PlutusScript (IsPlutusScriptLanguage)
import qualified Language.Marlowe.CLI.Cardano.Api.Value as CV
import Language.Marlowe.CLI.Command.Template (initialMarloweState, makeContract)
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
import Language.Marlowe.CLI.Test.CLI.Monad (runCli, runLabeledCli, runTraceCli)
import Language.Marlowe.CLI.Test.CLI.Types
  ( CLIContractInfo(CLIContractInfo, ciContract, ciCurrency, ciPlan, ciSubmitter, ciThread, ciWithdrawalsCheckPoints)
  , CLIContracts(CLIContracts)
  , CLIOperation(..)
  , ContractSource(InlineContract, UseTemplate)
  , InterpretMonad
  , MarloweValidators(..)
  , PartyRef(RoleRef, WalletRef)
  , UseTemplate(..)
  , anyCLIMarloweThread
  , getCLIMarloweThreadTransaction
  , getCLIMarloweThreadTxBody
  , ieConnection
  , ieCostModelParams
  , ieEra
  , ieExecutionMode
  , iePrintStats
  , ieProtocolVersion
  , ieSlotConfig
  , isContracts
  , isCurrencies
  , isPublishedScripts
  , isWallets
  )
import Language.Marlowe.CLI.Test.Contract (ContractNickname(..))
import Language.Marlowe.CLI.Test.Contract.ParametrizedMarloweJSON
  (ParametrizedMarloweJSON(ParametrizedMarloweJSON), decodeParametrizedContractJSON, decodeParametrizedInputJSON)
import Language.Marlowe.CLI.Test.ExecutionMode
  (ExecutionMode(OnChainMode, SimulationMode), skipInSimluationMode, toSubmitMode)
import Language.Marlowe.CLI.Test.Log (logLabeledMsg, logTraceMsg, throwLabeledError, throwTraceError)
import Language.Marlowe.Cardano (marloweNetworkFromLocalNodeConnectInfo)
import Language.Marlowe.Cardano.Thread
  (anyMarloweThreadCreated, foldrMarloweThread, marloweThreadTxIn, overAnyMarloweThread)
import Language.Marlowe.Protocol.Sync.Client (MarloweSyncClient)
import qualified Language.Marlowe.Runtime.App.Stream as Runtime.App
import Language.Marlowe.Runtime.ChainSync.Api (SlotNo)
import Language.Marlowe.Runtime.Core.Api (ContractId, MarloweVersionTag(V1))
import Marlowe.Contracts (coveredCall, escrow, swap, trivial)
import Observe.Event.Backend (EventBackend)
import Observe.Event.Dynamic (DynamicEventSelector)
import Observe.Event.Render.JSON.Handle (JSONRef)
import qualified Plutus.V1.Ledger.Value as PV
import PlutusTx.Monoid (Group(inv))

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

findWalletByAddress
  :: InterpretMonad m lang era
  => C.AddressInEra era
  -> m (WalletNickname, Wallet era)
findWalletByAddress address = do
  Wallets wallets <- use isWallets
  let
    wallet = find (\(_, w) -> address == waAddress w) (Map.toList wallets)

  liftCliMaybe
    ("[findWalletByPkh] Wallet not found for a given address: " <> show address <> " in wallets: " <> show wallets)
    wallet

findCurrency
  :: InterpretMonad m lang era
  => CurrencyNickname
  -> m Currency
findCurrency nickname = do
  (Currencies currencies) <- use isCurrencies
  liftCliMaybe ("[findCurrency] Unable to find currency:" <> show nickname) $ Map.lookup nickname currencies

findWalletByUniqueToken
  :: InterpretMonad m lang era
  => CurrencyNickname
  -> TokenName
  -> m (WalletNickname, Wallet era)
findWalletByUniqueToken currencyNickname tokenName = do
  Currency {..} <- findCurrency currencyNickname
  let
    check value = valueOf value ccCurrencySymbol tokenName == 1
    step Nothing (n, wallet@(waMintedTokens -> tokens)) = pure $ if check tokens
      then Just (n, wallet)
      else Nothing
    step res c@(n, waMintedTokens -> tokens) = if check tokens
      then case res of
        Just (n', _) ->
          throwError $ CliError $ "[findByUniqueToken] Token is not unique - found in two wallets: " <> show n <> " and " <> show n' <> "."
        Nothing ->
          pure (Just c)
      else pure res
  Wallets wallets <- use isWallets
  walletInfo <- foldM step Nothing (Map.toList wallets)
  liftCliMaybe
    ("[findWalletByUniqueToken] Wallet not found for a given token: " <> show ccCurrencySymbol <> ":" <> show tokenName)
    walletInfo

updateWallet
  :: InterpretMonad m lang era
  => WalletNickname
  -> (Wallet era -> Wallet era)
  -> m ()
updateWallet nickname update = do
  wallet <- findWallet nickname
  let
    wallet' = update wallet
  modifying isWallets \(Wallets wallets) -> Wallets (Map.insert nickname wallet' wallets)

findCLIContractInfo
  :: InterpretMonad m lang era
  => ContractNickname
  -> m (CLIContractInfo lang era)
findCLIContractInfo nickname = do
  CLIContracts contracts <- use isContracts
  liftCliMaybe
    ("[findCLIContractInfo] Marlowe contract structure was not found for a given nickname " <> show (coerce nickname :: String) <> ".")
    $ Map.lookup nickname contracts

getSingletonCurrency
  :: InterpretMonad m lang era
  => m (CurrencyNickname, Currency)
getSingletonCurrency = do
  Currencies currencies <- use isCurrencies
  case Map.toList currencies of
    [c] -> pure c
    _   -> throwError "Ambigious currency lookup."

decodeInputJSON
  :: InterpretMonad m lang era
  => ParametrizedMarloweJSON
  -> m M.Input
decodeInputJSON json = do
  currencies <- use isCurrencies
  wallets <- use isWallets
  network <- view ieConnection <&> marloweNetworkFromLocalNodeConnectInfo
  case decodeParametrizedInputJSON network wallets currencies json of
    Left err -> throwError $ CliError $ "Failed to decode input: " <> show err
    Right i  -> pure i

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

autoRunTransaction :: forall era lang m
                    . IsShelleyBasedEra era
                   => IsPlutusScriptLanguage lang
                   => InterpretMonad m lang era
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

  connection <- view ieConnection
  submitMode <- view ieExecutionMode <&> toSubmitMode
  era <- view ieEra
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
    submitter { waSubmittedTransactions = txBody : waSubmittedTransactions }

  let meOuts = classifyOutputs mTxId txOuts
  case filter isMarloweOut meOuts of
    [ApplicationOut {moTxIn}] -> do
      log' $ "Marlowe output:" <> show moTxIn
      pure (txBody, Just moTxIn)
    []                        -> pure (txBody, Nothing)
    _                         -> throwError "[AutoRun] Multiple Marlowe outputs detected - unable to handle them yet."

buildParty
  :: InterpretMonad m lang era
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

useTemplate
  :: InterpretMonad m lang era
  => Maybe CurrencyNickname
  -> UseTemplate
  -> m M.Contract
useTemplate currency =
  \case
  UseTrivial{..} -> do
    timeout' <- toMarloweTimeout utTimeout
    let
      partyRef = fromMaybe (WalletRef faucetNickname) utParty
    party <- buildParty currency partyRef
    makeContract $ trivial
      party
      utDepositLovelace
      utWithdrawalLovelace
      timeout'
  UseSwap{..} -> do
    aTimeout' <- toMarloweTimeout utATimeout
    bTimeout' <- toMarloweTimeout utBTimeout
    Currency { ccCurrencySymbol=aCurrencySymbol } <- findCurrency utACurrencyNickname
    Currency { ccCurrencySymbol=bCurrencySymbol } <- findCurrency utBCurrencyNickname
    let
      aPartyRef = fromMaybe (WalletRef "aParty") utAParty
      bPartyRef = fromMaybe (WalletRef "bParty") utBParty
    aParty <- buildParty currency aPartyRef
    bParty <- buildParty currency bPartyRef
    makeContract $ swap
        aParty
        (M.Token aCurrencySymbol utATokenName)
        (E.Constant utAAmount)
        aTimeout'
        bParty
        (M.Token bCurrencySymbol utBTokenName)
        (E.Constant utBAmount)
        bTimeout'
        E.Close
  UseEscrow{..} -> do
    paymentDeadline' <- toMarloweTimeout utPaymentDeadline
    complaintDeadline' <- toMarloweTimeout utComplaintDeadline
    disputeDeadline' <- toMarloweTimeout utDisputeDeadline
    mediationDeadline' <- toMarloweTimeout utMediationDeadline
    let
      sellerRef = fromMaybe (WalletRef "Seller") utSeller
      buyerRef = fromMaybe (WalletRef "Buyer") utBuyer
      mediatorRef = fromMaybe (WalletRef "Mediator") utMediator
    seller <- buildParty currency sellerRef
    buyer <- buildParty currency buyerRef
    mediator <- buildParty currency mediatorRef

    makeContract $ escrow
      (E.Constant utPrice)
      seller
      buyer
      mediator
      paymentDeadline'
      complaintDeadline'
      disputeDeadline'
      mediationDeadline'
  -- UseCoveredCall{..} -> do
  --   issueDate' <- toMarloweTimeout utIssueDate
  --   maturityDate' <- toMarloweTimeout utMaturityDate
  --   settlementDate' <- toMarloweTimeout utSettlementDate
  --   issuer <- buildParty currency utIssuer
  --   counterParty <- buildParty currency utCounterParty

  --   case utCurrency of
  --

  --   Currency { ccCurrencySymbol=currency } <- findCurrency utCurrency
  --   Currency { ccCurrencySymbol=underlying } <- findCurrency utUnderlying

  --   makeContract $ coveredCall
  --       issuer
  --       counterParty
  --       Nothing
  --       currency
  --       underlying
  --       (Constant strike)
  --       (Constant amount)
  --       issueDate'
  --       maturityDate'
  --       settlementDate'
  --UseZeroCouponBond{..} -> do  lendingDeadline' <- toMarloweTimeout lendingDeadline
  --                             paybackDeadline' <- toMarloweTimeout paybackDeadline
  --                             makeContract $
  --                               zeroCouponBond
  --                                 lender
  --                                 borrower
  --                                 lendingDeadline'
  --                                 paybackDeadline'
  --                                 (Constant principal)
  --                                 (Constant principal `AddValue` Constant interest)
  --                                 ada
  --                                 Close
  template -> throwError $ CliError $ "Template not implemented: " <> show template

publishCurrentValidators
  :: forall env era lang st m
   . C.IsShelleyBasedEra era
  => InterpretMonad m lang era
  => Maybe Bool
  -> Maybe WalletNickname
  -> m (MarloweScriptsRefs MarlowePlutusVersion era)
publishCurrentValidators publishPermanently possiblePublisher = do
  Wallet { waAddress, waSigningKey } <- maybe getFaucet findWallet possiblePublisher
  let
    publishingStrategy = case publishPermanently of
      Just True -> PublishPermanently NoStakeAddress
      _         -> PublishAtAddress waAddress
  connection <- view ieConnection
  printStats <- view iePrintStats
  era <- view ieEra
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

    Nothing -> view ieExecutionMode >>= \case
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
  => InterpretMonad m lang era
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

  era <- view ieEra
  slotConfig <- view ieSlotConfig
  costModelParams <- view ieCostModelParams
  protocolVersion <- view ieProtocolVersion
  LocalNodeConnectInfo { localNodeNetworkId } <- view ieConnection

  marloweTransaction <- case coMarloweValidators of
    ReferenceCurrentValidators publishPermanently possiblePublisher -> do
      logLabeledMsg co "Using reference scripts to the current Marlowe validator."
      refs <- use isPublishedScripts >>= \case
        Nothing -> do
          logLabeledMsg co "Publishing the scripts."
          marloweScriptRefs <- publishCurrentValidators publishPermanently possiblePublisher
          assign isPublishedScripts (Just marloweScriptRefs)
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
      era <- view ieEra
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

  CLIContracts contracts <- use isContracts
  when (isJust . Map.lookup coContractNickname $ contracts) do
    throwLabeledError co "Contract with a given nickname already exist."

  logLabeledMsg co $ "Saving initialized contract " <> show (coerce coContractNickname :: String)
  modifying (isContracts . coerced) $ Map.insert coContractNickname $ CLIContractInfo
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
  era <- view ieEra
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

  modifying (isContracts . coerced) $ Map.insert coContractNickname marloweContract'

interpret co@Publish {..} = do
  logLabeledMsg co "Using reference scripts to the current Marlowe validator."
  use isPublishedScripts >>= \case
    Nothing -> do
      logLabeledMsg co "Publishing the scripts."
      marloweScriptRefs <- publishCurrentValidators coPublishPermanently coPublisher
      assign isPublishedScripts (Just marloweScriptRefs)
    Just refs -> do
      throwLabeledError co "Scripts were already published during this test scenario."

interpret co@AutoRun {..} = do
  executionMode <- view ieExecutionMode
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
      modifying (isContracts . coerced)  $ Map.insert coContractNickname marloweContract'
    SimulationMode -> do
      -- TODO: We should be able to run balancing in here even in the simulation mode
      pure ()

interpret co@Withdraw {..} =
  view ieExecutionMode >>= skipInSimluationMode co do
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

  submitMode <- view ieExecutionMode <&> toSubmitMode
  connection <- view ieConnection
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
        era <- view ieEra
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
    wallet { waSubmittedTransactions = txBodies <> waSubmittedTransactions }

  let
    newWithdrawals = foldMapFlipped roles \role ->
      Map.singleton role (C.getTxId . overAnyMarloweThread getCLIMarloweThreadTxBody $ marloweThread)
    marloweContract' = marloweContract{ ciWithdrawalsCheckPoints = newWithdrawals <> ciWithdrawalsCheckPoints }
  modifying (isContracts . coerced) $ Map.insert coContractNickname marloweContract'

