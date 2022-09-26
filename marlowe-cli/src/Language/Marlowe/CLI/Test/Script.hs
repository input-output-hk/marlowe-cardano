-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Testing Marlowe contracts without the PAB.
--
-----------------------------------------------------------------------------


{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}


module Language.Marlowe.CLI.Test.Script
  where

import Cardano.Api
  ( AsType(AsPaymentKey)
  , CardanoMode
  , IsShelleyBasedEra
  , Key(getVerificationKey, verificationKeyHash)
  , LocalNodeConnectInfo(..)
  , PaymentCredential(PaymentCredentialByKey)
  , ScriptDataSupportedInEra
  , StakeAddressReference(NoStakeAddress)
  , generateSigningKey
  , makeShelleyAddressInEra
  )
import Control.Monad (foldM, forM, forM_, void, when)
import Control.Monad.Except (MonadError, MonadIO, catchError, liftIO, throwError)
import Control.Monad.State.Strict (MonadState, execStateT)
import Language.Marlowe.CLI.Command.Template (initialMarloweState, makeContract)
import Language.Marlowe.CLI.Types
  ( AnUTxO(AnUTxO)
  , CliEnv(..)
  , CliError(..)
  , CoinSelectionStrategy(CoinSelectionStrategy)
  , CurrencyIssuer(CurrencyIssuer)
  , MarlowePlutusVersion
  , MarloweScriptsRefs(MarloweScriptsRefs)
  , MarloweTransaction(mtState)
  , OutputQueryResult
  , PrintStats(PrintStats)
  , PublishingStrategy(PublishAtAddress, PublishPermanently)
  , ValidatorInfo(ValidatorInfo)
  , defaultCoinSelectionStrategy
  , toMarloweTimeout
  , toPOSIXTime
  , unAnUTxO
  )
import Language.Marlowe.Extended.V1 as E (ChoiceId(ChoiceId), Party)
import Marlowe.Contracts (swap, trivial)
import Plutus.V1.Ledger.Api (CostModelParams, TokenName)

import qualified Cardano.Api as C
import Control.Lens (assign, modifying, use, view)
import Control.Monad.Extra (whenM)
import Control.Monad.RWS.Class (MonadReader)
import Control.Monad.Reader (ReaderT(runReaderT))
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.OneLine as A
import qualified Data.Fixed as F
import Data.Foldable (Foldable(fold), foldl')
import Data.Foldable.Extra (for_)
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty((:|)), (<|))
import qualified Data.List.NonEmpty as L.NonEmpty
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import qualified Data.Set as S (singleton)
import qualified Data.Text as Text
import Data.Traversable (for)
import Data.Tuple.Extra (uncurry3)
import Language.Marlowe.CLI.Cardano.Api.PlutusScript (IsPlutusScriptLanguage)
import Language.Marlowe.CLI.Cardano.Api.Value (txOutValueValue)
import qualified Language.Marlowe.CLI.Cardano.Api.Value as CV
import qualified Language.Marlowe.CLI.Data.Aeson.Traversals as A
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
import Language.Marlowe.CLI.Test.Script.Debug
  (SoFormat(SoName), logSoMsg, logSoMsg', logTraceMsg, runSoCli, throwSoError, withCliErrorMsg)
import Language.Marlowe.CLI.Test.Types
  ( AssetId(AdaAsset, AssetId)
  , Assets(Assets)
  , ContractNickname
  , ContractSource(..)
  , Currency(Currency, ccCurrencySymbol, ccIssuer)
  , CurrencyNickname
  , ExecutionMode(..)
  , MarloweContract(..)
  , PartyRef(RoleRef, WalletRef)
  , ScriptEnv(..)
  , ScriptOperation(..)
  , ScriptState
  , ScriptTest(ScriptTest, stScriptOperations, stTestName)
  , Seconds(Seconds)
  , TokenAssignment(TokenAssignment)
  , UseTemplate(..)
  , Wallet(..)
  , WalletNickname(WalletNickname)
  , anyMarloweThread
  , emptyWallet
  , faucetNickname
  , foldrMarloweThread
  , getMarloweThreadTransaction
  , getMarloweThreadTxBody
  , getMarloweThreadTxIn
  , overAnyMarloweThread
  , scriptState
  , seConnection
  , seCostModelParams
  , seEra
  , seExecutionMode
  , sePrintStats
  , seProtocolVersion
  , seSlotConfig
  , ssContracts
  , ssCurrencies
  , ssReferenceScripts
  , ssWallets
  )
import Language.Marlowe.CLI.Transaction
  (buildFaucetImpl, buildMintingImpl, findMarloweScriptsRefs, publishImpl, selectUtxosImpl)
import qualified Language.Marlowe.CLI.Types as T
import qualified Language.Marlowe.Client as Client
import qualified Language.Marlowe.Core.V1.Semantics as M
import qualified Language.Marlowe.Core.V1.Semantics.Types as M
import qualified Language.Marlowe.Extended.V1 as EM
import Language.Marlowe.Pretty (pretty)
import Ledger.Tx.CardanoAPI (fromCardanoPolicyId)
import Plutus.ApiCommon (ProtocolVersion)
import Plutus.V1.Ledger.SlotConfig (SlotConfig(..))
import Plutus.V1.Ledger.Value (mpsSymbol, valueOf)
import qualified Plutus.V1.Ledger.Value as P
import qualified Plutus.V1.Ledger.Value as Value
import PlutusPrelude (foldMapM)
import PlutusTx.Prelude (inv)
import qualified PlutusTx.Prelude as PTx
import System.IO.Temp (emptySystemTempFile, emptyTempFile)

timeoutForOnChainMode :: MonadError CliError m
          => MonadReader (ScriptEnv era) m
          => m (Maybe Int)
timeoutForOnChainMode = do
            executionMode <- view seExecutionMode
            let
              executionTimeout = case executionMode of
                (OnChainMode (Seconds transactionTimeout)) -> Just transactionTimeout
                SimulationMode -> Nothing
            pure executionTimeout

interpret :: forall era m
           . IsShelleyBasedEra era
          => MonadError CliError m
          => MonadState (ScriptState MarlowePlutusVersion era) m
          => MonadReader (ScriptEnv era) m
          => MonadIO m
          => ScriptOperation
          -> m ()
interpret so@CreateWallet {..} = do
  skey <- liftIO $ generateSigningKey AsPaymentKey
  connection <- view seConnection
  let
    vkey = getVerificationKey skey
    LocalNodeConnectInfo {localNodeNetworkId} = connection
    address = makeShelleyAddressInEra localNodeNetworkId (PaymentCredentialByKey (verificationKeyHash vkey)) NoStakeAddress
    WalletNickname rawNickname = soWalletNickname
  let wallet = emptyWallet address (Left skey)
  logSoMsg' so $ "Wallet " <> show rawNickname <> " created with an address: " <> Text.unpack (C.serialiseAddress address)

  (addrFile, T.SigningKeyFile skeyFile) <- liftIO $ saveWallet soWalletNickname wallet Nothing
  logSoMsg' so $ "Wallet info stored in " <> addrFile <> " and " <> skeyFile

  ssWallets `modifying` Map.insert soWalletNickname wallet

interpret FundWallets {..} = do
  let
    values = [ C.lovelaceToValue v | v <- soValues ]

  (Wallet faucetAddress _ _ faucetSigningKey _ _) <- getFaucet
  addresses <- for soWalletNicknames \walletNickname -> do
    (Wallet address _ _ _ _ _) <- findWallet walletNickname
    pure address
  connection <- view seConnection
  timeout <- timeoutForOnChainMode
  txBody <- runCli "[FundWallet] " $ buildFaucetImpl
    connection
    (Just values)
    addresses
    faucetAddress
    faucetSigningKey
    defaultCoinSelectionStrategy
    timeout

  updateFaucet \faucet@(Wallet _ _ _ _ faucetTransactions _) ->
    faucet { waSubmittedTransactions = txBody : faucetTransactions }

interpret SplitWallet {..} = do
  Wallet address _ _ skey _ _ <- findWallet soWalletNickname
  connection <- view seConnection
  timeout <- timeoutForOnChainMode
  let
    values = [ C.lovelaceToValue v | v <- soValues ]

  void $ runCli "[createCollaterals] " $ buildFaucetImpl
    connection
    (Just values)
    [address]
    address
    skey
    defaultCoinSelectionStrategy
    timeout

interpret so@CheckBalance {..} = do
  wallet@Wallet {..} <- findWallet soWalletNickname
  utxos <- getWalletUTxO $ Right wallet
  let
    onChainTotal = CV.toPlutusValue $ foldMap txOutValueValue . Map.elems . C.unUTxO $ utxos

    fees = waSubmittedTransactions `foldMapFlipped` \(C.TxBody txBodyContent) -> do
      case C.txFee txBodyContent of
        C.TxFeeExplicit _ lovelace -> lovelace
        C.TxFeeImplicit _ -> C.Lovelace 0

    actualBalance = onChainTotal <> lovelaceToPlutusValue fees <> waMintingDistributionCosts <> inv waBalanceCheckBaseline

  logSoMsg' so $ "Number of already submitted transactions: " <> show (length waSubmittedTransactions)
  logSoMsg' so $ "Total transaction fees amount: " <> do
    let
      C.Lovelace amount = fees
    show ((fromInteger amount / 1_000_000) :: F.Micro) <> " ADA"

  expectedBalance <- assetsToPlutusValue soBalance
  when (expectedBalance /= actualBalance) do
    throwSoError so $ "Balance check difference: expectedBalance - actualBalance = " <> show (expectedBalance <> inv actualBalance)

interpret so@Mint {..} = do
  let
    issuerNickname = fromMaybe faucetNickname soIssuer

  currencies <- use ssCurrencies
  case Map.lookup soCurrencyNickname currencies of
    Just Currency { ccIssuer=ci } -> when (ci /= issuerNickname)  do
      throwError "Currency with a given nickname already exist and is minted by someone else."
    Nothing -> pure ()

  Wallet issuerAddress _ _ issuerSigningKey _ _ <- findWallet issuerNickname
  (tokenDistribution, walletAssignemnts) <- unzip <$> forM soTokenDistribution \(TokenAssignment owner tokenName amount) -> do
    Wallet destAddress _ _ _ _ _ <- findWallet owner
    pure ((tokenName, amount, destAddress), (owner, tokenName, amount))

  logSoMsg' so $ "Minting currency " <> show soCurrencyNickname <> " with tokens distribution: " <> show soTokenDistribution
  tokenDistribution' <- maybe (throwSoError so "Token distribution shouldn't be empty") pure $ L.NonEmpty.nonEmpty tokenDistribution
  let
    mintingAction = T.Mint
      (CurrencyIssuer issuerAddress issuerSigningKey)
      tokenDistribution'

  connection <- view seConnection
  timeout <- timeoutForOnChainMode
  printStats <- view sePrintStats
  (mintingTx, policy) <- runCli "[Mint] " $ buildMintingImpl
    connection
    mintingAction
    soMetadata
    Nothing
    timeout
    printStats

  logSoMsg' so $ "This currency symbol is " <> show policy
  let
    currencySymbol = mpsSymbol . fromCardanoPolicyId $ policy
    currency = Currency currencySymbol issuerNickname policy
    C.TxBody mintingTxContent = mintingTx

  forM_ walletAssignemnts \(walletNickname, tokenName, amount) -> do
    updateWallet walletNickname \wallet@Wallet {..} ->  do
      let
        minAdaIncome = C.txOuts mintingTxContent `foldMapFlipped` \(C.TxOut addr txOutValue _ _) ->
          -- If min ada goes to the issuer don't count it as a cost
          if addr == waAddress && addr /= issuerAddress
            then lovelaceToPlutusValue . C.selectLovelace . C.txOutValueToValue $ txOutValue
            else mempty
        tokenValue = Value.singleton currencySymbol tokenName (toInteger amount)
      wallet
        { waMintedTokens = tokenValue <> waMintedTokens
        , waMintingDistributionCosts = waMintingDistributionCosts <> PTx.inv minAdaIncome
        }

  updateWallet issuerNickname \issuer@Wallet {..} -> do
    let
      C.TxBody c = mintingTx
      minAdas = C.txOuts c `foldMapFlipped` \(C.TxOut addr value _ _) ->
        if addr /= issuerAddress
          then lovelaceToPlutusValue . C.selectLovelace . C.txOutValueToValue $ value
          else mempty
    issuer
      { waSubmittedTransactions = mintingTx : waSubmittedTransactions
      , waMintingDistributionCosts = waMintingDistributionCosts <> minAdas
      }
  ssCurrencies `modifying` Map.insert soCurrencyNickname currency

interpret so@BurnAll {..} = do
  Currency { ccCurrencySymbol, ccIssuer } <- findCurrency soCurrencyNickname
  Wallet { waAddress=issuerAddress, waSigningKey=issuerSigningKey } <- findWallet ccIssuer
  allWallets <- use ssWallets
  let
    providers = Map.elems allWallets <&> \Wallet { waAddress, waSigningKey } -> (waAddress, waSigningKey)
    currencyIssuer = T.CurrencyIssuer issuerAddress issuerSigningKey
    mintingAction = T.BurnAll currencyIssuer $ (issuerAddress, issuerSigningKey) :| filter ((/=) issuerAddress. fst) providers
  connection <- view seConnection
  timeout <- timeoutForOnChainMode
  printStats <- view sePrintStats
  (burningTx, _) <- runSoCli so $ buildMintingImpl
    connection
    mintingAction
    soMetadata
    Nothing
    timeout
    printStats

  -- We burn *all* the tokens so we have to drop them from the wallet balance baseline as well.
  -- We should not include them in this reference of untracked funds from the chain because they
  -- are not on chain anymore.
  for_ (Map.toList allWallets) \(walletNickname, Wallet { waBalanceCheckBaseline }) -> do
    let
      balanceCheckBaseline' = do
        let
          check (cs, _, _) = cs /= ccCurrencySymbol
        foldMap (uncurry3 P.singleton) . filter check . P.flattenValue $ waBalanceCheckBaseline

    updateWallet walletNickname \wallet@Wallet {} ->
      wallet { waBalanceCheckBaseline = balanceCheckBaseline' }

  updateWallet ccIssuer \issuer@Wallet {waSubmittedTransactions} ->
    issuer { waSubmittedTransactions = burningTx : waSubmittedTransactions }

interpret so@Initialize {..} = do
  let
    log' = logSoMsg SoName so

  marloweContract <- case soContractSource of
    InlineContract json -> decodeContractJSON json
    UseTemplate setup   -> useTemplate soRoleCurrency setup

  log' $ "Contract: " <> show (pretty marloweContract)

  let
    submitterNickname = fromMaybe faucetNickname soSubmitter
  address <- waAddress <$> findWallet submitterNickname
  submitterParty <- uncurry M.Address <$> marloweAddressFromCardanoAddress address

  currencySymbol <- case soRoleCurrency of
    Nothing -> pure P.adaSymbol
    Just nickname -> do
      Currency { ccCurrencySymbol } <- findCurrency nickname
      pure ccCurrencySymbol

  let
    marloweState = initialMarloweState submitterParty soMinAda
    marloweParams = Client.marloweParams currencySymbol

  slotConfig <- view seSlotConfig
  costModelParams <- view seCostModelParams
  protocolVersion <- view seProtocolVersion
  LocalNodeConnectInfo { localNodeNetworkId } <- view seConnection
  marloweTransaction <- use ssReferenceScripts >>= \case
    Just refs -> do
      logSoMsg' so "Using reference scripts to initialize Marlowe contract."
      runSoCli so $ initializeTransactionUsingScriptRefsImpl
        marloweParams
        slotConfig
        refs
        NoStakeAddress
        marloweContract
        marloweState
        False
        True
    Nothing -> do
      logSoMsg' so "Using in Tx scripts strategy to initialize Marlowe contract."
      runSoCli so $ initializeTransactionImpl
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

  whenM (isJust . Map.lookup soContractNickname <$> use ssContracts) do
    throwError "[Initialize] Contract with a given nickname already exist."

  modifying ssContracts $ Map.insert soContractNickname $ MarloweContract
    {
      mcContract = marloweContract
    , mcPlan = marloweTransaction :| []
    , mcThread = Nothing
    , mcWithdrawalsCheckPoints = mempty
    , mcCurrency = soRoleCurrency
    , mcSubmitter = submitterNickname
    }

interpret Prepare {..} = do
  marloweContract@MarloweContract {..} <- findMarloweContract soContractNickname

  let
    curr = NE.head mcPlan
  inputs <- for soInputs \input -> decodeInputJSON input
  minimumTime <- toPOSIXTime soMinimumTime
  maximumTime <- toPOSIXTime soMaximumTime
  new <- runCli "[Prepare] " $ prepareTransactionImpl
    curr
    inputs
    minimumTime
    maximumTime
    True

  let
    new' = case soOverrideMarloweState of
      Just customState -> new { mtState = customState }
      Nothing          -> new
    plan = new' <| mcPlan
    marloweContract' = marloweContract{ mcPlan = plan }

  modifying ssContracts $ Map.insert soContractNickname marloweContract'

interpret so@AutoRun {..} = do
  timeoutForOnChainMode >>= \case
    Nothing -> logSoMsg' so "Execution Mode set to 'Simulation' - Transactions are not submitted to chain in simulation mode"
    (Just _) -> do
      marloweContract@MarloweContract {..} <- findMarloweContract soContractNickname
      let
        plan = do
          let
            whole = reverse $ NE.toList mcPlan
            threadLength = foldrMarloweThread (const (+ 1)) 0
          case mcThread of
            Just thread -> do
              let
                l = overAnyMarloweThread threadLength thread
              drop l whole
            Nothing -> whole
        step th mt = do
          let
            prev :: Maybe (MarloweTransaction MarlowePlutusVersion era, C.TxIn)
            prev = do
              pmt <- overAnyMarloweThread getMarloweThreadTransaction <$> th
              txIn <- overAnyMarloweThread getMarloweThreadTxIn =<< th
              pure (pmt, txIn)
            invalid = fromMaybe False soInvalid

          (txBody, mTxIn) <- autoRunTransaction mcCurrency mcSubmitter prev mt invalid
          case anyMarloweThread mt txBody mTxIn th of
            Just th' -> pure $ Just th'
            Nothing  -> throwError "[AutoRun] Extending of the marlowe thread failed."

      thread' <- foldM step mcThread plan
      let
        marloweContract' = marloweContract { mcThread = thread' }
      ssContracts `modifying`  Map.insert soContractNickname marloweContract'

interpret so@Withdraw {..} = do
  marloweContract@MarloweContract {..} <- findMarloweContract soContractNickname

  marloweThread <- case mcThread of
    Just marloweThread -> pure marloweThread
    Nothing -> throwSoError so "Contract is not on the chain yet so there are not payouts as well."
  Wallet{waAddress, waSigningKey, waMintedTokens} <- findWallet soWalletNickname
  Currency { ccCurrencySymbol } <- maybe (snd <$> getCurrency) findCurrency mcCurrency
  let
    roles = P.flattenValue waMintedTokens `foldMapFlipped` \(cs, tn, _) ->
      if cs == ccCurrencySymbol
        then [tn]
        else mempty
  when (roles == mempty) $ do
    throwSoError so $ fold
      [ "Provided wallet "
      , show soWalletNickname
      , "has no roles associated with the given contract "
      , show soContractNickname
      ]

  timeout <- timeoutForOnChainMode
  connection <- view seConnection
  txBodies <- foldMapMFlipped roles \role -> do
    let
      lastWithdrawalCheckPoint = Map.lookup role mcWithdrawalsCheckPoints
      threadTransactions :: [(MarloweTransaction MarlowePlutusVersion era, C.TxId)]
      threadTransactions = do
        let step item acc = (getMarloweThreadTransaction item, C.getTxId . getMarloweThreadTxBody $ item) : acc
        overAnyMarloweThread (foldrMarloweThread step []) marloweThread

      possibleWithdrawals = takeWhile ((/=) lastWithdrawalCheckPoint . Just . snd) threadTransactions

      paymentRole (M.Payment _ (M.Party (M.Role r)) _ _) = Just r
      paymentRole _ = Nothing

      -- Sometimes we reuse the same currency accross multiple tests (when Faucet is an issuer) so we
      -- need to filter out payouts which are really associated with this particular test
      -- case. We can identify them by matching them against a set of submitted transaction ids.
      filterPayoutsUTxOs utxos = do
        let
          txIds = map snd possibleWithdrawals
          txInId (C.TxIn txId _) = txId
        filter (flip elem txIds . txInId . fst . unAnUTxO) utxos

    let
      anyWithdrawalsExist = possibleWithdrawals `anyFlipped` \(T.MarloweTransaction{..}, _) -> do
        elem role . mapMaybe paymentRole $ mtPayments

    if anyWithdrawalsExist
      then do
        let
          roleToken = M.Token ccCurrencySymbol role
          T.MarloweTransaction { mtRoleValidator } :| _ = mcPlan

        logSoMsg' so $ "Withdrawing funds for role " <> show role <> " after application of inputs: " <> do
          let
            inputs = foldMapFlipped possibleWithdrawals \(T.MarloweTransaction { mtInputs }, _) -> mtInputs
          show inputs

        txBody <- runSoCli so $ autoWithdrawFundsImpl
          connection
          roleToken
          mtRoleValidator
          Nothing
          waAddress
          [waSigningKey]
          (Just filterPayoutsUTxOs)
          C.TxMetadataNone
          timeout
          (PrintStats True)
          False
        pure [txBody]
      else
        pure []

  updateWallet soWalletNickname \wallet@Wallet {waSubmittedTransactions} ->
    wallet { waSubmittedTransactions = txBodies <> waSubmittedTransactions }

  let
    newWithdrawals = foldMapFlipped roles \role ->
      Map.singleton role (C.getTxId . overAnyMarloweThread getMarloweThreadTxBody $ marloweThread)
    marloweContract' = marloweContract{ mcWithdrawalsCheckPoints = newWithdrawals <> mcWithdrawalsCheckPoints }
  modifying ssContracts $ Map.insert soContractNickname marloweContract'

interpret so@Publish {..} = do
  whenM (isJust <$> use ssReferenceScripts) do
    throwSoError so "Scripts already published in this test script."

  Wallet { waAddress, waSigningKey } <- maybe getFaucet findWallet soPublisher
  let
    publishingStrategy = case soPublishPermanently of
      Just True -> PublishPermanently NoStakeAddress
      _         -> PublishAtAddress waAddress

  connection <- view seConnection
  printStats <- view sePrintStats
  marloweScriptRefs <- runSoCli so (findMarloweScriptsRefs connection publishingStrategy printStats) >>= \case
    Just marloweScriptRefs@(MarloweScriptsRefs (AnUTxO (mTxIn, _), mv) (AnUTxO (pTxIn, _), pv)) -> do
      let
        logValidatorInfo ValidatorInfo {..} = do
          logSoMsg' so $ Text.unpack (C.serialiseAddress viAddress)

      logSoMsg' so "Found already published scripts so using them."
      logSoMsg' so $ "Marlowe reference: " <> show mTxIn
      logValidatorInfo mv
      logSoMsg' so $ "Payout reference: " <> show pTxIn
      logValidatorInfo pv
      pure marloweScriptRefs

    Nothing -> view seExecutionMode >>= \case
      SimulationMode -> throwSoError so "Can't perform on chain script publishing in simulation mode"
      OnChainMode (Seconds timeout) -> do
        logSoMsg' so "Scripts not found so publishing them."
        runSoCli so $ publishImpl
          connection
          waSigningKey
          Nothing
          waAddress
          publishingStrategy
          (CoinSelectionStrategy False False [])
          timeout
          (PrintStats True)
  assign ssReferenceScripts (Just marloweScriptRefs)

interpret (Fail message) = throwError $ CliError message


autoRunTransaction :: forall era lang m
                    . IsShelleyBasedEra era
                   => IsPlutusScriptLanguage lang
                   => MonadError CliError m
                   => MonadReader (ScriptEnv era) m
                   => MonadState (ScriptState lang era) m
                   => MonadIO m
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
      M.IChoice (ChoiceId _ party) _ -> Just party
      M.INotify                      -> Nothing

    getInputParty :: Maybe Party -> M.Input -> m (Maybe Party)
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

  (submitterNickname, Wallet address _ _ skey _ _) <- foldM getInputParty Nothing mtInputs >>= \case
    Nothing                          -> (defaultSubmitter,) <$> findWallet defaultSubmitter
    Just (M.Address network address) -> (findWalletByAddress =<< marloweAddressToCardanoAddress network address)
    Just (M.Role rn)                 -> case currency of
      Just cn -> findWalletByUniqueToken cn rn
      Nothing -> throwError "[autoRunTransaction] Contract requires a role currency which was not specified."

  connection <- view seConnection
  timeout <- timeoutForOnChainMode
  txBody <- runCli "[AutoRun] " $ autoRunTransactionImpl
      connection
      prev
      curr
      address
      [skey]
      C.TxMetadataNone
      timeout
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

findMarloweContract :: MonadError CliError m
                    => MonadState (ScriptState lang era) m
                    => ContractNickname   -- ^ The nickname.
                    -> m (MarloweContract lang era)
findMarloweContract nickname = do
  contracts <- use ssContracts
  liftCliMaybe
    ("[findMarloweContract] Marlowe contract structure was not found for a given nickname " <> show nickname <> ".")
    $ Map.lookup nickname contracts


useTemplate :: MonadError CliError m
            => MonadState (ScriptState lang era) m
            => MonadIO m
            => Maybe CurrencyNickname
            -> UseTemplate
            -> m M.Contract
useTemplate currency = do
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
    UseSwap{..} -> do utATimeout' <- toMarloweTimeout utATimeout (TruncateMilliseconds True)
                      utBTimeout' <- toMarloweTimeout utBTimeout (TruncateMilliseconds True)
                      let
                        aPartyRef = fromMaybe (WalletRef faucetNickname) (Just utAParty)
                        bPartyRef = fromMaybe (WalletRef faucetNickname) (Just utBParty)
                      aParty <- buildParty currency aPartyRef
                      bParty <- buildParty currency bPartyRef
                      CustomCurrency {ccCurrencySymbol=aCurrencySymbol} <- findCurrency utACurrencyNickname
                      CustomCurrency {ccCurrencySymbol=bCurrencySymbol} <- findCurrency utBCurrencyNickname

                      let
                        aToken = M.Token aCurrencySymbol utATokenName
                        bToken = M.Token bCurrencySymbol utBTokenName

                      makeContract $ swap
                          aParty
                          aToken
                          (EM.Constant utAAmount)
                          utATimeout'
                          bParty
                          bToken
                          (EM.Constant utBAmount)
                          utBTimeout'
                          EM.Close
    --UseEscrow{..} -> do paymentDeadline' <- toMarloweTimeout paymentDeadline
    --                    complaintDeadline' <- toMarloweTimeout complaintDeadline
    --                    disputeDeadline' <- toMarloweTimeout disputeDeadline
    --                    mediationDeadline' <- toMarloweTimeout mediationDeadline
    --                    makeContract $ escrow
    --                       (E.Constant price)
    --                       seller
    --                       buyer
    --                       mediator
    --                       paymentDeadline'
    --                       complaintDeadline'
    --                       disputeDeadline'
    --                       mediationDeadline'

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
    --UseCoveredCall{..} -> do issueDate' <- toMarloweTimeout issueDate
    --                         maturityDate' <- toMarloweTimeout maturityDate
    --                         settlementDate' <- toMarloweTimeout settlementDate
    --                         makeContract $ coveredCall
    --                             issuer
    --                             counterparty
    --                             Nothing
    --                             currency
    --                             underlying
    --                             (Constant strike)
    --                             (Constant amount)
    --                             issueDate'
    --                             maturityDate'
    --                             settlementDate'
    template -> throwError $ CliError $ "Template not implemented: " <> show template


buildParty :: MonadState (ScriptState lang era) m
           => MonadError CliError m
           => Maybe CurrencyNickname
           -> PartyRef
           -> m Party
buildParty currencyNickname = \case
  WalletRef nickname -> do
      wallet <- findWallet nickname
      uncurry M.Address <$> marloweAddressFromCardanoAddress (waAddress wallet)
  RoleRef token -> do
    -- Cosistency check
    currency <- case currencyNickname of
      Nothing -> fst <$> getCurrency
      Just cn -> pure cn
    void $ findWalletByUniqueToken currency token
    -- We are allowed to use this M.Role
    pure $ M.Role token


findWallet :: MonadError CliError m
           => MonadState (ScriptState lang era) m
           => WalletNickname
           -> m (Wallet era)
findWallet nickname = do
  wallets <- use ssWallets
  liftCliMaybe ("[findWallet] Unable to find wallet:" <> show nickname) $ Map.lookup nickname wallets

updateWallet :: MonadError CliError m
             => MonadState (ScriptState lang era) m
             => WalletNickname
             -> (Wallet era -> Wallet era)
             -> m ()
updateWallet nickname update = do
  wallet <- findWallet nickname
  let
    wallet' = update wallet
  modifying ssWallets $ Map.insert nickname wallet'

getFaucet :: MonadError CliError m
          => MonadState (ScriptState lang era) m
          => m (Wallet era)
getFaucet = do
  findWallet faucetNickname

updateFaucet :: MonadError CliError m
             => MonadState (ScriptState lang era) m
             => (Wallet era -> Wallet era)
             -> m ()
updateFaucet update = do
  updateWallet faucetNickname update


findWalletByUniqueToken :: MonadError CliError m
                        => MonadState (ScriptState lang era) m
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
  wallets <- use ssWallets
  walletInfo <- foldM step Nothing (Map.toList wallets)
  liftCliMaybe
    ("[findWalletByUniqueToken] Wallet not found for a given token: " <> show ccCurrencySymbol <> ":" <> show tokenName)
    walletInfo


findWalletByAddress :: MonadError CliError m
                => MonadState (ScriptState lang era) m
                => C.AddressInEra era
                -> m (WalletNickname, Wallet era)
findWalletByAddress address = do
  wallets <- use ssWallets
  let
    step _ (n, w) = if address == waAddress w
      then Just (n, w)
      else Nothing
    wallet = foldl' step Nothing (Map.toList wallets)

  liftCliMaybe
    ("[findWalletByPkh] Wallet not found for a given address: " <> show address)
    wallet


runCli :: MonadError CliError m
       => MonadReader (ScriptEnv era) m
       => String
       -> ReaderT (CliEnv era) m a
       -> m a
runCli msg action = do
  era <- view seEra
  withCliErrorMsg (mappend msg) $ runReaderT action (CliEnv era)


getCurrency :: MonadError CliError m
            => MonadState (ScriptState lang era) m
            => m (CurrencyNickname, Currency)
getCurrency = do
  currencies <- use ssCurrencies
  case Map.toList currencies of
    [c] -> pure c
    _   -> throwError "Ambiguous currency lookup."


findCurrency :: (MonadState (ScriptState lang era) m, MonadError CliError m) => CurrencyNickname -> m Currency
findCurrency nickname = do
  currencies <- use ssCurrencies
  liftCliMaybe ("[findCurrency] Unable to find currency:" <> show nickname) $ Map.lookup nickname currencies


selectWalletUTxOs :: MonadIO m
                  => MonadReader (ScriptEnv era) m
                  => MonadState (ScriptState lang era) m
                  => MonadError CliError m
                  => Either WalletNickname (Wallet era)
                  -> T.OutputQuery era (OutputQueryResult era)
                  -> m (T.OutputQueryResult era)
selectWalletUTxOs w q = do
  (waAddress -> address) <- either findWallet pure w
  connection <- view seConnection
  runCli "[selectUtxosImpl]" $ selectUtxosImpl connection address q


getWalletUTxO :: (MonadIO m, MonadReader (ScriptEnv era) m, MonadState (ScriptState lang era) m, MonadError CliError m) => Either WalletNickname (Wallet era) -> m (C.UTxO era)
getWalletUTxO w = do
  (waAddress -> address) <- either findWallet pure w
  connection <- view seConnection
  runCli "[getWalletUTxO]" $ queryInEra connection
    . C.QueryUTxO
    . C.QueryUTxOByAddress
    . S.singleton
    . T.toAddressAny'
    $ address


-- | Test a Marlowe contract.
scriptTest  :: forall era m
             . MonadError CliError m
            => IsShelleyBasedEra era
            => MonadIO m
            => ScriptDataSupportedInEra era
            -> ProtocolVersion
            -> CostModelParams
            -> LocalNodeConnectInfo CardanoMode  -- ^ The connection to the local node.
            -> Wallet era                        -- ^ Wallet which should be used as faucet.
            -> SlotConfig                        -- ^ The time and slot correspondence.
            -> ExecutionMode
            -> ScriptTest                        -- ^ The tests to be run.
            -> m ()                              -- ^ Action for running the tests.
scriptTest era protocolVersion costModel connection faucet slotConfig executionMode ScriptTest{..} =
  do
    liftIO $ putStrLn ""
    liftIO . putStrLn $ "***** Test " <> show stTestName <> " *****"

    let
      interpretLoop = for_ stScriptOperations \operation -> do
        logSoMsg SoName operation ""
        interpret operation
      printStats = PrintStats True
      scriptEnv = ScriptEnv connection costModel era protocolVersion slotConfig executionMode printStats

    void $ catchError
      (runReaderT (execStateT interpretLoop (scriptState faucet)) scriptEnv)
      $ \e -> do
        -- TODO: Clean up wallets and instances.
        liftIO (print e)
        liftIO (putStrLn "***** FAILED *****")
        throwError (e :: CliError)
    liftIO $ putStrLn "***** PASSED *****"


rewritePartyRefs :: MonadIO m
                 => MonadState (ScriptState lang era) m
                 => MonadError CliError m
                 => A.Value
                 -> m A.Value
rewritePartyRefs = A.rewriteBottomUp rewrite
  where
    rewrite = \case
      A.Object (KeyMap.toList -> [("address", A.String walletNickname)]) -> do
        wallet <- findWallet (WalletNickname $ Text.unpack walletNickname)
        (network, address) <- marloweAddressFromCardanoAddress $ waAddress wallet
        pure $ A.toJSON (M.Address network address)
      v -> do
        pure v

rewriteCurrencyRefs :: MonadIO m
                 => MonadState (ScriptState lang era) m
                 => MonadError CliError m
                 => A.Value
                 -> m A.Value
rewriteCurrencyRefs = A.rewriteBottomUp rewrite
  where
    rewrite = \case
      obj@(A.Object (KeyMap.toAscList -> props)) -> do
        case props of
          [ ("currency_symbol", A.String currencyNickname) , ("token_name", tokenName)] -> do
              let
                nickname = CurrencyNickname $ Text.unpack currencyNickname
              CustomCurrency { ccCurrencySymbol=P.CurrencySymbol cs } <- findCurrency nickname
              pure $ A.object
                [
                  ("currency_symbol", A.toJSON cs)
                , ("token_name", tokenName)
                ]
          _ -> pure obj
      v -> do
        pure v

decodeContractJSON :: MonadIO m
                   => MonadState (ScriptState lang era) m
                   => MonadError CliError m
                   => A.Value
                   -> m M.Contract
decodeContractJSON json = do
  contractJSON <- rewritePartyRefs json
  case A.fromJSON contractJSON of
    A.Error err -> throwError . CliError $ "[decodeContractJSON] contract json (" <> Text.unpack (A.renderValue json) <> ") parsing error: " <> show err
    A.Success contract -> pure contract


decodeInputJSON :: MonadIO m
                => MonadState (ScriptState lang era) m
                => MonadError CliError m
                => A.Value
                -> m M.Input
decodeInputJSON json = do
  json' <- rewritePartyRefs json
  json'' <- rewriteCurrencyRefs json'
  case A.fromJSON json'' of
    A.Error err -> throwError . CliError $ "[decodeInputJSON] contract input json (" <> Text.unpack (A.renderValue json'') <> ") parsing error: " <> show err
    A.Success input -> pure input


saveWallet :: C.IsCardanoEra era => WalletNickname -> Wallet era -> Maybe FilePath -> IO (FilePath, T.SigningKeyFile)
saveWallet walletNickname wallet dir = do
  let
    WalletNickname nickname = walletNickname
    Wallet { waAddress, waSigningKey=sskey } = wallet
    addrFileTpl = "wallet-of-" <> nickname <> "-" <> ".pay"
    skeyFileTpl = "wallet-of-" <> nickname <> "-" <> ".skey"
    writeEnvelope p c = C.writeFileTextEnvelope p Nothing c

  (addrFile, skeyFile) <- case dir of
    Nothing -> do
      a <- emptySystemTempFile addrFileTpl
      s <- emptySystemTempFile skeyFileTpl
      pure (a, s)

    Just dir' -> do
      a <- emptyTempFile dir' addrFileTpl
      s <- emptyTempFile dir' skeyFileTpl
      pure (a, s)
  writeFile addrFile (Text.unpack $ C.serialiseAddress waAddress)
  void $ either (writeEnvelope skeyFile) (writeEnvelope skeyFile) sskey
  pure (addrFile, T.SigningKeyFile skeyFile)


lovelaceToPlutusValue :: C.Lovelace -> P.Value
lovelaceToPlutusValue (C.Lovelace v) = P.singleton P.adaSymbol P.adaToken v


assetsToPlutusValue :: MonadError CliError m
                    => MonadState (ScriptState lang era) m
                    => Assets
                    -> m P.Value
assetsToPlutusValue (Assets (Map.toList -> assets)) = do
  let
    assetToValue AdaAsset amount = pure $ P.singleton P.adaSymbol P.adaToken amount
    assetToValue (AssetId currencyNickname tokenName) amount = do
      Currency { ccCurrencySymbol } <- findCurrency currencyNickname
      pure $ P.singleton ccCurrencySymbol tokenName amount

  fold <$> for assets (uncurry assetToValue)


foldMapFlipped :: Monoid b => Foldable f => f a -> (a -> b) -> b
foldMapFlipped = flip foldMap

foldMapMFlipped :: Monoid b => Monad m => Foldable f => f a -> (a -> m b) -> m b
foldMapMFlipped = flip foldMapM

anyFlipped :: [a] -> (a -> Bool) -> Bool
anyFlipped = flip any

