{-# LANGUAGE AllowAmbiguousTypes #-}
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
{-# LANGUAGE ViewPatterns #-}


module Language.Marlowe.CLI.Test.Wallet.Interpret
  where

import Cardano.Api
  ( AddressInEra
  , AsType(AsPaymentKey)
  , CardanoMode
  , Key(getVerificationKey, verificationKeyHash)
  , LocalNodeConnectInfo(LocalNodeConnectInfo)
  , Lovelace
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
import Control.Lens (FunctorWithIndex(imap), ifor, makeLenses, modifying, use, view)
import Control.Lens.Combinators (_1, _2)
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
import Data.List.NonEmpty (NonEmpty((:|)))
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
  ( Asset(Asset)
  , AssetId(AdaAsset, AssetId)
  , AssetsBalance(AssetsBalance)
  , Currencies(Currencies)
  , Currency(Currency, ccCurrencySymbol, ccIssuer)
  , CurrencyNickname
  , InterpretMonad
  , SomeTxBody(..)
  , TokenAssignment(TokenAssignment)
  , Wallet(Wallet, waAddress, waBalanceCheckBaseline, waMintedTokens, waSigningKey, waSubmittedTransactions)
  , WalletNickname(WalletNickname)
  , WalletOperation(..)
  , Wallets(Wallets)
  , adaToken
  , checkBalance
  , connectionL
  , currenciesL
  , emptyWallet
  , eraL
  , executionModeL
  , faucetNickname
  , printStatsL
  , walletsL
  )
import Language.Marlowe.CLI.Transaction
  (buildBody, buildBodyWithContent, buildFaucetImpl, buildMintingImpl, queryUtxos, submitBody, submitBody')
import Language.Marlowe.CLI.Types
  ( AnUTxO(AnUTxO)
  , CliEnv
  , CliError(CliError)
  , CurrencyIssuer(CurrencyIssuer)
  , MarloweScriptsRefs
  , MarloweTransaction(MarloweTransaction, mtInputs)
  , PayFromScript(PayFromScript)
  , PrintStats
  , Seconds
  , SomePaymentSigningKey
  , SomeTimeout
  , defaultCoinSelectionStrategy
  )
import qualified Language.Marlowe.Core.V1.Semantics.Types as M
import qualified Language.Marlowe.Extended.V1 as E
import qualified Language.Marlowe.Runtime.Cardano.Api as Runtime.Cardano.Api
import qualified Language.Marlowe.Runtime.Core.Api as Runtime.Core.Api
import Ledger.Orphans ()
import Plutus.ApiCommon (ProtocolVersion)
import Plutus.V1.Ledger.Api (CostModelParams, CurrencySymbol, TokenName)
import Plutus.V1.Ledger.SlotConfig (SlotConfig)
import qualified Plutus.V1.Ledger.Value as P
import Text.Read (readMaybe)

import Contrib.Data.Foldable (foldMapFlipped, foldMapMFlipped)
import Control.Concurrent.STM (TChan, TVar)
import Control.Monad (foldM, forM, forM_, unless, void, when)
import Control.Monad.Error.Class (MonadError(throwError))
import Control.Monad.Reader.Class (asks)
import Control.Monad.State.Class (MonadState, gets, modify)
import Data.Functor ((<&>))
import qualified Data.List.NonEmpty as List.NonEmpty
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy)
import Data.Set (Set)
import qualified Data.Text as Text
import Data.Tuple.Extra (uncurry3)
import qualified Language.Marlowe.CLI.Cardano.Api.Value as CV
import Language.Marlowe.CLI.IO (liftCliMaybe, queryInEra)
import Language.Marlowe.CLI.Test.CLI.Monad (runCli, runLabeledCli)
import Language.Marlowe.CLI.Test.Contract.ParametrizedMarloweJSON
  (ParametrizedMarloweJSON(ParametrizedMarloweJSON), decodeParametrizedContractJSON, decodeParametrizedInputJSON)
import Language.Marlowe.CLI.Test.ExecutionMode (ExecutionMode, skipInSimluationMode, toSubmitMode)
import Language.Marlowe.CLI.Test.Log (logLabeledMsg, logTraceMsg, throwLabeledError, throwTraceError)
import qualified Language.Marlowe.CLI.Types as T
import Language.Marlowe.Cardano (marloweNetworkFromLocalNodeConnectInfo)
import Language.Marlowe.Protocol.Sync.Client (MarloweSyncClient)
import qualified Language.Marlowe.Runtime.App.Stream as Runtime.App
import Language.Marlowe.Runtime.App.Types (Client)
import Language.Marlowe.Runtime.ChainSync.Api (SlotNo)
import Language.Marlowe.Runtime.Core.Api (ContractId, MarloweVersionTag(V1))
import Ledger.Tx.CardanoAPI (fromCardanoPolicyId, fromCardanoValue)
import Observe.Event.Backend (EventBackend)
import Observe.Event.Dynamic (DynamicEventSelector)
import Observe.Event.Render.JSON.Handle (JSONRef)
import Plutus.V1.Ledger.Value (valueOf)
import qualified Plutus.V1.Ledger.Value as PV
import qualified Plutus.V2.Ledger.Api as P
import PlutusTx.Monoid (Group(inv))
import System.IO.Temp (emptySystemTempFile, emptyTempFile)

findWallet
  :: InterpretMonad env st m era
  => WalletNickname
  -> m (Wallet era)
findWallet nickname = do
  use walletsL >>= \(Wallets wallets) ->
    liftCliMaybe ("[findWallet] Unable to find wallet:" <> show nickname) $ Map.lookup nickname wallets

updateWallet
  :: InterpretMonad env st m era
  => WalletNickname
  -> (Wallet era -> Wallet era)
  -> m ()
updateWallet nickname update = do
  wallet <- findWallet nickname
  let
    wallet' = update wallet
  modifying walletsL \(Wallets wallets) -> Wallets (Map.insert nickname wallet' wallets)

getFaucet
  :: InterpretMonad env st m era
  => m (Wallet era)
getFaucet = do
  findWallet faucetNickname

updateFaucet
  :: InterpretMonad env st m era
  => (Wallet era -> Wallet era)
  -> m ()
updateFaucet update = do
  updateWallet faucetNickname update

fetchWalletUTxOs
  :: InterpretMonad env st m era
  => Wallet era
  -> m (C.UTxO era)
fetchWalletUTxOs (waAddress -> address) = do
  connection <- view connectionL
  era <- view eraL
  runCli era "[fetchWalletUTxO]" $ queryInEra connection
    . C.QueryUTxO
    . C.QueryUTxOByAddress
    . S.singleton
    . T.toAddressAny'
    $ address

getSingletonCurrency
  :: InterpretMonad env st m era
  => m (CurrencyNickname, Currency)
getSingletonCurrency = do
  Currencies currencies <- use currenciesL
  case Map.toList currencies of
    [c] -> pure c
    _   -> throwError "Ambigious currency lookup."

findCurrency
  :: InterpretMonad env st m era
  => CurrencyNickname
  -> m Currency
findCurrency nickname = do
  (Currencies currencies) <- use currenciesL
  liftCliMaybe ("[findCurrency] Unable to find currency:" <> show nickname) $ Map.lookup nickname currencies

findCurrencyBySymbol
  :: InterpretMonad env st m era
  => CurrencySymbol
  -> m (CurrencyNickname, Currency)
findCurrencyBySymbol currencySymbol = do
  (Currencies currencies) <- use currenciesL
  let
    possibleCurrency = find
      (\(n, c) -> ccCurrencySymbol c == currencySymbol) (Map.toList currencies)
  liftCliMaybe ("[findCurrencyBySymbol] Unable to find currency:" <> show currencySymbol) possibleCurrency

findWalletByAddress
  :: InterpretMonad env st m era
  => C.AddressInEra era
  -> m (WalletNickname, Wallet era)
findWalletByAddress address = do
  Wallets wallets <- use walletsL
  let
    wallet = find (\(_, w) -> address == waAddress w) (Map.toList wallets)

  liftCliMaybe
    ("[findWalletByPkh] Wallet not found for a given address: " <> show address <> " in wallets: " <> show wallets)
    wallet

findWalletByUniqueToken
  :: InterpretMonad env st m era
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
  Wallets wallets <- use walletsL
  walletInfo <- foldM step Nothing (Map.toList wallets)
  liftCliMaybe
    ("[findWalletByUniqueToken] Wallet not found for a given token: " <> show ccCurrencySymbol <> ":" <> show tokenName)
    walletInfo

assetToPlutusValue
  :: InterpretMonad env st m era
  => Asset
  -> m P.Value
assetToPlutusValue (Asset AdaAsset amount) = pure $ P.singleton P.adaSymbol P.adaToken amount
assetToPlutusValue (Asset (AssetId currencyNickname tokenName) amount) = do
  Currency { ccCurrencySymbol } <- findCurrency currencyNickname
  pure $ P.singleton ccCurrencySymbol tokenName amount

plutusValueToAssets
  :: InterpretMonad env st m era
  => P.Value
  -> m [Asset]
plutusValueToAssets value = do
  let
    valueToAsset (currencySymbol, tokenName, _) | currencySymbol == P.adaSymbol && tokenName == P.adaToken = do
      pure (Asset AdaAsset (P.fromBuiltin $ P.valueOf value currencySymbol tokenName))
    valueToAsset (cCurrencySymbol, tokenName, amount) = do
      let
      (currencyNickname, currency) <- findCurrencyBySymbol cCurrencySymbol
      pure (Asset (AssetId currencyNickname tokenName) amount)
  for (P.flattenValue value) valueToAsset

assetIdToToken
  :: InterpretMonad env st m era
  => AssetId
  -> m M.Token
assetIdToToken (AssetId currencyNickname currencyToken) = do
  Currency { ccCurrencySymbol=currencySymbol } <- findCurrency currencyNickname
  pure $ M.Token currencySymbol currencyToken
assetIdToToken AdaAsset = pure adaToken

decodeInputJSON
  :: InterpretMonad env st m era
  => ParametrizedMarloweJSON
  -> m M.Input
decodeInputJSON json = do
  currencies <- use currenciesL
  wallets <- use walletsL
  network <- view connectionL <&> marloweNetworkFromLocalNodeConnectInfo
  case decodeParametrizedInputJSON network wallets currencies json of
    Left err -> throwError $ CliError $ "Failed to decode input: " <> show err
    Right i  -> pure i

decodeContractJSON
  :: InterpretMonad env st m era
  => ParametrizedMarloweJSON
  -> m M.Contract
decodeContractJSON json = do
  currencies <- use currenciesL
  wallets <- use walletsL
  network <- view connectionL <&> marloweNetworkFromLocalNodeConnectInfo
  case decodeParametrizedContractJSON network wallets currencies json of
    Left err -> throwError $ CliError $ "Failed to decode contract: " <> show err
    Right c  -> pure c


interpret
  :: forall env era st m
   . C.IsShelleyBasedEra era
  => InterpretMonad env st m era
  => WalletOperation
  -> m ()
interpret so@CheckBalance {..} =
  view executionModeL >>= skipInSimluationMode so do
    wallet@Wallet {..} <- findWallet woWalletNickname
    utxos <- fetchWalletUTxOs wallet :: m (C.UTxO era)
    let
      onChainTotal = CV.toPlutusValue $ foldMap txOutValueValue . Map.elems . C.unUTxO $ utxos

      txFee :: forall era'. C.IsShelleyBasedEra era' => TxBody era' -> Lovelace
      txFee (C.TxBody txBodyContent) =
        case C.txFee txBodyContent of
          C.TxFeeExplicit _ lovelace -> lovelace
          C.TxFeeImplicit _ -> C.Lovelace 0

      fees = waSubmittedTransactions `foldMapFlipped` \case
        (BabbageTxBody txBody) -> txFee txBody
        (SomeTxBody txBody) -> txFee txBody

    logLabeledMsg so $ "Checking balance of wallet:" <> show case woWalletNickname of WalletNickname n -> n
    logLabeledMsg so $ "Number of already submitted transactions: " <> show (length waSubmittedTransactions)
    logLabeledMsg so $ "Total transaction fees amount: " <> do
      let
        C.Lovelace amount = fees
      show ((fromInteger amount / 1_000_000) :: F.Micro) <> " ADA"

    actualBalance <- plutusValueToAssets $ onChainTotal <> lovelaceToPlutusValue fees <> inv waBalanceCheckBaseline
    let
      AssetsBalance expectedBalance = woBalance

    for_ actualBalance \(Asset assetId v) -> do
      case Map.lookup assetId expectedBalance of
        Nothing -> do
          throwLabeledError so $ "Unexpected asset in balance check:" <> show assetId
        Just expectedValue -> do
          unless (checkBalance expectedValue v) do
            logLabeledMsg so $ "Actual balance of asset:" <> show assetId <> " is:" <> show v
            logLabeledMsg so $ "Expected balance of asset:" <> show assetId <> " is: " <> show expectedValue
            throwLabeledError so $ "Balance check failed for an asset:" <> show assetId

    for_ (Map.toList expectedBalance) \(assetId, expectedValue) -> do
      let
        actualAssets = map (\(Asset k _) -> k) actualBalance
      unless (assetId `elem` actualAssets) do
        logLabeledMsg so $ "Expected balance of asset:" <> show assetId <> " is:" <> show expectedValue
        throwLabeledError so $ "Expected asset is missing in balance check:" <> show assetId

interpret so@CreateWallet {..} = do
  skey <- liftIO $ generateSigningKey AsPaymentKey
  (connection :: LocalNodeConnectInfo CardanoMode) <- view connectionL
  let
    vkey = getVerificationKey skey
    LocalNodeConnectInfo {localNodeNetworkId} = connection
    (address :: AddressInEra era) = makeShelleyAddressInEra localNodeNetworkId (PaymentCredentialByKey (verificationKeyHash vkey)) NoStakeAddress
    WalletNickname rawNickname = woWalletNickname
  let wallet = emptyWallet address (Left skey)
  logLabeledMsg so $ "Wallet " <> show rawNickname <> " created with an address: " <> Text.unpack (C.serialiseAddress address)

  (addrFile, T.SigningKeyFile skeyFile) <- liftIO $ saveWalletFiles woWalletNickname wallet Nothing
  logLabeledMsg so $ "Wallet info stored in " <> addrFile <> " and " <> skeyFile

  modifying walletsL \(Wallets wallets) -> Wallets (Map.insert woWalletNickname wallet wallets)

interpret so@BurnAll {..} = do
  Currencies (Map.toList -> currencies) <- use currenciesL
  for_ currencies \(currencyNickname, currency) -> do
    Currency { ccCurrencySymbol, ccIssuer } <- findCurrency currencyNickname
    (Wallets allWallets :: Wallets era) <- use walletsL
    Wallet { waAddress=issuerAddress, waSigningKey=issuerSigningKey } <- findWallet ccIssuer
    let
      providers = Map.elems allWallets <&> \Wallet { waAddress, waSigningKey } -> (waAddress, waSigningKey)
      currencyIssuer = T.CurrencyIssuer issuerAddress issuerSigningKey
      mintingAction = T.BurnAll currencyIssuer $ (issuerAddress, issuerSigningKey) :| filter ((/=) issuerAddress. fst) providers
    connection <- view connectionL
    submitMode <- view executionModeL <&> toSubmitMode
    printStats <- view printStatsL
    era <- view eraL
    (burningTx, _) <- runLabeledCli era so $ buildMintingImpl
      connection
      mintingAction
      woMetadata
      Nothing
      submitMode
      printStats

    -- We burn *all* the tokens so we have to drop them from the wallet balance baseline as well.
    -- We should not include them in this reference of untracked funds from the chain because they
    -- are not on the chain anymore.
    for_ (Map.toList allWallets) \(walletNickname, Wallet { waBalanceCheckBaseline }) -> do
      let
        balanceCheckBaseline' = do
          let
            check (cs, _, _) = cs /= ccCurrencySymbol
          foldMap (uncurry3 P.singleton) . filter check . P.flattenValue $ waBalanceCheckBaseline

      updateWallet walletNickname \(wallet@Wallet {} :: Wallet era)->
        wallet { waBalanceCheckBaseline = balanceCheckBaseline' }

    updateWallet ccIssuer \issuer@Wallet {waSubmittedTransactions} ->
      issuer { waSubmittedTransactions = SomeTxBody burningTx : waSubmittedTransactions }

interpret FundWallets {..} = do
  let
    values = [ C.lovelaceToValue v | v <- woValues ]

  (Wallet faucetAddress _ _ faucetSigningKey _ :: Wallet era) <- getFaucet
  addresses <- for woWalletNicknames \walletNickname -> do
    (Wallet address _ _ _ _) <- findWallet walletNickname
    pure address
  connection <- view connectionL
  submitMode <- view executionModeL <&> toSubmitMode
  era <- view eraL
  txBody <- runCli era "[FundWallet] " $ buildFaucetImpl
    connection
    (Just values)
    addresses
    faucetAddress
    faucetSigningKey
    defaultCoinSelectionStrategy
    submitMode

  updateFaucet \faucet@(Wallet _ _ _ _ faucetTransactions) ->
    faucet { waSubmittedTransactions = SomeTxBody txBody : faucetTransactions }

interpret so@Mint {..} = do
  let
    issuerNickname = fromMaybe faucetNickname woIssuer

  (Currencies currencies) <- use currenciesL
  case Map.lookup woCurrencyNickname currencies of
    Just Currency { ccIssuer=ci } -> when (ci /= issuerNickname)  do
      throwError "Currency with a given nickname already exist and is minted by someone else."
    Nothing -> pure ()

  ifor currencies \currencyNickname Currency { ccIssuer } -> when (ccIssuer == issuerNickname && currencyNickname /= woCurrencyNickname) do
    throwError "Current minting strategy mints unique currency per issuer. You can't mint multiple currencies with the same issuer."

  Wallet issuerAddress _ _ issuerSigningKey _ :: Wallet era <- findWallet issuerNickname
  (tokenDistribution, walletAssignemnts) <- unzip <$> forM woTokenDistribution \(TokenAssignment owner tokenName amount) -> do
    Wallet destAddress _ _ _ _ <- findWallet owner
    pure ((tokenName, amount, destAddress, Just woMinLovelace), (owner, tokenName, amount))

  logLabeledMsg so $ "Minting currency " <> show woCurrencyNickname <> " with tokens distribution: " <> show woTokenDistribution
  tokenDistribution' <- maybe (throwLabeledError so "Token distribution shouldn't be empty") pure $ List.NonEmpty.nonEmpty tokenDistribution
  let
    mintingAction = T.Mint
      (CurrencyIssuer issuerAddress issuerSigningKey)
      tokenDistribution'

  connection <- view connectionL
  submitMode <- view executionModeL <&> toSubmitMode
  printStats <- view printStatsL
  era <- view eraL
  (mintingTx, policy) <- runCli era "[Mint] " $ buildMintingImpl
    connection
    mintingAction
    woMetadata
    Nothing
    submitMode
    printStats

  logLabeledMsg so $ "This currency symbol is " <> show policy
  let
    currencySymbol = PV.mpsSymbol . fromCardanoPolicyId $ policy
    currency = Currency currencySymbol issuerNickname policy
    C.TxBody mintingTxContent = mintingTx

  forM_ walletAssignemnts \(walletNickname, tokenName, amount) -> do
    updateWallet walletNickname \(wallet@Wallet {..} :: Wallet era) ->  do
      let
        tokenValue = PV.singleton currencySymbol tokenName (toInteger amount)
      wallet
        { waMintedTokens = tokenValue <> waMintedTokens
        }

  updateWallet issuerNickname \issuer@Wallet {..} -> do
    let
      C.TxBody c = mintingTx
      minAdas = C.txOuts c `foldMapFlipped` \(C.TxOut addr value _ _) ->
        if addr /= issuerAddress
          then lovelaceToPlutusValue . C.selectLovelace . C.txOutValueToValue $ value
          else mempty
    issuer
      { waSubmittedTransactions = SomeTxBody mintingTx : waSubmittedTransactions
      }
  modifying currenciesL \(Currencies currencies) ->
    Currencies $ Map.insert woCurrencyNickname currency currencies

interpret SplitWallet {..} = do
  Wallet address _ _ skey _ :: Wallet era <- findWallet woWalletNickname
  connection <- view connectionL
  submitMode <- view executionModeL <&> toSubmitMode
  let
    values = [ C.lovelaceToValue v | v <- woValues ]

  era <- view eraL
  void $ runCli era "[createCollaterals] " $ buildFaucetImpl
    connection
    (Just values)
    [address]
    address
    skey
    defaultCoinSelectionStrategy
    submitMode

interpret wo@ReturnFunds{} = do
  Wallets wallets <- use walletsL
  let
    step :: (WalletNickname, Wallet era) -> m ([(C.TxIn, C.TxOut C.CtxUTxO era)], [SomePaymentSigningKey], C.Value)
    step (walletNickname, wallet@Wallet { waSigningKey }) = do
      if walletNickname == faucetNickname
      then
        pure ([], [], mempty)
      else do
        C.UTxO (Map.toList -> utxos) <- (fetchWalletUTxOs wallet :: m (C.UTxO era))
        let
          subtotal = foldMap (txOutValueValue . snd) utxos
          tokens = subtotal <> C.negateValue (C.lovelaceToValue . C.selectLovelace $ subtotal)
        pure (utxos, [waSigningKey], tokens)
  (utxos, signingKeys, tokens) <- foldMapMFlipped (Map.toList wallets) step

  Wallet { waAddress } <- getFaucet
  let
    total = foldMap (txOutValueValue . snd) utxos
    inputs = map fst utxos
    outputs = []
    changeAddress = waAddress

  if total == mempty
  then
    logLabeledMsg wo "Nothing to refund"
  else do
    connection <- view connectionL
    era <- view eraL
    (bodyContent, body) <- runLabeledCli era wo $ buildBodyWithContent
      connection
      ([] :: [PayFromScript C.PlutusScriptV1])
      Nothing
      []
      inputs
      outputs
      Nothing
      changeAddress
      Nothing
      []
      C.TxMintNone
      C.TxMetadataNone
      False
      False

    logLabeledMsg wo $ "Returning funds to the faucet: " <> show total

    submitMode <- view executionModeL <&> toSubmitMode
    case submitMode of
      T.DoSubmit timeout -> do
        runLabeledCli era wo $ submitBody' connection body bodyContent changeAddress signingKeys timeout
        let
          wallets' = Wallets $ flip imap wallets \walletNickname wallet@Wallet { waMintedTokens } -> do
            if walletNickname == faucetNickname
            then
              wallet { waMintedTokens = waMintedTokens <> fromCardanoValue tokens }
            else
              wallet { waMintedTokens = mempty }
        modifying walletsL (const wallets')
      T.DontSubmit -> do
        pure ()

saveWalletFiles :: C.IsCardanoEra era => WalletNickname -> Wallet era -> Maybe FilePath -> IO (FilePath, T.SigningKeyFile)
saveWalletFiles walletNickname wallet dir = do
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

