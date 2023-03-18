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

-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Type safe list of transactions representing on chain Marlowe execution.
--
-----------------------------------------------------------------------------

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
import Control.Lens (makeLenses, modifying, use, view)
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
import Data.Foldable (fold, for_)
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
  ( AssetId(AdaAsset, AssetId)
  , Assets(Assets)
  , Currencies(Currencies)
  , Currency(Currency, ccCurrencySymbol, ccIssuer)
  , CurrencyNickname
  , InterpretMonad
  , TokenAssignment(TokenAssignment)
  , Wallet(Wallet, waAddress, waBalanceCheckBaseline, waMintedTokens, waSigningKey, waSubmittedTransactions)
  , WalletNickname(WalletNickname)
  , WalletOperation(BurnAll, CheckBalance, CreateWallet, FundWallets, Mint, SplitWallet, soBalance, soCurrencyNickname, soIssuer, soMetadata, soMinLovelace, soTokenDistribution, soValues, soWalletNickname, soWalletNicknames)
  , Wallets(Wallets)
  , emptyWallet
  , faucetNickname
  , ieConnection
  , ieEra
  , ieExecutionMode
  , iePrintStats
  , isCurrencies
  , isWallets
  )
import Language.Marlowe.CLI.Transaction (buildFaucetImpl, buildMintingImpl, queryUtxos)
import Language.Marlowe.CLI.Types
  ( CliEnv
  , CliError
  , CurrencyIssuer(CurrencyIssuer)
  , MarloweScriptsRefs
  , MarloweTransaction(MarloweTransaction, mtInputs)
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

import Contrib.Data.Foldable (foldMapFlipped)
import Control.Concurrent.STM (TChan, TVar)
import Control.Monad (forM, forM_, void, when)
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
import Language.Marlowe.CLI.Test.ExecutionMode (ExecutionMode, skipInSimluationMode, toSubmitMode)
import Language.Marlowe.CLI.Test.Log (logLabeledMsg, logTraceMsg, throwLabeledError, throwTraceError)
import qualified Language.Marlowe.CLI.Types as T
import Language.Marlowe.Protocol.Sync.Client (MarloweSyncClient)
import qualified Language.Marlowe.Runtime.App.Stream as Runtime.App
import Language.Marlowe.Runtime.App.Types (Client)
import Language.Marlowe.Runtime.ChainSync.Api (SlotNo)
import Language.Marlowe.Runtime.Core.Api (ContractId, MarloweVersionTag(V1))
import Ledger.Tx.CardanoAPI (fromCardanoPolicyId)
import Observe.Event.Backend (EventBackend)
import Observe.Event.Dynamic (DynamicEventSelector)
import Observe.Event.Render.JSON.Handle (JSONRef)
import qualified Plutus.V1.Ledger.Value as PV
import PlutusTx.Monoid (Group(inv))
import System.IO.Temp (emptySystemTempFile, emptyTempFile)

findWallet
  :: InterpretMonad m era
  => WalletNickname
  -> m (Wallet era)
findWallet nickname = do
  use isWallets >>= \(Wallets wallets) ->
    liftCliMaybe ("[findWallet] Unable to find wallet:" <> show nickname) $ Map.lookup nickname wallets

updateWallet
  :: InterpretMonad m era
  => WalletNickname
  -> (Wallet era -> Wallet era)
  -> m ()
updateWallet nickname update = do
  wallet <- findWallet nickname
  let
    wallet' = update wallet
  modifying isWallets \(Wallets wallets) -> Wallets (Map.insert nickname wallet' wallets)

getFaucet
  :: InterpretMonad m era
  => m (Wallet era)
getFaucet = do
  findWallet faucetNickname

updateFaucet
  :: InterpretMonad m era
  => (Wallet era -> Wallet era)
  -> m ()
updateFaucet update = do
  updateWallet faucetNickname update

-- findWalletByUniqueToken :: MonadError CliError m
--                         => MonadState (ScriptState lang era) m
--                         => CurrencyNickname
--                         -> TokenName
--                         -> m (WalletNickname, Wallet era)
-- findWalletByUniqueToken currencyNickname tokenName = do
--   Currency {..} <- findCurrency currencyNickname
--   let
--     check value = valueOf value ccCurrencySymbol tokenName == 1
--     step Nothing (n, wallet@(waMintedTokens -> tokens)) = pure $ if check tokens
--       then Just (n, wallet)
--       else Nothing
--     step res c@(n, waMintedTokens -> tokens) = if check tokens
--       then case res of
--         Just (n', _) ->
--           throwError $ CliError $ "[findByUniqueToken] Token is not unique - found in two wallets: " <> show n <> " and " <> show n' <> "."
--         Nothing ->
--           pure (Just c)
--       else pure res
--   wallets <- use ssWallets
--   walletInfo <- foldM step Nothing (Map.toList wallets)
--   liftCliMaybe
--     ("[findWalletByUniqueToken] Wallet not found for a given token: " <> show ccCurrencySymbol <> ":" <> show tokenName)
--     walletInfo
--
--
-- findWalletByAddress :: MonadError CliError m
--                 => MonadState (ScriptState lang era) m
--                 => C.AddressInEra era
--                 -> m (WalletNickname, Wallet era)
-- findWalletByAddress address = do
--   wallets <- use ssWallets
--   let
--     wallet = find (\(_, w) -> address == waAddress w) (Map.toList wallets)
--
--   liftCliMaybe
--     ("[findWalletByPkh] Wallet not found for a given address: " <> show address <> " in wallets: " <> show wallets)
--     wallet
--
--
--
-- selectWalletUTxOs :: MonadIO m
--                   => MonadReader (ScriptEnv era) m
--                   => MonadState (ScriptState lang era) m
--                   => MonadError CliError m
--                   => Either WalletNickname (Wallet era)
--                   -> T.OutputQuery era (OutputQueryResult era)
--                   -> m (T.OutputQueryResult era)
-- selectWalletUTxOs w q = do
--   (waAddress -> address) <- either findWallet pure w
--   connection <- view seConnection
--   runCli "[selectUtxosImpl]" $ selectUtxosImpl connection address q
--
--

fetchWalletUTxOs
  :: InterpretMonad m era
  => Wallet era
  -> m (C.UTxO era)
fetchWalletUTxOs (waAddress -> address) = do
  connection <- view ieConnection
  era <- view ieEra
  runCli era "[fetchWalletUTxO]" $ queryInEra connection
    . C.QueryUTxO
    . C.QueryUTxOByAddress
    . S.singleton
    . T.toAddressAny'
    $ address

getSingletonCurrency
  :: InterpretMonad m era
  => m (CurrencyNickname, Currency)
getSingletonCurrency = do
  Currencies currencies <- use isCurrencies
  case Map.toList currencies of
    [c] -> pure c
    _   -> throwError "Ambigious currency lookup."

findCurrency
  :: InterpretMonad m era
  => CurrencyNickname
  -> m Currency
findCurrency nickname = do
  (Currencies currencies) <- use isCurrencies
  liftCliMaybe ("[findCurrency] Unable to find currency:" <> show nickname) $ Map.lookup nickname currencies

assetsToPlutusValue
  :: InterpretMonad m era
  => Assets
  -> m P.Value
assetsToPlutusValue (Assets (Map.toList -> assets)) = do
  let
    assetToValue AdaAsset amount = pure $ P.singleton P.adaSymbol P.adaToken amount
    assetToValue (AssetId currencyNickname tokenName) amount = do
      Currency { ccCurrencySymbol } <- findCurrency currencyNickname
      pure $ P.singleton ccCurrencySymbol tokenName amount
  fold <$> for assets (uncurry assetToValue)

interpret
  :: forall env era st m
   . C.IsShelleyBasedEra era
  => InterpretMonad m era
  => WalletOperation
  -> m ()
interpret so@CheckBalance {..} =
  view ieExecutionMode >>= skipInSimluationMode so do
    wallet@Wallet {..} <- findWallet soWalletNickname
    utxos <- (fetchWalletUTxOs wallet :: m (C.UTxO era))
    let
      onChainTotal = CV.toPlutusValue $ foldMap txOutValueValue . Map.elems . C.unUTxO $ utxos

      fees = waSubmittedTransactions `foldMapFlipped` \(C.TxBody txBodyContent) -> do
        case C.txFee txBodyContent of
          C.TxFeeExplicit _ lovelace -> lovelace
          C.TxFeeImplicit _ -> C.Lovelace 0

      actualBalance = onChainTotal <> lovelaceToPlutusValue fees <> inv waBalanceCheckBaseline

    logLabeledMsg so $ "Number of already submitted transactions: " <> show (length waSubmittedTransactions)
    logLabeledMsg so $ "Total transaction fees amount: " <> do
      let
        C.Lovelace amount = fees
      show ((fromInteger amount / 1_000_000) :: F.Micro) <> " ADA"

    expectedBalance <- assetsToPlutusValue soBalance
    when (expectedBalance /= actualBalance) do
      throwLabeledError so $ "Balance check difference: expectedBalance - actualBalance = " <> show (expectedBalance <> inv actualBalance)

interpret so@CreateWallet {..} = do
  skey <- liftIO $ generateSigningKey AsPaymentKey
  (connection :: LocalNodeConnectInfo CardanoMode) <- view ieConnection
  let
    vkey = getVerificationKey skey
    LocalNodeConnectInfo {localNodeNetworkId} = connection
    (address :: AddressInEra era) = makeShelleyAddressInEra localNodeNetworkId (PaymentCredentialByKey (verificationKeyHash vkey)) NoStakeAddress
    WalletNickname rawNickname = soWalletNickname
  let wallet = emptyWallet address (Left skey)
  logLabeledMsg so $ "Wallet " <> show rawNickname <> " created with an address: " <> Text.unpack (C.serialiseAddress address)

  (addrFile, T.SigningKeyFile skeyFile) <- liftIO $ saveWalletFiles soWalletNickname wallet Nothing
  logLabeledMsg so $ "Wallet info stored in " <> addrFile <> " and " <> skeyFile

  modifying isWallets \(Wallets wallets) -> Wallets (Map.insert soWalletNickname wallet wallets)

interpret so@BurnAll {..} = do
  Currency { ccCurrencySymbol, ccIssuer } <- findCurrency soCurrencyNickname
  (Wallets allWallets :: Wallets era) <- use isWallets
  Wallet { waAddress=issuerAddress, waSigningKey=issuerSigningKey } <- findWallet ccIssuer
  let
    providers = Map.elems allWallets <&> \Wallet { waAddress, waSigningKey } -> (waAddress, waSigningKey)
    currencyIssuer = T.CurrencyIssuer issuerAddress issuerSigningKey
    mintingAction = T.BurnAll currencyIssuer $ (issuerAddress, issuerSigningKey) :| filter ((/=) issuerAddress. fst) providers
  connection <- view ieConnection
  submitMode <- view ieExecutionMode <&> toSubmitMode
  printStats <- view iePrintStats
  era <- view ieEra
  (burningTx, _) <- runLabeledCli era so $ buildMintingImpl
    connection
    mintingAction
    soMetadata
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
    issuer { waSubmittedTransactions = burningTx : waSubmittedTransactions }

interpret FundWallets {..} = do
  let
    values = [ C.lovelaceToValue v | v <- soValues ]

  (Wallet faucetAddress _ _ faucetSigningKey _ :: Wallet era) <- getFaucet
  addresses <- for soWalletNicknames \walletNickname -> do
    (Wallet address _ _ _ _) <- findWallet walletNickname
    pure address
  connection <- view ieConnection
  submitMode <- view ieExecutionMode <&> toSubmitMode
  era <- view ieEra
  txBody <- runCli era "[FundWallet] " $ buildFaucetImpl
    connection
    (Just values)
    addresses
    faucetAddress
    faucetSigningKey
    defaultCoinSelectionStrategy
    submitMode

  updateFaucet \faucet@(Wallet _ _ _ _ faucetTransactions) ->
    faucet { waSubmittedTransactions = txBody : faucetTransactions }

interpret so@Mint {..} = do
  let
    issuerNickname = fromMaybe faucetNickname soIssuer

  (Currencies currencies) <- use isCurrencies
  case Map.lookup soCurrencyNickname currencies of
    Just Currency { ccIssuer=ci } -> when (ci /= issuerNickname)  do
      throwError "Currency with a given nickname already exist and is minted by someone else."
    Nothing -> pure ()

  Wallet issuerAddress _ _ issuerSigningKey _ :: Wallet era <- findWallet issuerNickname
  (tokenDistribution, walletAssignemnts) <- unzip <$> forM soTokenDistribution \(TokenAssignment owner tokenName amount) -> do
    Wallet destAddress _ _ _ _ <- findWallet owner
    pure ((tokenName, amount, destAddress, Just soMinLovelace), (owner, tokenName, amount))

  logLabeledMsg so $ "Minting currency " <> show soCurrencyNickname <> " with tokens distribution: " <> show soTokenDistribution
  tokenDistribution' <- maybe (throwLabeledError so "Token distribution shouldn't be empty") pure $ List.NonEmpty.nonEmpty tokenDistribution
  let
    mintingAction = T.Mint
      (CurrencyIssuer issuerAddress issuerSigningKey)
      tokenDistribution'

  connection <- view ieConnection
  submitMode <- view ieExecutionMode <&> toSubmitMode
  printStats <- view iePrintStats
  era <- view ieEra
  (mintingTx, policy) <- runCli era "[Mint] " $ buildMintingImpl
    connection
    mintingAction
    soMetadata
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
      { waSubmittedTransactions = mintingTx : waSubmittedTransactions
      }
  modifying isCurrencies \(Currencies currencies) ->
    Currencies $ Map.insert soCurrencyNickname currency currencies

interpret SplitWallet {..} = do
  Wallet address _ _ skey _ :: Wallet era <- findWallet soWalletNickname
  connection <- view ieConnection
  submitMode <- view ieExecutionMode <&> toSubmitMode
  let
    values = [ C.lovelaceToValue v | v <- soValues ]

  era <- view ieEra
  void $ runCli era "[createCollaterals] " $ buildFaucetImpl
    connection
    (Just values)
    [address]
    address
    skey
    defaultCoinSelectionStrategy
    submitMode

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

