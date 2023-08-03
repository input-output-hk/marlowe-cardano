{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Marlowe.CLI.Test.Wallet.Interpret where

import Cardano.Api (
  AddressInEra,
  AsType (AsPaymentKey),
  CardanoMode,
  Key (getVerificationKey, verificationKeyHash),
  LocalNodeConnectInfo (LocalNodeConnectInfo),
  PaymentCredential (PaymentCredentialByKey),
  ScriptDataSupportedInEra,
  StakeAddressReference (NoStakeAddress),
  generateSigningKey,
  makeShelleyAddressInEra,
 )
import Cardano.Api qualified as C
import Control.Lens (ifor_, modifying, use, uses, view, (.=))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Fixed qualified as F
import Data.Foldable (Foldable (fold), find, for_)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.Strict qualified as Map
import Data.Set qualified as S (fromList, singleton)
import Data.Traversable (for)
import Language.Marlowe.CLI.Cardano.Api.Value (lovelaceToPlutusValue, toPlutusValue)
import Language.Marlowe.CLI.Test.Wallet.Types (
  Asset (Asset),
  AssetId (AdaAssetId, AssetId),
  AssetsBalance (AssetsBalance),
  Currencies (Currencies),
  Currency (Currency, ccCurrencySymbol, ccIssuer),
  CurrencyNickname,
  InterpretMonad,
  IsExternalWallet (..),
  SomeTxBody (..),
  TokenAssignment (TokenAssignment),
  TokenRecipient (..),
  TokenRecipientScript (..),
  Wallet (Wallet, _waAddress, _waBalanceCheckBaseline, _waSigningKey, _waSubmittedTransactions),
  WalletNickname (WalletNickname),
  WalletOperation (..),
  adaToken,
  checkBalance,
  connectionL,
  currenciesL,
  emptyWallet,
  eraL,
  executionModeL,
  faucetNickname,
  getAllWallets,
  insertWallet,
  mapWallets,
  printStatsL,
  unWalletNickname,
  walletTxFees,
  walletsL,
 )
import Language.Marlowe.CLI.Test.Wallet.Types qualified as T
import Language.Marlowe.CLI.Transaction (
  buildBodyWithContent,
  buildFaucetImpl,
  buildMintingImpl,
  queryUtxos,
  submitBody',
 )
import Language.Marlowe.CLI.Types (
  AnUTxO,
  CurrencyIssuer (CurrencyIssuer),
  PayFromScript,
  SigningKeyFile (..),
  TokensRecipient (..),
  defaultCoinSelectionStrategy,
  validatorAddress,
 )
import Language.Marlowe.Core.V1.Semantics.Types qualified as M
import Plutus.V1.Ledger.Api (CurrencySymbol, TokenName)
import Plutus.V1.Ledger.Value qualified as P

import Cardano.Api.Byron (SerialiseAsRawBytes (..))
import Cardano.Api.Shelley qualified as CAS
import Contrib.Control.Monad.Except (note)
import Contrib.Data.Foldable (foldMapFlipped, ifoldMapMFlipped)
import Control.Category ((>>>))
import Control.Error.Util qualified as Error
import Control.Monad (forM, unless, void, when)
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Except (liftEither, runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..))
import Data.Bifunctor qualified as Bifunctor
import Data.ByteString.Base16.Aeson (EncodeBase16 (..))
import Data.Coerce (coerce)
import Data.Foldable qualified as Foldable
import Data.Foldable.WithIndex (ifoldrM)
import Data.Functor ((<&>))
import Data.List.NonEmpty qualified as List
import Data.List.NonEmpty qualified as List.NonEmpty
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Tuple.Extra (uncurry3)
import Language.Marlowe.CLI.Cardano.Api.PlutusScript (fromV2Validator)
import Language.Marlowe.CLI.Cardano.Api.Value qualified as CV
import Language.Marlowe.CLI.IO (queryInEra, readSigningKey)
import Language.Marlowe.CLI.Run (toCardanoPolicyId)
import Language.Marlowe.CLI.Sync (toPlutusAddress)
import Language.Marlowe.CLI.Test.CLI.Monad (runCli, runLabeledCli)
import Language.Marlowe.CLI.Test.Contract.ParametrizedMarloweJSON (
  ParametrizedMarloweJSON,
  decodeParametrizedContractJSON,
  decodeParametrizedInputJSON,
  now,
 )
import Language.Marlowe.CLI.Test.ExecutionMode (skipInSimluationMode, toSubmitMode)
import Language.Marlowe.CLI.Test.InterpreterError (assertionFailed', testExecutionFailed')
import Language.Marlowe.CLI.Test.Log (Label, logStoreLabeledMsg, throwLabeledError)
import Language.Marlowe.CLI.Types qualified as CT
import Language.Marlowe.Cardano (marloweNetworkFromLocalNodeConnectInfo)
import Language.Marlowe.Scripts.OpenRole (openRoleValidator)
import Ledger.Tx.CardanoAPI (fromCardanoPolicyId, toCardanoPolicyId)
import Plutus.V1.Ledger.Value (valueOf)
import Plutus.V1.Ledger.Value qualified as PV
import Plutus.V2.Ledger.Api (MintingPolicyHash (..))
import Plutus.V2.Ledger.Api qualified as P
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Builtins qualified as P
import PlutusTx.Monoid (Group (inv))
import System.IO.Temp (emptySystemTempFile, emptyTempFile)

findWallet
  :: (InterpretMonad env st m era)
  => WalletNickname
  -> m (Either String (Wallet era))
findWallet nickname =
  use walletsL >>= \(getAllWallets -> wallets) ->
    pure $ Error.note ("[getWallet] Unable to find wallet:" <> show nickname) (Map.lookup nickname wallets)

getWallet
  :: (InterpretMonad env st m era)
  => WalletNickname
  -> m (Wallet era)
getWallet nickname = findWallet nickname >>= (Bifunctor.first testExecutionFailed' >>> liftEither)

updateWallet
  :: (InterpretMonad env st m era)
  => WalletNickname
  -> (Wallet era -> Wallet era)
  -> m ()
updateWallet nickname update = do
  wallet <- getWallet nickname
  let wallet' = update wallet
  wallets <- use walletsL
  case T.updateWallet nickname wallet' wallets of
    Nothing -> throwError $ testExecutionFailed' $ "[updateWallet] Unable to update wallet:" <> show nickname
    Just wallets' -> walletsL .= wallets'

updateWallets
  :: (InterpretMonad env st m era)
  => (Wallet era -> Wallet era)
  -> m ()
updateWallets f = do
  wallets <- use walletsL
  let wallets' = mapWallets f wallets
  walletsL .= wallets'

getFaucet
  :: (InterpretMonad env st m era)
  => m (Wallet era)
getFaucet =
  getWallet faucetNickname

updateFaucet
  :: (InterpretMonad env st m era)
  => (Wallet era -> Wallet era)
  -> m ()
updateFaucet = updateWallet faucetNickname

openRoleValidatorAddress
  :: forall env st m era
   . (InterpretMonad env st m era)
  => m (AddressInEra era)
openRoleValidatorAddress = do
  era <- view eraL
  connection <- view connectionL
  let LocalNodeConnectInfo{localNodeNetworkId} = connection
  pure $ validatorAddress (fromV2Validator openRoleValidator) era localNodeNetworkId NoStakeAddress

fetchWalletUTxOs
  :: (InterpretMonad env st m era)
  => Wallet era
  -> m [AnUTxO era]
fetchWalletUTxOs (_waAddress -> address) = do
  connection <- view connectionL
  era <- view eraL
  utxo <-
    runCli era "[fetchWalletUTxO]"
      $ queryInEra connection
        . C.QueryUTxO
        . C.QueryUTxOByAddress
        . S.singleton
        . CT.toAddressAny'
      $ address
  pure $ CT.fromUTxO utxo

fetchWalletsUTxOs
  :: (InterpretMonad env st m era)
  => m (Map.Map WalletNickname [AnUTxO era])
fetchWalletsUTxOs = do
  wallets <- uses walletsL getAllWallets
  let walletsAddresses = wallets <&> \wallet -> _waAddress wallet

  connection <- view connectionL
  era <- view eraL

  (CT.fromUTxO -> utxos) <-
    runCli era "[fetchWalletsUTxOs]"
      $ queryInEra connection
        . C.QueryUTxO
        . C.QueryUTxOByAddress
        . S.fromList
        . fmap CT.toAddressAny'
        . Foldable.toList
      $ walletsAddresses

  let addressToUtxos =
        Map.fromListWith mappend $
          utxos <&> \anUtxo@(CT.AnUTxO (_, C.TxOut addr _ _ _)) ->
            (toPlutusAddress addr, [anUtxo])

  pure $
    wallets <&> \wallet ->
      fromMaybe mempty $ Map.lookup (toPlutusAddress $ _waAddress wallet) addressToUtxos

fetchWalletsValue
  :: forall env st m era
   . (InterpretMonad env st m era)
  => m (Map.Map WalletNickname P.Value)
fetchWalletsValue = do
  (walletsUtxos :: Map.Map WalletNickname [CT.AnUTxO era]) <- fetchWalletsUTxOs
  pure $
    walletsUtxos <&> foldMap \(CT.AnUTxO (_, C.TxOut _ txOutValue _ _)) ->
      CV.toPlutusValue $ C.txOutValueToValue txOutValue

fetchWalletValue
  :: forall env st m era
   . (InterpretMonad env st m era)
  => Wallet era
  -> m P.Value
fetchWalletValue wallet = do
  walletUtxos <- fetchWalletUTxOs wallet
  pure $ foldMapFlipped walletUtxos \(CT.AnUTxO (_, C.TxOut _ txOutValue _ _)) ->
    CV.toPlutusValue $ C.txOutValueToValue txOutValue

getSingletonCurrency
  :: (InterpretMonad env st m era)
  => m (CurrencyNickname, Currency)
getSingletonCurrency = do
  Currencies currencies <- use currenciesL
  case Map.toList currencies of
    [c] -> pure c
    _ -> throwError $ testExecutionFailed' "Ambigious currency lookup."

getCurrency
  :: (InterpretMonad env st m era)
  => CurrencyNickname
  -> m Currency
getCurrency nickname = do
  (Currencies currencies) <- use currenciesL
  note
    (testExecutionFailed' $ "[getCurrency] Unable to find currency:" <> show nickname)
    $ Map.lookup nickname currencies

findCurrency
  :: (InterpretMonad env st m era)
  => CurrencyNickname
  -> m (Either String Currency)
findCurrency nickname = do
  (Currencies currencies) <- use currenciesL
  pure $ Error.note ("[getCurrency] Unable to find currency:" <> show nickname) $ Map.lookup nickname currencies

getCurrencyBySymbol
  :: (InterpretMonad env st m era)
  => CurrencySymbol
  -> m (CurrencyNickname, Currency)
getCurrencyBySymbol currencySymbol = do
  (Currencies currencies) <- use currenciesL
  let possibleCurrency =
        find
          (\(_, c) -> ccCurrencySymbol c == currencySymbol)
          (Map.toList currencies)
  note
    (testExecutionFailed' $ "[getCurrencyBySymbol] Unable to find currency:" <> show currencySymbol)
    possibleCurrency

getWalletByAddress
  :: (InterpretMonad env st m era)
  => C.AddressInEra era
  -> m (WalletNickname, Wallet era)
getWalletByAddress address = do
  wallets <- uses walletsL getAllWallets
  let wallet = find (\(_, w) -> address == _waAddress w) (Map.toList wallets)
  note
    ( testExecutionFailed' $
        "[getWalletByPkh] Wallet not found for a given address: " <> show address <> " in wallets: " <> show wallets
    )
    wallet

getWalletByUniqueToken
  :: (InterpretMonad env st m era)
  => CurrencyNickname
  -> TokenName
  -> m (WalletNickname, Wallet era)
getWalletByUniqueToken currencyNickname tokenName =
  findWalletByUniqueToken currencyNickname tokenName >>= (Bifunctor.first testExecutionFailed' >>> liftEither)

findWalletByUniqueToken
  :: (InterpretMonad env st m era)
  => CurrencyNickname
  -> TokenName
  -> m (Either String (WalletNickname, Wallet era))
findWalletByUniqueToken currencyNickname tokenName = runExceptT do
  Currency{..} <- ExceptT (findCurrency currencyNickname)
  let check value = valueOf value ccCurrencySymbol tokenName == 1
      step n value Nothing =
        pure $
          if check value
            then Just n
            else Nothing
      step n value res =
        if check value
          then case res of
            Just n' ->
              throwError $ "[findByUniqueToken] Token is not unique - found in two wallets: " <> show n <> " and " <> show n' <> "."
            Nothing ->
              pure $ Just n
          else pure res
  wallet2value <- lift fetchWalletsValue
  possibleOwnerNickname <- ifoldrM step Nothing wallet2value
  ownerNickname <-
    liftEither $
      Error.note
        ("[getWalletByUniqueToken] Wallet not found for a given token: " <> show ccCurrencySymbol <> ":" <> show tokenName)
        possibleOwnerNickname
  (ownerNickname,) <$> (ExceptT $ findWallet ownerNickname)

getWalletsByCurrencyTokens
  :: (InterpretMonad env st m era)
  => CurrencyNickname
  -- ^ Currency to look for
  -> Maybe [TokenName]
  -- ^ If Nothing, then we check if the wallet has any tokens of the given currency.
  -> m [(WalletNickname, Wallet era, List.NonEmpty P.TokenName)]
getWalletsByCurrencyTokens currencyNickname possibleTokenNames = do
  Currency{..} <- getCurrency currencyNickname
  let findTokens :: P.Value -> Maybe (List.NonEmpty P.TokenName)
      findTokens value@(P.Value valueMap) = do
        let valueTokenNames = case possibleTokenNames of
              Just tokenNames ->
                filter (\tokenName -> valueOf value ccCurrencySymbol tokenName > 0) tokenNames
              Nothing -> do
                case AssocMap.lookup ccCurrencySymbol valueMap of
                  Just tokenMap -> AssocMap.keys tokenMap
                  _ -> []
        List.NonEmpty.nonEmpty valueTokenNames

  wallet2value <- fetchWalletsValue
  let matches =
        catMaybes $
          Map.toList wallet2value <&> \(n, value) -> do
            tokenNames <- findTokens value
            pure (n, tokenNames)
  for matches $ \(n, tokenNames) -> do
    wallet <- getWallet n
    pure (n, wallet, tokenNames)

assetToMarloweValuePair :: (InterpretMonad env st m era) => Asset -> m (M.Token, Integer)
assetToMarloweValuePair (Asset AdaAssetId amount) = pure (M.Token "" "", amount)
assetToMarloweValuePair (Asset (AssetId currencyNickname tokenName) amount) = do
  Currency{ccCurrencySymbol} <- findCurrency currencyNickname
  pure (M.Token ccCurrencySymbol tokenName, amount)

assetToPlutusValue
  :: (InterpretMonad env st m era)
  => Asset
  -> m P.Value
assetToPlutusValue (Asset AdaAssetId amount) = pure $ P.singleton P.adaSymbol P.adaToken amount
assetToPlutusValue (Asset (AssetId currencyNickname tokenName) amount) = do
  Currency{ccCurrencySymbol} <- getCurrency currencyNickname
  pure $ P.singleton ccCurrencySymbol tokenName amount

plutusValueToAssets
  :: (InterpretMonad env st m era)
  => P.Value
  -> m [Asset]
plutusValueToAssets value = do
  let valueToAsset (currencySymbol, tokenName, _)
        | currencySymbol == P.adaSymbol && tokenName == P.adaToken =
            pure (Asset AdaAssetId (P.fromBuiltin $ P.valueOf value currencySymbol tokenName))
      valueToAsset (cCurrencySymbol, tokenName, amount) = do
        let
        (currencyNickname, _) <- getCurrencyBySymbol cCurrencySymbol
        pure (Asset (AssetId currencyNickname tokenName) amount)
  for (P.flattenValue value) valueToAsset

assetIdToToken
  :: (InterpretMonad env st m era)
  => AssetId
  -> m M.Token
assetIdToToken (AssetId currencyNickname currencyToken) = do
  Currency{ccCurrencySymbol = currencySymbol} <- getCurrency currencyNickname
  pure $ M.Token currencySymbol currencyToken
assetIdToToken AdaAssetId = pure adaToken

decodeInputJSON
  :: (InterpretMonad env st m era)
  => ParametrizedMarloweJSON
  -> m M.Input
decodeInputJSON json = do
  currencies <- use currenciesL
  wallets <- use walletsL
  network <- view connectionL <&> marloweNetworkFromLocalNodeConnectInfo
  n <- now
  case decodeParametrizedInputJSON network wallets currencies n json of
    Left err -> throwError $ testExecutionFailed' $ "Failed to decode input: " <> show err
    Right i -> pure i

decodeContractJSON
  :: (InterpretMonad env st m era)
  => ParametrizedMarloweJSON
  -> m M.Contract
decodeContractJSON json = do
  currencies <- use currenciesL
  wallets <- use walletsL
  network <- view connectionL <&> marloweNetworkFromLocalNodeConnectInfo
  n <- now
  case decodeParametrizedContractJSON network wallets currencies n json of
    Left err -> throwError $ testExecutionFailed' $ "Failed to decode contract: " <> show err
    Right c -> pure c

interpret
  :: forall env era st m
   . (C.IsShelleyBasedEra era)
  => (InterpretMonad env st m era)
  => WalletOperation
  -> m ()
interpret so@CheckBalance{..} =
  view executionModeL >>= skipInSimluationMode so do
    wallet@Wallet{..} <- getWallet woWalletNickname
    utxos <- fetchWalletUTxOs wallet :: m [AnUTxO era]
    let onChainTotal = CV.toPlutusValue $ foldMap CT.anUTxOValue utxos
        fees = walletTxFees wallet

    logStoreLabeledMsg so $ "Checking balance of a wallet: " <> show case woWalletNickname of WalletNickname n -> n
    logStoreLabeledMsg so $ "Number of already submitted transactions: " <> show (length _waSubmittedTransactions)
    logStoreLabeledMsg so $
      "Total transaction fees amount: " <> do
        let C.Lovelace amount = fees
        show ((fromInteger amount / 1_000_000) :: F.Micro) <> " ADA"

    actualBalance <- plutusValueToAssets $ onChainTotal <> lovelaceToPlutusValue fees <> inv _waBalanceCheckBaseline
    let AssetsBalance expectedBalance = woBalance

    -- Iterate over assets in the wallet and check if we meet the expected balance.
    for_ actualBalance \(Asset assetId v) -> do
      case Map.lookup assetId expectedBalance of
        Nothing ->
          when (v /= 0) do
            throwLabeledError so $
              assertionFailed' ("Unexpected asset in balance check:" <> show v <> " of asset: " <> show assetId)
        Just expectedValue ->
          unless (checkBalance expectedValue v) do
            logStoreLabeledMsg so $ "Actual balance of asset " <> show assetId <> " is:" <> show v
            logStoreLabeledMsg so $ "Expected balance of asset " <> show assetId <> " is: " <> show expectedValue
            throwLabeledError so $ assertionFailed' $ "Balance check failed for an asset: " <> show assetId

    -- Iterate over expected assets and check if we have them in the wallet.
    --  * If we have them then we already checked the balance above.
    --  * If we miss an asset, we check if the expected balance is 0.
    for_ (Map.toList expectedBalance) \(assetId, expectedValue) -> do
      let actualAssets = map (\(Asset k _) -> k) actualBalance
      unless (assetId `elem` actualAssets || checkBalance expectedValue 0) do
        logStoreLabeledMsg so $ "Expected balance of an asset " <> show assetId <> " is:" <> show expectedValue
        throwLabeledError so $ assertionFailed' $ "Asset is missing from the wallet on chain balance:" <> show assetId
interpret so@(CreateWallet woWalletNickname possibleUTxOs) = do
  (connection :: LocalNodeConnectInfo CardanoMode) <- view connectionL
  let WalletNickname rawNickname = woWalletNickname
  era <- view eraL
  wallet@Wallet{_waAddress = address} <- liftIO $ createWallet era connection

  logStoreLabeledMsg so $
    "Wallet " <> show rawNickname <> " created with an address: " <> Text.unpack (C.serialiseAddress address)

  (addrFile, CT.SigningKeyFile skeyFile) <- liftIO $ saveWalletFiles woWalletNickname wallet Nothing
  logStoreLabeledMsg so $ "Wallet info stored in " <> addrFile <> " and " <> skeyFile

  wallets <- use walletsL
  case insertWallet woWalletNickname wallet wallets of
    Nothing -> throwError $ testExecutionFailed' $ "Wallet with nickname " <> show rawNickname <> " already exists"
    Just wallets' -> walletsL .= wallets'

  case possibleUTxOs of
    Nothing -> pure ()
    Just utxos -> do
      logStoreLabeledMsg so $ "Funding wallet " <> show rawNickname <> " with " <> show (length utxos) <> " UTxOs"
      fundWallets so [woWalletNickname] utxos
interpret so@ExternalWallet{..} = do
  era <- view eraL
  connection <- view connectionL
  skey <- runLabeledCli era so $ readSigningKey (SigningKeyFile woSigningKeyFile)
  let LocalNodeConnectInfo{localNodeNetworkId} = connection
      vkey = either getVerificationKey (C.castVerificationKey . getVerificationKey) skey
      (address :: AddressInEra era) = makeShelleyAddressInEra localNodeNetworkId (PaymentCredentialByKey (verificationKeyHash vkey)) NoStakeAddress

  utxo <- runLabeledCli era so $ queryUtxos connection address
  let wallet = T.fromUTxO address skey utxo (IsExternalWallet True)
  wallets <- use walletsL
  case insertWallet woWalletNickname wallet wallets of
    Nothing ->
      throwError $
        testExecutionFailed' $
          "Wallet with nickname " <> show (unWalletNickname woWalletNickname) <> " already exists"
    Just wallets' -> do
      logStoreLabeledMsg so $
        "Wallet "
          <> show (unWalletNickname woWalletNickname)
          <> " loaded with an address: "
          <> Text.unpack (C.serialiseAddress address)
      walletsL .= wallets'
interpret so@BurnAll{..} = do
  Currencies (Map.toList -> currencies) <- use currenciesL
  allWalletsUtxos <- fold <$> fetchWalletsUTxOs
  wallets <- uses walletsL getAllWallets
  let initialTotal = toPlutusValue $ foldMap CT.anUTxOValue allWalletsUtxos
      initialSymbols = Set.fromList $ P.symbols initialTotal
<<<<<<< HEAD
  for_ currencies \(currencyNickname, Currency{ccIssuer = possibleIssuer, ccCurrencySymbol}) -> case possibleIssuer of
    Just issuer ->
      if not (ccCurrencySymbol `Set.member` initialSymbols)
        then logStoreLabeledMsg so $ "Currency " <> show currencyNickname <> " tokens not found, skipping."
        else do
          Wallet{_waAddress = issuerAddress, _waSigningKey = issuerSigningKey} <- findWallet issuer
          let providers = Map.elems wallets <&> \Wallet{_waAddress, _waSigningKey} -> (_waAddress, _waSigningKey)
              currencyIssuer = CT.CurrencyIssuer issuerAddress issuerSigningKey
              mintingAction = CT.BurnAll currencyIssuer $ (issuerAddress, issuerSigningKey) :| filter ((/=) issuerAddress . fst) providers
          connection <- view connectionL
          submitMode <- view executionModeL <&> toSubmitMode
          printStats <- view printStatsL
          era <- view eraL
          (burningTx, _) <-
            runLabeledCli era so $
              buildMintingImpl
                connection
                mintingAction
                woMetadata
                -- Nothing
                (Just 134070922)
                submitMode
                printStats

          -- This is a bit counterintuitive but we have to actually update the initial state
          -- of the wallets if they contained tokens before the test run.
          -- We burn *all* the tokens so we have to drop them from the wallet balance baseline as well.
          updateWallets \wallet@Wallet{_waBalanceCheckBaseline} -> do
            let balanceCheckBaseline' = do
                  let check (cs, _, _) = cs /= ccCurrencySymbol
                  foldMap (uncurry3 P.singleton) . filter check . P.flattenValue $ _waBalanceCheckBaseline
            wallet{_waBalanceCheckBaseline = balanceCheckBaseline'}

          updateWallet issuer \w@Wallet{_waSubmittedTransactions} ->
            w{_waSubmittedTransactions = SomeTxBody era burningTx : _waSubmittedTransactions}
    Nothing -> logStoreLabeledMsg so $ "Currency " <> show currencyNickname <> " was minted externally, skipping."
interpret wo@Fund{..} =
  fundWallets wo woWalletNicknames woUTxOs
interpret so@Mint{..} = do
  era <- view eraL
  connection <- view connectionL

  let issuerNickname = fromMaybe faucetNickname woIssuer

  (Currencies currencies) <- use currenciesL
  case Map.lookup woCurrencyNickname currencies of
    Just Currency{ccIssuer = ci} -> when (ci /= Just issuerNickname) do
      throwError $ testExecutionFailed' "Currency with a given nickname already exist and is minted by someone else."
    Nothing -> pure ()

  ifor_ currencies \currencyNickname Currency{ccIssuer} -> do
    when (ccIssuer == Just issuerNickname && currencyNickname /= woCurrencyNickname) $
      throwError $
        testExecutionFailed'
          "Current minting strategy mints unique currency per issuer. You can't mint multiple currencies with the same issuer."

-- <<<<<<< HEAD
--   error "FAILURE"
-- <<<<<<< HEAD
--   Wallet issuerAddress _ issuerSigningKey _ _ :: Wallet era <- findWallet issuerNickname
--   tokenDistribution <- forM woTokenDistribution \(TokenAssignment recipient tokens) -> do
--     destAddress <- case recipient of
--       Left bech32 -> do
--         case C.deserialiseFromBech32 C.AsShelleyAddress bech32 of
--           Left err -> throwError $ testExecutionFailed' $ "Failed to parse address: " <> show err
--           Right addr -> do
--             pure $ C.shelleyAddressInEra addr
--       Right walletNickname -> do
--         Wallet addr _ _ _ _ <- findWallet walletNickname
--         pure addr
--     pure (destAddress, Just woMinLovelace, tokens)
-- =======
--   Wallet issuerAddress _ issuerSigningKey _ :: Wallet era <- getWallet issuerNickname
--   tokenDistribution <- forM woTokenDistribution \(TokenAssignment recipient tokenName amount) -> do
--     destAddress <- case recipient of
--       WalletRecipient walletNickname -> do
--         Wallet destAddress _ _ _ <- getWallet walletNickname
--         pure destAddress
--       ScriptRecipient OpenRoleScript -> do
--         openRoleValidatorAddress
--     pure (tokenName, amount, destAddress, Just woMinLovelace)
-- >>>>>>> 909fe1a17 (Ongoing work on open role integartion into the test suite)
-- second change
-- =======
--   Wallet issuerAddress _ issuerSigningKey _ :: Wallet era <- getWallet issuerNickname
--   tokenDistribution <- forM woTokenDistribution \(TokenAssignment recipient tokenName amount) -> do
--     recipient' <- case recipient of
--       WalletRecipient walletNickname -> do
--         Wallet destAddress _ _ _ <- getWallet walletNickname
--         pure $ RegularAddressRecipient destAddress
--       ScriptRecipient OpenRoleScript -> do
--         let datum = CAS.fromPlutusData . P.toData $ P.emptyByteString
--         addr <- openRoleValidatorAddress
--         pure $ ScriptAddressRecipient addr datum
--     pure (tokenName, amount, recipient', Just woMinLovelace)
-- >>>>>>> 27b967b02 (Fully integarte open roles into the testing DSL with full e2e scenario.)

  logStoreLabeledMsg so $
    "Minting currency " <> coerce woCurrencyNickname <> " with tokens distribution: " <> show woTokenDistribution
  logStoreLabeledMsg so $ "The issuer is " <> coerce issuerNickname
  let mintingAction =
        CT.Mint
          (CurrencyIssuer issuerAddress issuerSigningKey)
          tokenDistribution
  submitMode <- view executionModeL <&> toSubmitMode
  printStats <- view printStatsL
  (mintingTx, policy) <-
    runCli era "[Mint] " $
      buildMintingImpl
        connection
        mintingAction
        woMetadata
        (Just 134070922)
        -- Nothing
        submitMode
        printStats

  logStoreLabeledMsg so $ "This currency symbol is " <> show policy
  let currencySymbol = PV.mpsSymbol . P.MintingPolicyHash . P.toBuiltin . serialiseToRawBytes $ policy
      currency = Currency currencySymbol (Just issuerNickname) policy

  updateWallet issuerNickname \issuer@Wallet{..} ->
    issuer
      { _waSubmittedTransactions = SomeTxBody era mintingTx : _waSubmittedTransactions
      }
  modifying currenciesL \(Currencies currencies') ->
    Currencies $ Map.insert woCurrencyNickname currency currencies'
interpret SplitWallet{..} = do
  Wallet address _ skey _ _ :: Wallet era <- findWallet woWalletNickname
  connection <- view connectionL
  submitMode <- view executionModeL <&> toSubmitMode
  let values = [C.lovelaceToValue v | v <- woUTxOs]

  era <- view eraL
  void $
    runCli era "[createCollaterals] " $
      buildFaucetImpl
        connection
        (Just values)
        [address]
        address
        skey
        defaultCoinSelectionStrategy
        submitMode
interpret wo@ReturnFunds{} = do
  (era :: ScriptDataSupportedInEra era) <- view eraL
  (allWalletsUtxos :: Map.Map WalletNickname [AnUTxO era]) <- fetchWalletsUTxOs
  (utxos, signingKeys) <- ifoldMapMFlipped allWalletsUtxos \n (utxos :: [CT.AnUTxO era]) -> do
    if n /= faucetNickname
      then do
        (Wallet{_waSigningKey, _waExternal} :: Wallet era) <- getWallet n
        if _waExternal
          then pure mempty
          else pure (utxos, [_waSigningKey])
      else pure mempty

  Wallet{_waAddress} <- getFaucet
  let total = foldMap CT.anUTxOValue utxos
      inputs = map (fst . CT.unAnUTxO) utxos
      outputs = []
      changeAddress = _waAddress

  if total == mempty
    then logStoreLabeledMsg wo "Nothing to refund"
    else do
      connection <- view connectionL
      (bodyContent, body) <-
        runLabeledCli era wo $
          buildBodyWithContent
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
            Nothing

      logStoreLabeledMsg wo $ "Returning funds to the faucet: " <> show total

      submitMode <- view executionModeL <&> toSubmitMode
      case submitMode of
        CT.DoSubmit timeout -> do
          (_, tx) <- runLabeledCli era wo $ submitBody' connection body bodyContent changeAddress signingKeys timeout
          updateWallet faucetNickname \issuer@Wallet{_waSubmittedTransactions} ->
            issuer{_waSubmittedTransactions = SomeTxBody era tx : _waSubmittedTransactions}
        CT.DontSubmit ->
          pure ()
interpret ExternalCurrency{..} = do
  (currencySymbol, policyId) <- case (woCurrencySymbol, woPolicyId) of
    (Just _, Just _) -> throwError $ testExecutionFailed' "You can't specify both currency symbol and policy id."
    (Nothing, Nothing) -> throwError $ testExecutionFailed' "You must specify either currency symbol or policy id."
    (Just (EncodeBase16 bs), _) -> do
      let cs@(P.CurrencySymbol currencySymbol) = P.currencySymbol bs

      case toCardanoPolicyId $ MintingPolicyHash currencySymbol of
        Right policy -> pure (cs, policy)
        Left err -> throwError $ testExecutionFailed' $ "Failed to parse currency symbol: " <> show err
    (_, Just _) -> throwError $ testExecutionFailed' "External currency with policy id is not supported yet."

  let currency = Currency currencySymbol Nothing policyId
  Currencies currencies <- use currenciesL
  case Map.lookup woCurrencyNickname currencies of
    Just Currency{..} -> do
      when (isJust ccIssuer) $
        throwError $
          testExecutionFailed' "Currency with a given nickname already exist and is minted by someone else."
      when (ccCurrencySymbol /= currencySymbol) $
        throwError $
          testExecutionFailed' "Currency with a given nickname already exist and has different currency symbol."
    Nothing ->
      modifying currenciesL \(Currencies currencies') ->
        Currencies $ Map.insert woCurrencyNickname currency currencies'

saveWalletFiles
  :: (C.IsCardanoEra era) => WalletNickname -> Wallet era -> Maybe FilePath -> IO (FilePath, CT.SigningKeyFile)
saveWalletFiles walletNickname wallet dir = do
  let WalletNickname nickname = walletNickname
      Wallet{_waAddress, _waSigningKey = sskey} = wallet
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
  writeFile addrFile (Text.unpack $ C.serialiseAddress _waAddress)
  void $ either (writeEnvelope skeyFile) (writeEnvelope skeyFile) sskey
  pure (addrFile, CT.SigningKeyFile skeyFile)

-- TODO: We should use `CardanoEra era` as an `era` carrier here.
createWallet
  :: forall era
   . (C.IsShelleyBasedEra era)
  => ScriptDataSupportedInEra era
  -> LocalNodeConnectInfo CardanoMode
  -> IO (Wallet era)
createWallet _ connection = do
  skey <- generateSigningKey AsPaymentKey
  let vkey = getVerificationKey skey
      LocalNodeConnectInfo{localNodeNetworkId} = connection
      (address :: AddressInEra era) =
        makeShelleyAddressInEra
          localNodeNetworkId
          (PaymentCredentialByKey (verificationKeyHash vkey))
          NoStakeAddress
  pure $ emptyWallet address (Left skey)

fundWallets
  :: (InterpretMonad env st m era)
  => (Label l)
  => l
  -> [WalletNickname]
  -> [C.Lovelace]
  -> m ()
fundWallets label walletNicknames utxos = do
  let values = [C.lovelaceToValue v | v <- utxos]

  (Wallet faucetAddress _ faucetSigningKey _ _ :: Wallet era) <- getFaucet
  addresses <- for walletNicknames \walletNickname -> do
    (Wallet address _ _ _ _) <- getWallet walletNickname
    pure address
  connection <- view connectionL
  submitMode <- view executionModeL <&> toSubmitMode
  era <- view eraL
  txBody <-
    runLabeledCli era label $
      buildFaucetImpl
        connection
        (Just values)
        addresses
        faucetAddress
        faucetSigningKey
        defaultCoinSelectionStrategy
        submitMode

  updateFaucet \faucet@(Wallet _ _ _ faucetTransactions _) ->
    faucet{_waSubmittedTransactions = SomeTxBody era txBody : faucetTransactions}
