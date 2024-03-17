{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
module Language.Marlowe.CLI.Transaction (
  -- * Types
  TxInEra,

  -- * Building
  buildClean,
  buildContinuing,
  buildFaucet,
  buildFaucet',
  buildFaucetImpl,
  buildIncoming,
  buildMinting,
  buildMintingImpl,
  buildOutgoing,
  buildPublishing,
  buildPublishingImpl,
  buildSimple,
  publishImpl,
  querySlotting,
  mkTxOutValue,
  babbageEraOnwardsToAllegraEraOnwards,

  -- * Submitting
  submit,
  submitBody,

  -- * Querying
  findMarloweScriptsRefs,
  findPublished,

  -- * Low-Level Functions
  buildBody,
  buildBodyWithContent,
  buildPayFromScript,
  buildPayToScript,
  hashSigningKey,
  makeBalancedTxOut,
  makeTxOut,
  makeTxOut',
  maximumFee,
  querySlotConfig,
  queryUtxos,
  selectUtxos,
  selectUtxosImpl,

  -- * Balancing
  ensureMinUtxo,
  findMinUtxo,
  selectCoins,
) where

import Cardano.Api (
  AddressInEra (..),
  AllegraEraOnwards (..),
  AsType (..),
  AssetId (..),
  AssetName (..),
  BabbageEraOnwards (..),
  BalancedTxBody (..),
  BuildTx,
  BuildTxWith (..),
  CtxTx,
  CtxUTxO,
  EraHistory (..),
  ExecutionUnits (..),
  File (..),
  Hash,
  KeyWitnessInCtx (..),
  LocalNodeConnectInfo (..),
  Lovelace,
  MaryEraOnwards (..),
  PaymentCredential (PaymentCredentialByScript),
  PaymentKey,
  PlutusScript,
  PlutusScriptVersion (..),
  PolicyId (..),
  Quantity (..),
  QueryInMode (..),
  QueryInShelleyBasedEra (..),
  QueryUTxOFilter (..),
  Script (..),
  ScriptDatum (..),
  ScriptHash,
  ScriptLanguageInEra,
  ScriptValidity (ScriptInvalid),
  ScriptWitness (..),
  ScriptWitnessInCtx (..),
  ShelleyBasedEra (..),
  SimpleScript (..),
  SimpleScript',
  SlotNo (..),
  StakeAddressReference (NoStakeAddress),
  TxAuxScripts (..),
  TxBody (..),
  TxBodyContent (..),
  TxBodyErrorAutoBalance (..),
  TxBodyScriptData (..),
  TxCertificates (..),
  TxExtraKeyWitnesses (..),
  TxFee (..),
  TxId,
  TxIn (..),
  TxInsCollateral (..),
  TxInsReference (TxInsReferenceNone),
  TxMetadataInEra (..),
  TxMetadataJsonSchema (TxMetadataJsonNoSchema),
  TxMintValue (..),
  TxOut (..),
  TxOutDatum (..),
  TxOutValue (..),
  TxReturnCollateral (TxReturnCollateralNone),
  TxScriptValidity (..),
  TxTotalCollateral (TxTotalCollateralNone),
  TxUpdateProposal (..),
  TxValidityLowerBound (..),
  TxValidityUpperBound (..),
  TxWithdrawals (..),
  UTxO (..),
  Value,
  WitCtxTxIn,
  Witness (..),
  babbageEraOnwardsToAlonzoEraOnwards,
  babbageEraOnwardsToMaryEraOnwards,
  babbageEraOnwardsToShelleyBasedEra,
  calculateMinimumUTxO,
  getTxId,
  hashScript,
  lovelaceToValue,
  makeShelleyAddressInEra,
  makeTransactionBodyAutoBalance,
  metadataFromJson,
  negateValue,
  queryNodeLocalState,
  readFileTextEnvelope,
  selectAsset,
  selectLovelace,
  serialiseToCBOR,
  serialiseToRawBytesHex,
  toScriptInAnyLang,
  txOutValueToValue,
  valueFromList,
  valueToList,
  valueToLovelace,
  verificationKeyHash,
  writeFileTextEnvelope,
 )
import Cardano.Api qualified as C
import Cardano.Api.Shelley (
  ExecutionUnitPrices (..),
  LedgerProtocolParameters (..),
  ProtocolParameters (..),
  ReferenceScript (ReferenceScript, ReferenceScriptNone),
  SimpleScriptOrReferenceInput (SScript),
  convertToLedgerProtocolParameters,
  fromPlutusData,
  protocolParamMaxBlockExUnits,
  protocolParamMaxTxExUnits,
  protocolParamMaxTxSize,
 )
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..))
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..))
import Cardano.Slotting.EpochInfo.API (epochInfoRange, epochInfoSlotToUTCTime, hoistEpochInfo)
import Contrib.Cardano.UTxO qualified as U
import Contrib.Data.Foldable (foldMapFlipped, tillFirstMatch)
import Control.Arrow ((***))
import Control.Error (MaybeT (MaybeT, runMaybeT), hoistMaybe, mapMaybe, note)
import Control.Monad (forM, unless, void, when)
import Control.Monad.Except (MonadError, MonadIO, liftEither, liftIO, runExcept, throwError)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (lift)
import Data.Aeson (ToJSON (toJSON))
import Data.Aeson qualified as A (Value (Null, Object), object)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Aeson.Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS (length)
import Data.ByteString.Char8 qualified as BS8 (unpack)
import Data.Fixed (div')
import Data.Foldable (Foldable (fold), for_)
import Data.Function (on)
import Data.Functor ((<&>))
import Data.List (delete, minimumBy, partition)
import Data.List.Extra (notNull)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Append qualified as AM
import Data.Map.Strict qualified as M (elems, fromList, lookup, singleton, toList)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isNothing, listToMaybe, maybeToList)
import Data.Ratio ((%))
import Data.Set qualified as S (empty, fromList, singleton)
import Data.Text qualified as T
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Units (Second)
import Data.Traversable (for)
import Data.Tuple.Extra (uncurry3)
import GHC.Natural (Natural)
import Language.Marlowe.CLI.Cardano.Api (
  adjustMinimumUTxO,
  toTxOutDatumInTx,
  txOutValueValue,
 )
import Language.Marlowe.CLI.Cardano.Api.Address.ProofOfBurn (permanentPublisher)
import Language.Marlowe.CLI.Cardano.Api.PlutusScript (toScript, toScriptLanguageInEra)
import Language.Marlowe.CLI.Export (buildValidatorInfo)
import Language.Marlowe.CLI.IO (
  decodeFileBuiltinData,
  decodeFileStrict,
  getEraHistory,
  getMajorProtocolVersion,
  getPV2CostModelParams,
  getProtocolParams,
  getSystemStart,
  liftCli,
  liftCliIO,
  liftCliMaybe,
  maybeWriteJson,
  queryByAddress,
  queryInEra,
  queryUTxOs,
  readMaybeMetadata,
  readSigningKey,
  submitTxBody,
  submitTxBody',
 )
import Language.Marlowe.CLI.Types (
  AnUTxO (AnUTxO),
  CliEnv,
  CliError (..),
  CoinSelectionStrategy (CoinSelectionStrategy, csPreserveInlineDatums, csPreserveReferenceScripts, csPreserveTxIns),
  CurrencyIssuer (CurrencyIssuer),
  MarloweScriptsRefs (MarloweScriptsRefs),
  MintingAction (BurnAll, Mint, maIssuer),
  OutputQuery (..),
  OutputQueryResult (..),
  PayFromScript (..),
  PayToScript (..),
  PrintStats (PrintStats),
  PublishingStrategy (..),
  QueryExecutionContext (..),
  SigningKeyFile,
  SomePaymentSigningKey,
  SubmitMode (DoSubmit),
  TokensRecipient (..),
  TxBodyFile (TxBodyFile),
  TxBuildupContext (..),
  ValidatorInfo (ValidatorInfo, viHash, viScript),
  askEra,
  asksEra,
  defaultCoinSelectionStrategy,
  doWithCardanoEra,
  getVerificationKey,
  mkNodeTxBuildup,
  queryContextNetworkId,
  toAddressAny',
  toAsType,
  toPaymentVerificationKey,
  toQueryContext,
  validatorInfo',
 )
import Language.Marlowe.CLI.Types qualified as PayToScript (PayToScript (value))
import Language.Marlowe.Scripts
import Ouroboros.Consensus.HardFork.History (interpreterToEpochInfo)
import Ouroboros.Network.Protocol.LocalStateQuery.Type (Target (VolatileTip))
import Plutus.V1.Ledger.SlotConfig (SlotConfig (..))
import PlutusLedgerApi.V1 (Datum (..), POSIXTime (..), Redeemer (..), TokenName (..), fromBuiltin, toData)
import System.IO (hPutStrLn, stderr)

-- | Build a non-Marlowe transaction.
buildSimple
  :: forall era m
   . (MonadError CliError m, C.IsShelleyBasedEra era)
  => (MonadIO m)
  => (MonadReader (CliEnv era) m)
  => TxBuildupContext era
  -- ^ The connection info for the local node.
  -> [SigningKeyFile]
  -- ^ The files for required signing keys.
  -> [TxIn]
  -- ^ The transaction inputs.
  -> [(AddressInEra era, TxOutDatum CtxTx era, Value)]
  -- ^ The transaction outputs.
  -> AddressInEra era
  -- ^ The change address.
  -> Maybe FilePath
  -- ^ The file containing JSON metadata, if any.
  -> TxBodyFile
  -- ^ The output file for the transaction body.
  -> Bool
  -- ^ Whether to print statistics about the transaction.
  -> Bool
  -- ^ Assertion that the transaction is invalid.
  -> m TxId
  -- ^ Action to build the transaction body.
buildSimple txBuildupCtx signingKeyFiles inputs outputs changeAddress metadataFile (TxBodyFile bodyFile) printStats invalid =
  do
    metadata <- readMaybeMetadata metadataFile
    signingKeys <- mapM readSigningKey signingKeyFiles
    outputs' <- mapM (uncurry3 makeTxOut') outputs
    let queryCtx = toQueryContext txBuildupCtx
    body <-
      buildBody
        queryCtx
        ([] :: [PayFromScript C.PlutusScriptV1])
        Nothing
        []
        inputs
        outputs'
        Nothing
        changeAddress
        Nothing
        []
        TxMintNone
        metadata
        printStats
        invalid
    doWithCardanoEra $ liftCliIO $ writeFileTextEnvelope (File bodyFile) Nothing body
    submitBody txBuildupCtx body signingKeys invalid

-- | Build a non-Marlowe transaction that cleans an address.
buildClean
  :: (MonadError CliError m, C.IsShelleyBasedEra era)
  => (MonadIO m)
  => (MonadReader (CliEnv era) m)
  => LocalNodeConnectInfo
  -- ^ The connection info for the local node.
  -> [SigningKeyFile]
  -- ^ The files for required signing keys.
  -> Lovelace
  -- ^ The value to be sent to addresses with tokens.
  -> AddressInEra era
  -- ^ The change address.
  -> Maybe (SlotNo, SlotNo)
  -- ^ The valid slot range, if any.
  -> TxMintValue BuildTx era
  -- ^ The mint value.
  -> TxMetadataInEra era
  -- ^ The metadata.
  -> TxBodyFile
  -- ^ The output file for the transaction body.
  -> Maybe Second
  -- ^ Number of seconds to wait for the transaction to be confirmed, if it is to be confirmed.
  -> m TxId
  -- ^ Action to build the transaction body.
buildClean connection signingKeyFiles lovelace changeAddress range mintValue metadata (TxBodyFile bodyFile) timeout =
  do
    signingKeys <- mapM readSigningKey signingKeyFiles
    utxos <-
      fmap (M.toList . unUTxO)
        . queryInEra connection
        . QueryUTxO
        . QueryUTxOByAddress
        . S.singleton
        $ toAddressAny' changeAddress
    let minting =
          case mintValue of
            TxMintValue _ minting' _ -> minting'
            _ -> mempty
        inputs = fst <$> utxos
        extractValue (TxOut _ value _ _) = txOutValueToValue value
        total = mconcat $ extractValue . snd <$> utxos
    outputs <-
      sequence
        [ makeTxOut changeAddress C.TxOutDatumNone (value <> lovelaceToValue lovelace) ReferenceScriptNone
        | value <- valueFromList . pure <$> valueToList (total <> minting)
        , isNothing $ valueToLovelace value
        ]

    body <-
      buildBody
        (QueryNode connection)
        ([] :: [PayFromScript C.PlutusScriptV1])
        Nothing
        []
        inputs
        outputs
        Nothing
        changeAddress
        range
        []
        mintValue
        metadata
        False
        False
    doWithCardanoEra $ liftCliIO $ writeFileTextEnvelope (File bodyFile) Nothing body
    let txBuildupCtx = mkNodeTxBuildup connection timeout
    submitTxBody txBuildupCtx body signingKeys

-- | Build a non-Marlowe transaction that fills and address from a faucet.
buildFaucet
  :: (MonadError CliError m, C.IsShelleyBasedEra era)
  => (MonadIO m)
  => (MonadReader (CliEnv era) m)
  => LocalNodeConnectInfo
  -- ^ The connection info for the local node.
  -> Maybe Value
  -- ^ The value to be sent to the funded addresses. By default we drain the source.
  -> [AddressInEra era]
  -- ^ The addresses to receive funds.
  -> AddressInEra era
  -- ^ The faucet address.
  -> SigningKeyFile
  -- ^ The required signing key.
  -> Maybe Second
  -- ^ Number of seconds to wait for the transaction to be confirmed, if it is to be confirmed.
  -> m TxId
  -- ^ Action to build the transaction body.
buildFaucet connection possibleValue destAddresses fundAddress fundSigningKeyFile timeout =
  do
    let singleton x = x : mempty
    fundSigningKey <- readSigningKey fundSigningKeyFile
    body <-
      buildFaucetImpl
        (mkNodeTxBuildup connection timeout)
        (singleton <$> possibleValue)
        destAddresses
        fundAddress
        fundSigningKey
        defaultCoinSelectionStrategy
    pure $
      getTxId body

-- | Build a non-Marlowe transaction that fills and address from a faucet.
buildFaucetImpl
  :: (MonadError CliError m, C.IsShelleyBasedEra era)
  => (MonadIO m)
  => (MonadReader (CliEnv era) m)
  => TxBuildupContext era
  -- ^ The connection info for the local node or pure context for tx buildup.
  -> Maybe [Value]
  -- ^ The list of values to be sent to the funded addresses as separate outputs.
  -> [AddressInEra era]
  -- ^ The addresses to receive funds.
  -> AddressInEra era
  -- ^ The faucet address.
  -> SomePaymentSigningKey
  -- ^ The required signing key.
  -> CoinSelectionStrategy
  -- ^ Instruction which utxos to preserve.
  -> m (TxBody era)
  -- ^ Action to build the transaction body.
buildFaucetImpl txBuildupCtx possibleValues destAddresses fundAddress fundSigningKey coinSelectionStrategy =
  do
    let queryCtx = toQueryContext txBuildupCtx
    utxos <- queryByAddress queryCtx fundAddress
    let utxosList = U.toList utxos
    (inputs, outputs', changeAddress) <- case (possibleValues, destAddresses) of
      (Nothing, [destAddress]) -> do
        let total = foldMap (txOutValueValue . snd) utxosList
            nonAda = total <> negateValue (C.lovelaceToValue . C.selectLovelace $ total)

        outputs <-
          if nonAda /= mempty
            then do
              era <- askEra
              protocol <- getProtocolParams queryCtx
              protocol' <- liftCli $ convertToLedgerProtocolParameters (babbageEraOnwardsToShelleyBasedEra era) protocol
              txOut <- makeBalancedTxOut era protocol' destAddress C.TxOutDatumNone nonAda ReferenceScriptNone
              pure [txOut]
            else pure []
        pure (map fst utxosList, outputs, destAddress)
      (Nothing, _) -> do
        throwError "Total value transfer is only supported to a single destination."
      (Just values, _) -> do
        outputs <-
          sequence $
            [makeTxOut destAddress C.TxOutDatumNone value ReferenceScriptNone | value <- values, destAddress <- destAddresses]

        (_, i, o) <-
          selectCoins
            queryCtx
            mempty
            outputs
            Nothing
            fundAddress
            coinSelectionStrategy
            Nothing
        pure (i, o, fundAddress)

    body <-
      buildBody
        queryCtx
        ([] :: [PayFromScript C.PlutusScriptV1])
        Nothing
        []
        inputs
        outputs'
        Nothing
        changeAddress
        Nothing
        []
        TxMintNone
        TxMetadataNone
        False
        False
    void $ submitTxBody txBuildupCtx body [fundSigningKey]
    pure body

-- | Build a non-Marlowe transaction that fills and address from a faucet.
buildFaucet'
  :: forall m era
   . (MonadError CliError m, C.IsShelleyBasedEra era)
  => (MonadIO m)
  => (MonadReader (CliEnv era) m)
  => LocalNodeConnectInfo
  -- ^ The connection info for the local node.
  -> Value
  -- ^ The value to be sent to the funded addresses.
  -> [AddressInEra era]
  -- ^ The funded addresses.
  -> TxBodyFile
  -- ^ The output file for the transaction body.
  -> Maybe Second
  -- ^ Number of seconds to wait for the transaction to be confirmed, if it is to be confirmed.
  -> m TxId
  -- ^ Action to build the transaction body.
buildFaucet' connection value addresses (TxBodyFile bodyFile) timeout =
  do
    era <- askEra
    let network = localNodeNetworkId connection
        script = RequireAllOf []
        scriptLanguageInEra :: ScriptLanguageInEra SimpleScript' era
        scriptLanguageInEra = case era of
          BabbageEraOnwardsBabbage -> C.SimpleScriptInBabbage
          BabbageEraOnwardsConway -> C.SimpleScriptInConway
        witness =
          BuildTxWith
            . ScriptWitness ScriptWitnessForSpending
            $ SimpleScriptWitness scriptLanguageInEra (SScript script)
        changeAddress =
          makeShelleyAddressInEra
            (babbageEraOnwardsToShelleyBasedEra era)
            network
            (PaymentCredentialByScript . hashScript . SimpleScript $ script)
            NoStakeAddress
    utxos <-
      fmap (M.toList . unUTxO)
        . queryInEra connection
        . QueryUTxO
        . QueryUTxOByAddress
        . S.singleton
        $ toAddressAny' changeAddress
    let inputs = [(txIn, witness) | txIn <- fst <$> utxos]
        extractValue (TxOut _ v _ _) = txOutValueToValue v
        total = mconcat $ extractValue . snd <$> utxos
        lovelace = lovelaceToValue . toEnum . (`div` 2) . fromEnum $ selectLovelace total
        value' = mconcat $ replicate (length addresses) value
    outputs <-
      mapM (uncurry3 makeTxOut') $
        (changeAddress, C.TxOutDatumNone, total <> negateValue value' <> negateValue lovelace)
          : [(fundedAddress, C.TxOutDatumNone, value) | fundedAddress <- addresses]
    body <-
      buildBody
        (QueryNode connection)
        ([] :: [PayFromScript C.PlutusScriptV1])
        Nothing
        inputs
        []
        outputs
        Nothing
        changeAddress
        Nothing
        []
        TxMintNone
        TxMetadataNone
        False
        False
    C.cardanoEraConstraints (C.babbageEraOnwardsToCardanoEra era) $
      liftCliIO $
        writeFileTextEnvelope (File bodyFile) Nothing body
    let txBuildupCtx = mkNodeTxBuildup connection timeout
    submitTxBody txBuildupCtx body []

-- | Build and submit a non-Marlowe transaction that mints tokens
-- | using files as a data exchange medium.
buildMinting
  :: (MonadError CliError m, C.IsShelleyBasedEra era)
  => (MonadIO m)
  => (MonadReader (CliEnv era) m)
  => LocalNodeConnectInfo
  -- ^ The connection info for the local node.
  -> SigningKeyFile
  -- ^ The file for required signing key.
  -> Either
      [(AddressInEra era, SigningKeyFile)]
      (NonEmpty (TokenName, Natural, AddressInEra era))
  -- ^ Minting policy related action.
  -- Pass token providers for burning or token distribution for minting.
  -> Maybe FilePath
  -- ^ The CIP-25 metadata for the minting, with keys for each token name.
  -> Maybe SlotNo
  -- ^ The slot number after which minting is no longer possible.
  -> AddressInEra era
  -- ^ The change address.
  -> TxBodyFile
  -- ^ The output file for the transaction body.
  -> Maybe Second
  -- ^ Number of seconds to wait for the transaction to be confirmed, if it is to be confirmed.
  -> m ()
  -- ^ Action to build the transaction body.
buildMinting connection signingKeyFile mintingAction metadataFile expires changeAddress (TxBodyFile bodyFile) timeout = do
  signingKey <- readSigningKey signingKeyFile
  let currencyIssuer = CurrencyIssuer changeAddress signingKey
  mintingAction' <- case mintingAction of
    Left (provider : providers) -> do
      let loadWallet (addr, skeyFile) = do
            skey <- readSigningKey skeyFile
            pure (addr, skey)
      provider' <- loadWallet provider
      providers' <- for providers loadWallet
      pure $ BurnAll currencyIssuer (provider' :| providers')
    Left _ -> do
      throwError "Token provider set is empty."
    Right tokenDistribution -> do
      -- Simplified version of the token distribution, where all tokens are minted at once.
      pure $
        Mint currencyIssuer $
          tokenDistribution <&> \(name, amount, addr) ->
            (RegularAddressRecipient addr, Nothing, [(name, amount)])
  metadataJson <- mapM decodeFileStrict metadataFile
  metadata <- forM metadataJson \case
    A.Object metadataProps -> pure metadataProps
    _ -> throwError "Metadata should file should contain a json object"
  let txBuildupCtx = mkNodeTxBuildup connection timeout
  (body, policy) <- buildMintingImpl txBuildupCtx mintingAction' metadata expires (PrintStats True)
  doWithCardanoEra $ liftCliIO $ writeFileTextEnvelope (File bodyFile) Nothing body
  liftIO . putStrLn $ read . show . unPolicyId $ policy

nonAdaValue :: Value -> Value
nonAdaValue value = value <> C.negateValue (C.lovelaceToValue (fromMaybe 0 $ C.valueToLovelace value))

-- | Build and submit a non-Marlowe transaction that mints tokens.
buildMintingImpl
  :: (MonadError CliError m, C.IsShelleyBasedEra era)
  => (MonadIO m)
  => (MonadReader (CliEnv era) m)
  => TxBuildupContext era
  -- ^ The connection info for the local node.
  -> MintingAction era
  -- ^ The token names, amount and a possible recipient addresses.
  -> Maybe Aeson.Object
  -- ^ The CIP-25 metadata for the minting, with keys for each token name.
  -> Maybe SlotNo
  -- ^ The slot number after which minting is no longer possible.
  -> PrintStats
  -- ^ Whether to print stats about the transaction.
  -> m (TxBody era, PolicyId)
  -- ^ Action to build the transaction body.
buildMintingImpl txBuildupCtx mintingAction metadataProps expires (PrintStats printStats) =
  do
    let queryCtx = toQueryContext txBuildupCtx
    era <- askEra
    protocol <- getProtocolParams queryCtx
    protocol' <- liftCli $ convertToLedgerProtocolParameters (babbageEraOnwardsToShelleyBasedEra era) protocol
    let CurrencyIssuer changeAddress signingKey = maIssuer mintingAction
        verification =
          verificationKeyHash $ toPaymentVerificationKey $ getVerificationKey signingKey
        (script, scriptHash) = mintingScript verification expires

        policy = PolicyId scriptHash

    (inputs, outputs, signingKeys, mint) <- case mintingAction of
      Mint _ tokenDistribution -> do
        let tokenDistribution' = do
              tokenDistribution <&> \(recipient, minAda, tokens) -> do
                let toValue (TokenName name) count = valueFromList . pure $ (AssetId policy (AssetName $ fromBuiltin name), C.Quantity $ toInteger count)
                    value = foldMap (uncurry toValue) tokens
                (recipient, value, minAda)

        -- TODO: use sensible coin selection here. Currently coin selection fails in the context of minting.
        utxos <- fmap (M.toList . unUTxO) $ queryByAddress queryCtx changeAddress

        assetsOutputs <- fmap catMaybes $ for utxos \(_, TxOut addr txOutValue _ _) -> do
          let value = C.txOutValueToValue txOutValue
              assetsValue = nonAdaValue value
          if assetsValue /= mempty
            then Just <$> makeBalancedTxOut era protocol' addr C.TxOutDatumNone assetsValue ReferenceScriptNone
            else pure Nothing

        outputs' <- fmap NonEmpty.toList $ for tokenDistribution' \(recipient, mintedValue, minAda) -> do
          let (address, possibleDatum) = case recipient of
                RegularAddressRecipient addr -> (addr, C.TxOutDatumNone)
                ScriptAddressRecipient addr txOutDatum -> do
                  (addr, txOutDatum)

          case minAda of
            Just minAda' -> do
              let adaValue = C.lovelaceToValue minAda'
                  value = adaValue <> mintedValue
              makeTxOut' address possibleDatum value
            Nothing ->
              makeBalancedTxOut era protocol' address possibleDatum mintedValue ReferenceScriptNone
        pure (map fst utxos, assetsOutputs <> outputs', [signingKey], foldMap (\(_, m, _) -> m) tokenDistribution')
      BurnAll _ providers -> do
        let signingKeys' = signingKey : (NonEmpty.toList . fmap snd $ providers)
            addresses = NonEmpty.toList . fmap fst $ providers

        allUtxos <-
          fmap (M.toList . unUTxO)
            $ queryUTxOs queryCtx
            $ QueryUTxOByAddress
              . S.fromList
              . fmap toAddressAny'
            $ addresses

        (inputs', tokensValueList, changes) <- fmap (unzip3 . catMaybes) $ for allUtxos \(txIn, TxOut addr txOutValue _ refScript) -> do
          let value = C.txOutValueToValue txOutValue
              toPolicyId C.AdaAssetId = Nothing
              toPolicyId (AssetId p _) = Just p
              tokensValue = C.valueFromList . filter (\(assetId, _) -> toPolicyId assetId == Just policy) . C.valueToList $ value
          if (tokensValue /= mempty || addr == changeAddress) && refScript == C.ReferenceScriptNone
            then do
              let changeValue = value <> negateValue tokensValue
                  -- Accumulate change value per token provider address.
                  -- `AddressInEra` has no `Ord` instance so we fallback to the
                  -- `AddressAny` which complicates a bit the final `outputs` build up.
                  change =
                    AM.AppendMap $
                      M.singleton
                        (toAddressAny' addr)
                        -- We want to keep all the ADA of the submitter and postpone
                        -- this particular change calculation to the final
                        -- transaction balancing in the `buildBody` function.
                        if addr == changeAddress
                          then nonAdaValue changeValue
                          else changeValue
              pure $ Just (txIn, tokensValue, change)
            else pure Nothing
        let tokensValue = fold tokensValueList
            changesMap = AM.unAppendMap $ fold changes

        outputs' <- fmap catMaybes $ for addresses \addr -> runMaybeT do
          value <- hoistMaybe (toAddressAny' addr `M.lookup` changesMap)
          MaybeT $
            if value /= mempty
              then Just <$> makeBalancedTxOut era protocol' addr C.TxOutDatumNone value ReferenceScriptNone
              else pure Nothing
        when (tokensValue == mempty) $ do
          throwError . CliError $ "Unable to find currency " <> show policy <> " tokens."
        pure (inputs', outputs', signingKeys', C.negateValue tokensValue)

    let minting =
          TxMintValue (babbageEraOnwardsToMaryEraOnwards era) mint
            . BuildTxWith
            . M.singleton policy
            $ SimpleScriptWitness
              ( case era of
                  BabbageEraOnwardsBabbage -> C.SimpleScriptInBabbage
                  BabbageEraOnwardsConway -> C.SimpleScriptInConway
              )
              (SScript script)

    metadata' <-
      case metadataProps of
        Just metadataProps' ->
          fmap (TxMetadataInEra (babbageEraOnwardsToShelleyBasedEra era))
            . liftCli
            . metadataFromJson TxMetadataJsonNoSchema
            . A.Object
            . KeyMap.singleton "721"
            . A.Object
            . KeyMap.singleton (Aeson.Key.fromString . BS8.unpack $ serialiseToRawBytesHex policy)
            $ A.Object metadataProps'
        _ -> pure TxMetadataNone

    (bodyContent, body) <-
      buildBodyWithContent
        queryCtx
        ([] :: [PayFromScript C.PlutusScriptV1])
        Nothing
        []
        inputs
        outputs
        Nothing
        changeAddress
        ((0,) <$> expires)
        []
        minting
        metadata'
        printStats
        False
        Nothing

    (_, body') <- submitTxBody' txBuildupCtx body bodyContent changeAddress signingKeys
    pure (body', policy)

-- | Create a minting script.
mintingScript
  :: Hash PaymentKey
  -- ^ The hash of the payment key.
  -> Maybe SlotNo
  -- ^ The last slot on which minting can occur, if any.
  -> (SimpleScript, ScriptHash)
  -- ^ The script and its hash.
mintingScript hash Nothing =
  let script = RequireSignature hash
   in ( script
      , hashScript $ SimpleScript script
      )
mintingScript hash (Just slot) =
  let script =
        RequireAllOf
          [ RequireSignature hash
          , RequireTimeBefore slot
          ]
   in ( script
      , hashScript $ SimpleScript script
      )

-- | Build a transaction paying into a Marlowe contract.
buildIncoming
  :: (MonadError CliError m, C.IsShelleyBasedEra era)
  => (MonadIO m)
  => (MonadReader (CliEnv era) m)
  => LocalNodeConnectInfo
  -- ^ The connection info for the local node.
  -> AddressInEra era
  -- ^ The script address.
  -> [SigningKeyFile]
  -- ^ The files for required signing keys.
  -> FilePath
  -- ^ The file containing the datum for the payment to the script.
  -> Value
  -- ^ The value to be paid to the script.
  -> [TxIn]
  -- ^ The transaction inputs.
  -> [(AddressInEra era, C.TxOutDatum C.CtxTx era, Value)]
  -- ^ The transaction outputs.
  -> AddressInEra era
  -- ^ The change address.
  -> Maybe FilePath
  -- ^ The file containing JSON metadata, if any.
  -> TxBodyFile
  -- ^ The output file for the transaction body.
  -> Maybe Second
  -- ^ Number of seconds to wait for the transaction to be confirmed, if it is to be confirmed.
  -> Bool
  -- ^ Whether to print statistics about the transaction.
  -> Bool
  -- ^ Assertion that the transaction is invalid.
  -> m TxId
  -- ^ Action to build the transaction body.
buildIncoming connection scriptAddress signingKeyFiles outputDatumFile outputValue inputs outputs changeAddress metadataFile (TxBodyFile bodyFile) timeout printStats invalid =
  do
    era <- askEra
    metadata <- readMaybeMetadata metadataFile
    outputDatum <- Datum <$> decodeFileBuiltinData outputDatumFile
    signingKeys <- mapM readSigningKey signingKeyFiles
    outputs' <- mapM (uncurry3 makeTxOut') outputs
    body <-
      buildBody
        (QueryNode connection)
        ([] :: [PayFromScript C.PlutusScriptV1])
        (Just $ buildPayToScript era scriptAddress outputValue outputDatum)
        []
        inputs
        outputs'
        Nothing
        changeAddress
        Nothing
        []
        TxMintNone
        metadata
        printStats
        invalid
    doWithCardanoEra $ liftCliIO $ writeFileTextEnvelope (File bodyFile) Nothing body
    let txBuildupCtx = mkNodeTxBuildup connection timeout
    submitBody txBuildupCtx body signingKeys invalid

buildReferenceScript
  :: forall era lang m
   . (C.IsPlutusScriptLanguage lang)
  => (MonadError CliError m)
  => (MonadReader (CliEnv era) m)
  => PlutusScript lang
  -> m (ReferenceScript era)
buildReferenceScript plutusScript = do
  era <- askEra
  pure $ ReferenceScript era . toScriptInAnyLang . toScript $ plutusScript

publisherAddress
  :: ScriptHash
  -> PublishingStrategy era
  -> BabbageEraOnwards era
  -> C.NetworkId
  -> AddressInEra era
publisherAddress scriptHash publishingStrategy era network = case publishingStrategy of
  PublishAtAddress addr -> addr
  PublishPermanently stake -> do
    let paymentCredentials = permanentPublisher scriptHash
    makeShelleyAddressInEra (babbageEraOnwardsToShelleyBasedEra era) network paymentCredentials stake

-- | Information required to publish a script
type ScriptPublishingInfo lang era =
  ( Lovelace
  , AddressInEra era
  , ValidatorInfo lang era
  )

buildScriptPublishingInfo
  :: forall lang era m
   . (MonadIO m)
  => (MonadError CliError m)
  => (MonadReader (CliEnv era) m)
  => (C.IsPlutusScriptLanguage lang)
  => QueryExecutionContext era
  -> PlutusScript lang
  -> PublishingStrategy era
  -> m (ScriptPublishingInfo lang era)
buildScriptPublishingInfo queryCtx plutusScript publishingStrategy = do
  era <- askEra
  protocol <- getProtocolParams queryCtx
  protocol' <- liftCli $ convertToLedgerProtocolParameters (babbageEraOnwardsToShelleyBasedEra era) protocol
  protocolVersion <- getMajorProtocolVersion queryCtx
  costModel <- getPV2CostModelParams queryCtx
  let networkId = queryContextNetworkId queryCtx
      scriptHash = hashScript . toScript $ plutusScript
      publisher = publisherAddress scriptHash publishingStrategy era networkId

  -- Stake information in this context is probably meaningless. We assign real staking when we use a reference.
  referenceScriptInfo <- validatorInfo' plutusScript Nothing era protocolVersion costModel networkId NoStakeAddress
  referenceScript <- buildReferenceScript plutusScript

  let (minAda, _) = adjustMinimumUTxO era protocol' publisher C.TxOutDatumNone mempty referenceScript
  pure (minAda, publisher, referenceScriptInfo)

buildPublishingImpl
  :: forall era m
   . (MonadError CliError m)
  => (MonadIO m)
  => (MonadReader (CliEnv era) m)
  => (C.IsShelleyBasedEra era)
  => TxBuildupContext era
  -- ^ The connection info for the local node or pure tx buildup context.
  -> SomePaymentSigningKey
  -- ^ The file for required signing key.
  -> Maybe SlotNo
  -- ^ The slot number after which publishing is no longer possible.
  -> AddressInEra era
  -- ^ The change address.
  -> PublishingStrategy era
  -> CoinSelectionStrategy
  -> PrintStats
  -> m ([TxBody era], MarloweScriptsRefs C.PlutusScriptV2 era)
buildPublishingImpl buildupCtx signingKey expires changeAddress publishingStrategy coinSelectionStrategy (PrintStats printStats) = do
  let queryCtx = toQueryContext buildupCtx
  pm <- buildScriptPublishingInfo queryCtx marloweValidator publishingStrategy
  pp <- buildScriptPublishingInfo queryCtx payoutValidator publishingStrategy
  po <- buildScriptPublishingInfo queryCtx openRolesValidator publishingStrategy

  let buildPublishedScriptTxOut (minAda, publisher, referenceValidator) = do
        referenceScript <- buildReferenceScript $ viScript referenceValidator
        makeTxOut publisher C.TxOutDatumNone (lovelaceToValue minAda) referenceScript

  initialUTxOs <- queryByAddress queryCtx changeAddress

  let publish utxos publishScripts = do
        outputs <- for publishScripts buildPublishedScriptTxOut
        (_, inputs, outputs') <-
          selectCoins
            queryCtx
            mempty
            outputs
            Nothing
            changeAddress
            coinSelectionStrategy
            (Just utxos)

        (txBodyContent, txBody) <-
          buildBodyWithContent
            queryCtx
            ([] :: [PayFromScript C.PlutusScriptV2])
            Nothing
            []
            inputs
            outputs'
            Nothing
            changeAddress
            ((0,) <$> expires)
            [hashSigningKey signingKey]
            TxMintNone
            TxMetadataNone
            printStats
            False
            (Just utxos)

        -- We track utxo set which we operate on so we can construct
        -- the next transaction.
        let TxBodyContent{txIns, txInsCollateral, txOuts} = txBodyContent
            txIns' = do
              let collateralTxIns = case txInsCollateral of
                    TxInsCollateralNone -> []
                    TxInsCollateral _ txInsCollateral' -> txInsCollateral'
              collateralTxIns <> (fst <$> txIns)
            txId = C.getTxId txBody
            newUtxos = Map.fromList $ foldMapFlipped (zip [0 ..] txOuts) \(idx, txOut@(C.TxOut addr _ _ _)) ->
              [(C.TxIn txId (C.TxIx idx), C.toCtxUTxOTxOut txOut) | addr == changeAddress]
            utxos' = unUTxO utxos `Map.withoutKeys` S.fromList txIns' <> newUtxos
        pure (UTxO utxos', txBody)

  (utxos', txBody1) <- publish initialUTxOs [pm, pp]
  (_, txBody2) <- publish utxos' [po]

  let findReferenceScriptOutput txBody plutusV2Script = do
        let txId = C.getTxId txBody
            C.TxBody C.TxBodyContent{txOuts} = txBody
            script =
              C.ScriptInAnyLang
                (C.PlutusScriptLanguage C.plutusScriptVersion)
                (toScript plutusV2Script)

            match (ix, txOut@(C.TxOut _ _ _ referenceScript)) = case referenceScript of
              C.ReferenceScript _ txOutScript ->
                if script == txOutScript
                  then Just (ix, txOut)
                  else Nothing
              _ -> Nothing

        (ix, txOut) <-
          liftEither $ note "Unable to find published script" $ listToMaybe $ mapMaybe match $ zip [0 ..] txOuts
        let txOut' = C.toCtxUTxOTxOut txOut
        pure (AnUTxO (C.TxIn txId (C.TxIx ix), txOut'))

  marloweRef <- do
    let (_, _, referenceScriptInfo) = pm
        ValidatorInfo{viScript} = referenceScriptInfo
    anUTxO <- findReferenceScriptOutput txBody1 viScript
    pure (anUTxO, referenceScriptInfo)

  rolePayoutRef <- do
    let (_, _, referenceScriptInfo) = pp
        ValidatorInfo{viScript} = referenceScriptInfo
    anUTxO <- findReferenceScriptOutput txBody1 viScript
    pure (anUTxO, referenceScriptInfo)

  openRoleRef <- do
    let (_, _, referenceScriptInfo) = po
        ValidatorInfo{viScript} = referenceScriptInfo
    anUTxO <- findReferenceScriptOutput txBody2 viScript
    pure (anUTxO, referenceScriptInfo)

  let txBodies = [txBody1, txBody2]
      serialiseAddress (_, addr, _) = T.unpack . C.serialiseAddress $ addr
      showScriptHash (_, _, ValidatorInfo{viHash}) = show viHash
      showMinAda (ma, _, _) = show ma
      showTxIn (AnUTxO (txIn, _), _) = show txIn

  when printStats $ liftIO do
    hPutStrLn stderr ""
    hPutStrLn stderr $
      "Marlowe script published at address: " <> serialiseAddress pm
    hPutStrLn stderr ""
    hPutStrLn stderr $ "Marlowe script hash: " <> showScriptHash pm
    hPutStrLn stderr ""
    hPutStrLn stderr $
      "Marlowe ref script UTxO min ADA: " <> showMinAda pm
    hPutStrLn stderr ""
    hPutStrLn stderr $ "Marlowe ref script UTxO: " <> showTxIn marloweRef

    hPutStrLn stderr ""
    hPutStrLn stderr $
      "Payout script published at address: " <> serialiseAddress pp
    hPutStrLn stderr ""
    hPutStrLn stderr $ "Payout script hash: " <> showScriptHash pp
    hPutStrLn stderr ""
    hPutStrLn stderr $ "Payout ref script UTxO min ADA: " <> showMinAda pp
    hPutStrLn stderr ""
    hPutStrLn stderr $ "Payout ref script UTxO: " <> showTxIn rolePayoutRef

    hPutStrLn stderr ""
    hPutStrLn stderr $ "Open role script published at address: " <> serialiseAddress po
    hPutStrLn stderr ""
    hPutStrLn stderr $ "Open role script hash: " <> showScriptHash po
    hPutStrLn stderr ""
    hPutStrLn stderr $ "Open role ref script UTxO min ADA: " <> showMinAda po
    hPutStrLn stderr ""
    hPutStrLn stderr $ "Open role ref script UTxO: " <> showTxIn openRoleRef
  pure (txBodies, MarloweScriptsRefs marloweRef rolePayoutRef openRoleRef)

-- CLI command handler.
buildPublishing
  :: forall era m
   . (MonadError CliError m)
  => (MonadIO m)
  => (MonadReader (CliEnv era) m)
  => (C.IsShelleyBasedEra era)
  => LocalNodeConnectInfo
  -- ^ The connection info for the local node.
  -> SigningKeyFile
  -- ^ The file for required signing key.
  -> Maybe SlotNo
  -- ^ The slot number after which publishing is no longer possible.
  -> AddressInEra era
  -- ^ The change address.
  -> Maybe (PublishingStrategy era)
  -> TxBodyFile
  -> Maybe Second
  -> PrintStats
  -> m ()
buildPublishing connection signingKeyFile expires changeAddress strategy (TxBodyFile bodyFile) timeout printStats = do
  let strategy' = fromMaybe (PublishAtAddress changeAddress) strategy
  signingKey <- readSigningKey signingKeyFile
  (txBodies, _) <-
    buildPublishingImpl @era
      (mkNodeTxBuildup connection timeout)
      signingKey
      expires
      changeAddress
      strategy'
      defaultCoinSelectionStrategy
      printStats

  for_ txBodies \txBody ->
    doWithCardanoEra $ liftCliIO $ writeFileTextEnvelope (File bodyFile) Nothing txBody
  let txBuildupCtx = mkNodeTxBuildup connection timeout
  for_ txBodies \txBody ->
    void $ submitTxBody txBuildupCtx txBody [signingKey]

publishImpl
  :: forall era m
   . (MonadError CliError m)
  => (MonadIO m)
  => (MonadReader (CliEnv era) m)
  => (C.IsShelleyBasedEra era)
  => TxBuildupContext era
  -- ^ The connection info for the local node.
  -> SomePaymentSigningKey
  -- ^ The file for required signing key.
  -> Maybe SlotNo
  -- ^ The slot number after which publishing is no longer possible.
  -> AddressInEra era
  -- ^ The change address.
  -> PublishingStrategy era
  -> CoinSelectionStrategy
  -> PrintStats
  -> m ([TxBody era], MarloweScriptsRefs C.PlutusScriptV2 era)
publishImpl txBuildupCtx signingKey expires changeAddress publishingStrategy coinSelectionStrategy printStats = do
  (txBodies, _) <-
    buildPublishingImpl @era
      txBuildupCtx
      signingKey
      expires
      changeAddress
      publishingStrategy
      coinSelectionStrategy
      printStats
  for_ txBodies \txBody ->
    submitTxBody txBuildupCtx txBody [signingKey]

  refs <- do
    let queryCtx = toQueryContext txBuildupCtx
    findMarloweScriptsRefs queryCtx publishingStrategy printStats >>= \case
      Nothing -> throwError . CliError $ "Unable to find just published scripts by tx:" <> show (map getTxId txBodies)
      Just m -> pure m
  pure (txBodies, refs)

findScriptRef
  :: forall era m
   . (MonadReader (CliEnv era) m)
  => (C.IsShelleyBasedEra era)
  => (MonadIO m)
  => (MonadError CliError m)
  => QueryExecutionContext era
  -- ^ Either already selected UTxO or connection info for the local node.
  -> ScriptHash
  -> PublishingStrategy era
  -> PrintStats
  -> m (Maybe (AnUTxO era, ValidatorInfo C.PlutusScriptV2 era))
findScriptRef queryCtx scriptHash publishingStrategy (PrintStats printStats) = do
  era <- askEra
  let networkId = queryContextNetworkId queryCtx
      publisher = publisherAddress scriptHash publishingStrategy era networkId

  when printStats $ liftIO do
    hPutStrLn stderr ""
    hPutStrLn stderr $
      "Searching for reference script at address: " <> (T.unpack . C.serialiseAddress $ publisher)
    hPutStrLn stderr ""
    hPutStrLn stderr $
      "Expected reference script hash: "
        <> show scriptHash

  runMaybeT do
    let query = FindReferenceScript C.plutusScriptVersion scriptHash
    (u@(AnUTxO (txIn, _)), script) <- MaybeT $ selectUtxosImpl queryCtx publisher query
    i <- lift $ buildValidatorInfo queryCtx script (Just txIn) NoStakeAddress
    pure (u, i)

findMarloweScriptsRefs
  :: forall era m
   . (MonadReader (CliEnv era) m)
  => (MonadIO m)
  => (MonadError CliError m)
  => (C.IsShelleyBasedEra era)
  => QueryExecutionContext era
  -- ^ Either already selected UTxOs or connection info to select UTxOs.
  -> PublishingStrategy era
  -> PrintStats
  -> m (Maybe (MarloweScriptsRefs C.PlutusScriptV2 era))
findMarloweScriptsRefs queryCtx publishingStrategy printStats = do
  let marloweHash = hashScript $ toScript marloweValidator
      payoutHash = hashScript $ toScript payoutValidator
      openRoleHash = hashScript $ toScript openRolesValidator

  runMaybeT do
    m <- MaybeT $ findScriptRef queryCtx marloweHash publishingStrategy printStats
    p <- MaybeT $ findScriptRef queryCtx payoutHash publishingStrategy printStats
    o <- MaybeT $ findScriptRef queryCtx openRoleHash publishingStrategy printStats
    pure $ MarloweScriptsRefs m p o

-- | CLI Command handler.
findPublished
  :: forall era m
   . (C.IsShelleyBasedEra era)
  => (MonadReader (CliEnv era) m)
  => (MonadIO m)
  => (MonadError CliError m)
  => QueryExecutionContext era
  -> Maybe (PublishingStrategy era)
  -> m ()
findPublished queryCtx publishingStrategy = do
  let publishingStrategy' = fromMaybe (PublishPermanently NoStakeAddress) publishingStrategy
  findMarloweScriptsRefs @era queryCtx publishingStrategy' (PrintStats True) >>= \case
    Just (MarloweScriptsRefs (mu, mi) (ru, ri) (ou, oi)) -> do
      let refJSON (AnUTxO (i, _)) ValidatorInfo{viHash} =
            A.object
              [ ("txIn", toJSON i)
              , ("hash", toJSON viHash)
              ]

      maybeWriteJson Nothing $
        A.object
          [ ("marlowe", refJSON mu mi)
          , ("payout", refJSON ru ri)
          , ("openRole", refJSON ou oi)
          ]
    Nothing -> maybeWriteJson Nothing A.Null

-- | TODO: Add support for constant validator.
-- | Build a transaction that spends from and pays to a Marlowe contract.
buildContinuing
  :: (MonadError CliError m, C.IsShelleyBasedEra era)
  => (MonadIO m)
  => (MonadReader (CliEnv era) m)
  => LocalNodeConnectInfo
  -- ^ The connection info for the local node.
  -> AddressInEra era
  -- ^ The script address.
  -> FilePath
  -- ^ The file containing the script validator.
  -> FilePath
  -- ^ The file containing the redeemer.
  -> FilePath
  -- ^ The file containing the datum for spending from the script.
  -> [SigningKeyFile]
  -- ^ The files for required signing keys.
  -> TxIn
  -- ^ The script eUTxO to be spent.
  -> FilePath
  -- ^ The file containing the datum for the payment to the script.
  -> Value
  -- ^ The value to be paid to the script.
  -> [TxIn]
  -- ^ The transaction inputs.
  -> [(AddressInEra era, C.TxOutDatum C.CtxTx era, Value)]
  -- ^ The transaction outputs.
  -> TxIn
  -- ^ The collateral.
  -> AddressInEra era
  -- ^ The change address.
  -> SlotNo
  -- ^ The first valid slot for the transaction.
  -> SlotNo
  -- ^ The last valid slot for the transaction.
  -> Maybe FilePath
  -- ^ The file containing JSON metadata, if any.
  -> TxBodyFile
  -- ^ The output file for the transaction body.
  -> Maybe Second
  -- ^ Number of seconds to wait for the transaction to be confirmed, if it is to be confirmed.
  -> Bool
  -- ^ Whether to print statistics about the transaction.
  -> Bool
  -- ^ Assertion that the transaction is invalid.
  -> m TxId
  -- ^ Action to build the transaction body.
buildContinuing connection scriptAddress validatorFile redeemerFile inputDatumFile signingKeyFiles txIn outputDatumFile outputValue inputs outputs collateral changeAddress minimumSlot maximumSlot metadataFile (TxBodyFile bodyFile) timeout printStats invalid =
  do
    metadata <- readMaybeMetadata metadataFile
    validator <- liftCliIO (readFileTextEnvelope (AsPlutusScript AsPlutusScriptV2) (File validatorFile))
    redeemer <- Redeemer <$> decodeFileBuiltinData redeemerFile
    inputDatum <- Datum <$> decodeFileBuiltinData inputDatumFile
    outputDatum <- Datum <$> decodeFileBuiltinData outputDatumFile
    signingKeys <- mapM readSigningKey signingKeyFiles
    outputs' <- mapM (uncurry3 makeTxOut') outputs
    era <- askEra
    body <-
      buildBody
        (QueryNode connection)
        [buildPayFromScript (C.PScript validator) (Just inputDatum) redeemer txIn]
        (Just $ buildPayToScript era scriptAddress outputValue outputDatum)
        []
        inputs
        outputs'
        (Just collateral)
        changeAddress
        (Just (minimumSlot, maximumSlot))
        (hashSigningKey <$> signingKeys)
        TxMintNone
        metadata
        printStats
        invalid
    doWithCardanoEra $ liftCliIO $ writeFileTextEnvelope (File bodyFile) Nothing body
    let txBuildupCtx = mkNodeTxBuildup connection timeout
    submitBody txBuildupCtx body signingKeys invalid

-- | TODO: Add support for constant validator.
-- | Build a transaction spending from a Marlowe contract.
buildOutgoing
  :: (MonadError CliError m, C.IsShelleyBasedEra era)
  => (MonadIO m)
  => (MonadReader (CliEnv era) m)
  => LocalNodeConnectInfo
  -- ^ The connection info for the local node.
  -> FilePath
  -- ^ The file containing the script validator.
  -> FilePath
  -- ^ The file containing the redeemer.
  -> FilePath
  -- ^ The file containing the datum for spending from the script.
  -> [SigningKeyFile]
  -- ^ The files for required signing keys.
  -> TxIn
  -- ^ The script eUTxO to be spent.
  -> [TxIn]
  -- ^ The transaction inputs.
  -> [(AddressInEra era, C.TxOutDatum C.CtxTx era, Value)]
  -- ^ The transaction outputs.
  -> TxIn
  -- ^ The collateral.
  -> AddressInEra era
  -- ^ The change address.
  -> SlotNo
  -- ^ The first valid slot for the transaction.
  -> SlotNo
  -- ^ The last valid slot for the transaction.
  -> Maybe FilePath
  -- ^ The file containing JSON metadata, if any.
  -> TxBodyFile
  -- ^ The output file for the transaction body.
  -> Maybe Second
  -- ^ Number of seconds to wait for the transaction to be confirmed, if it is to be confirmed.
  -> Bool
  -- ^ Whether to print statistics about the transaction.
  -> Bool
  -- ^ Assertion that the transaction is invalid.
  -> m TxId
  -- ^ Action to build the transaction body.
buildOutgoing connection validatorFile redeemerFile inputDatumFile signingKeyFiles txIn inputs outputs collateral changeAddress minimumSlot maximumSlot metadataFile (TxBodyFile bodyFile) timeout printStats invalid =
  do
    metadata <- readMaybeMetadata metadataFile
    validator <- liftCliIO (readFileTextEnvelope (AsPlutusScript AsPlutusScriptV2) (File validatorFile))
    redeemer <- Redeemer <$> decodeFileBuiltinData redeemerFile
    inputDatum <- Datum <$> decodeFileBuiltinData inputDatumFile
    signingKeys <- mapM readSigningKey signingKeyFiles
    outputs' <- mapM (uncurry3 makeTxOut') outputs
    body <-
      buildBody
        (QueryNode connection)
        [buildPayFromScript (C.PScript validator) (Just inputDatum) redeemer txIn]
        Nothing
        []
        inputs
        outputs'
        (Just collateral)
        changeAddress
        (Just (minimumSlot, maximumSlot))
        (hashSigningKey <$> signingKeys)
        TxMintNone
        metadata
        printStats
        invalid
    doWithCardanoEra $ liftCliIO $ writeFileTextEnvelope (File bodyFile) Nothing body
    let txBuildupCtx = mkNodeTxBuildup connection timeout
    submitBody txBuildupCtx body signingKeys invalid

-- | Collect information on paying from a script.
buildPayFromScript
  :: C.PlutusScriptOrReferenceInput lang
  -- ^ The script.
  -> Maybe Datum
  -- ^ The datum.
  -> Redeemer
  -- ^ The redeemer.
  -> TxIn
  -- ^ The eUTxO to be spent.
  -> PayFromScript lang
  -- ^ Payment information.
buildPayFromScript script datum redeemer txIn = PayFromScript{..}

buildPayToScript
  :: BabbageEraOnwards era
  -> AddressInEra era
  -- ^ The script address.
  -> Value
  -- ^ The value to be paid.
  -> Datum
  -- ^ The datum.
  -> PayToScript era
  -- ^ The payment information.
buildPayToScript era address value plutusDatum =
  let datumOut = toTxOutDatumInTx era plutusDatum
   in PayToScript{..}

-- | Hash a signing key.
hashSigningKey
  :: SomePaymentSigningKey
  -- ^ The key.
  -> Hash PaymentKey
  -- ^ The hash.
hashSigningKey =
  verificationKeyHash
    . toPaymentVerificationKey
    . getVerificationKey

-- | Build a balanced transaction body.
buildBody
  :: forall era lang m
   . (MonadError CliError m, C.IsShelleyBasedEra era)
  => (C.IsPlutusScriptLanguage lang)
  => (MonadIO m)
  => (MonadReader (CliEnv era) m)
  => QueryExecutionContext era
  -- ^ The connection info for the local node.
  -> [PayFromScript lang]
  -- ^ Payment information from the script, if any.
  -> Maybe (PayToScript era)
  -- ^ Payment information to the script, if any.
  -> [TxInEra era]
  -- ^ Transaction inputs with witnesses.
  -> [TxIn]
  -- ^ Transaction inputs.
  -> [TxOut CtxTx era]
  -- ^ Action for building the transaction output.
  -> Maybe TxIn
  -- ^ Collateral, if any.
  -> AddressInEra era
  -- ^ The change address.
  -> Maybe (SlotNo, SlotNo)
  -- ^ The valid slot range, if any.
  -> [Hash PaymentKey]
  -- ^ The extra required signatures.
  -> TxMintValue BuildTx era
  -- ^ The mint value.
  -> TxMetadataInEra era
  -- ^ The metadata.
  -> Bool
  -- ^ Whether to print statistics about the transaction.
  -> Bool
  -- ^ Assertion that the transaction is invalid.
  -> m (TxBody era)
  -- ^ The action to build the transaction body.
buildBody queryCtx payFromScript payToScript extraInputs inputs outputs collateral changeAddress slotRange extraSigners mintValue metadata printStats invalid =
  snd
    <$> buildBodyWithContent
      queryCtx
      payFromScript
      payToScript
      extraInputs
      inputs
      outputs
      collateral
      changeAddress
      slotRange
      extraSigners
      mintValue
      metadata
      printStats
      invalid
      Nothing

-- We need the `TxContext` when we want to resubmit a failing transaction with adjusted fees.
buildBodyWithContent
  :: forall era lang m
   . (MonadError CliError m, C.IsShelleyBasedEra era)
  => (C.IsPlutusScriptLanguage lang)
  => (MonadIO m)
  => (MonadReader (CliEnv era) m)
  => QueryExecutionContext era
  -- ^ The connection info for the local node.
  -> [PayFromScript lang]
  -- ^ Payment information from the script, if any.
  -> Maybe (PayToScript era)
  -- ^ Payment information to the script, if any.
  -> [TxInEra era]
  -- ^ Transaction inputs with witnesses.
  -> [TxIn]
  -- ^ Transaction inputs.
  -> [TxOut CtxTx era]
  -- ^ Action for building the transaction output.
  -> Maybe TxIn
  -- ^ Collateral, if any.
  -> AddressInEra era
  -- ^ The change address.
  -> Maybe (SlotNo, SlotNo)
  -- ^ The valid slot range, if any.
  -> [Hash PaymentKey]
  -- ^ The extra required signatures.
  -> TxMintValue BuildTx era
  -- ^ The mint value.
  -> TxMetadataInEra era
  -- ^ The metadata.
  -> Bool
  -- ^ Whether to print statistics about the transaction.
  -> Bool
  -- ^ Assertion that the transaction is invalid.
  -> Maybe (UTxO era)
  -- ^ The UTxO to use for the transaction.
  -> m (TxBodyContent C.BuildTx era, TxBody era)
  -- ^ The action to build the transaction body together with context.
buildBodyWithContent queryCtx payFromScript payToScript extraInputs inputs outputs collateral changeAddress slotRange extraSigners mintValue metadata printStats invalid possibleUTxOs =
  do
    era <- askEra
    start <- getSystemStart queryCtx
    history <- getEraHistory queryCtx
    protocol <- getProtocolParams queryCtx
    let protocol' = (\pp -> pp{protocolParamMaxTxExUnits = protocolParamMaxBlockExUnits pp}) protocol
    protocol'' <- liftCli $ convertToLedgerProtocolParameters (babbageEraOnwardsToShelleyBasedEra era) protocol'
    (scriptTxIn, txInsReferences) <-
      unzip <$> for payFromScript \s -> liftCli do
        redeemScript era s
    let txInsReference = do
          let step (C.TxInsReference _ refs) (C.TxInsReference w refs') = C.TxInsReference w (refs <> refs')
              step C.TxInsReferenceNone acc = acc
              step ref _ = ref
          foldr step C.TxInsReferenceNone txInsReferences

    let txInsCollateral = TxInsCollateral (babbageEraOnwardsToAlonzoEraOnwards era) $ maybeToList collateral
        txReturnCollateral = TxReturnCollateralNone
        txTotalCollateral = TxTotalCollateralNone
        txFee = TxFeeExplicit (babbageEraOnwardsToShelleyBasedEra era) 0
        txValidityLowerBound =
          maybe
            TxValidityNoLowerBound
            (TxValidityLowerBound (babbageEraOnwardsToAllegraEraOnwards era) . fst)
            slotRange
        txValidityUpperBound = TxValidityUpperBound (babbageEraOnwardsToShelleyBasedEra era) $ snd <$> slotRange
        txMetadata = metadata
        txAuxScripts = TxAuxScriptsNone
        txExtraKeyWits = TxExtraKeyWitnesses (babbageEraOnwardsToAlonzoEraOnwards era) extraSigners
        txProtocolParams = BuildTxWith $ Just protocol''
        txWithdrawals = TxWithdrawalsNone
        txCertificates = TxCertificatesNone
        txUpdateProposal = TxUpdateProposalNone
        txMintValue = mintValue
        txScriptValidity =
          if invalid
            then TxScriptValidity (babbageEraOnwardsToAlonzoEraOnwards era) ScriptInvalid
            else TxScriptValidityNone
        txIns = extraInputs <> scriptTxIn <> fmap makeTxIn inputs
        scriptTxOut = maybe [] (payScript era) payToScript
        txOuts = scriptTxOut <> outputs
        txProposalProcedures = Nothing
        txVotingProcedures = Nothing

    let txInsReferencesToTxIns C.TxInsReferenceNone = []
        txInsReferencesToTxIns (C.TxInsReference _ refs) = refs
        refTxIns = txInsReferencesToTxIns txInsReference
        allTxIns = (fst <$> txIns) <> refTxIns

    -- This UTxO set is used for change calculation.
    utxos <- case possibleUTxOs of
      Just utxos -> pure utxos
      Nothing -> do
        let q = QueryUTxOByTxIn . S.fromList $ allTxIns
        queryUTxOs queryCtx q
    let foundTxIns = map fst . M.toList . unUTxO $ utxos
        missingTxIns = [txIn | txIn <- allTxIns, txIn `notElem` foundTxIns]
    when (notNull missingTxIns) do
      throwError . CliError $ "Some inputs are missing from the chain (possibly reference inputs): " <> show missingTxIns

    let mkChangeTxOut value = do
          let txOutValue = mkTxOutValue era value
          C.TxOut changeAddress txOutValue TxOutDatumNone ReferenceScriptNone

        balancingLoop :: Integer -> C.Value -> m (C.TxBodyContent C.BuildTx era, BalancedTxBody era)
        balancingLoop counter changeValue = do
          -- changeTxOut@(TxOut addr (txOutValueToValue -> changeValue) datum ref) = do
          when (counter == 0) $ throwError . CliError $ do
            "Unsuccessful balancing of the transaction: " <> show (TxBodyContent{..})
          let -- Recompute execution units with full set of UTxOs, including change.
              buildTxBodyContent = TxBodyContent{..}{txOuts = mkChangeTxOut changeValue : txOuts}
              trial =
                makeTransactionBodyAutoBalance
                  (babbageEraOnwardsToShelleyBasedEra era)
                  start
                  (C.toLedgerEpochInfo history)
                  protocol''
                  S.empty
                  mempty
                  mempty
                  utxos
                  buildTxBodyContent
                  changeAddress
                  Nothing
          case trial of
            -- Correct for a negative balance in cases where execution units, and hence fees, have increased.
            Left (TxBodyErrorAdaBalanceNegative delta) -> do
              balancingLoop (counter - 1) (C.lovelaceToValue delta <> changeValue)
            Left err -> throwError . CliError $ show err
            Right balanced@(BalancedTxBody _ (TxBody TxBodyContent{txFee = fee}) _ _) -> do
              pure (buildTxBodyContent{txFee = fee}, balanced)

        totalIn = foldMap txOutValueValue . (M.elems . C.unUTxO) $ utxos
        totalOut = foldMap txOutValueValue txOuts
        totalMint = case txMintValue of
          C.TxMintValue _ value _ -> value
          _ -> mempty
        -- Initial setup is `fee = 0` - we output all the difference as a change and expect balancing error ;-)
        initialChange = totalIn <> totalMint <> C.negateValue totalOut

    (txBodyContent, BalancedTxBody _ txBody _ lovelace) <- balancingLoop 10 initialChange
    when printStats
      . liftIO
      $ do
        hPutStrLn stderr ""
        hPutStrLn stderr $ "Fee: " <> show lovelace
        let size = BS.length $ C.shelleyBasedEraConstraints (babbageEraOnwardsToShelleyBasedEra era) $ serialiseToCBOR txBody
            maxSize = fromIntegral $ protocolParamMaxTxSize protocol
            fractionSize = 100 * size `div` maxSize
        hPutStrLn stderr $ "Size: " <> show size <> " / " <> show maxSize <> " = " <> show fractionSize <> "%"
        let ExUnits memory steps = findExUnits txBody
            maxExecutionUnits = protocolParamMaxTxExUnits protocol
            fractionMemory = 100 * memory `div` maybe 0 executionMemory maxExecutionUnits
            fractionSteps = 100 * steps `div` maybe 0 executionSteps maxExecutionUnits
        hPutStrLn stderr "Execution units:"
        hPutStrLn stderr $
          "  Memory: "
            <> show memory
            <> " / "
            <> show (maybe 0 executionMemory maxExecutionUnits)
            <> " = "
            <> show fractionMemory
            <> "%"
        hPutStrLn stderr $
          "  Steps: "
            <> show steps
            <> " / "
            <> show (maybe 0 executionSteps maxExecutionUnits)
            <> " = "
            <> show fractionSteps
            <> "%"

    return (txBodyContent, txBody)

mkTxOutValue :: BabbageEraOnwards era -> Value -> TxOutValue era
mkTxOutValue BabbageEraOnwardsBabbage = C.TxOutValueShelleyBased ShelleyBasedEraBabbage . C.toLedgerValue MaryEraOnwardsBabbage
mkTxOutValue BabbageEraOnwardsConway = C.TxOutValueShelleyBased ShelleyBasedEraConway . C.toLedgerValue MaryEraOnwardsConway

babbageEraOnwardsToAllegraEraOnwards :: BabbageEraOnwards era -> C.AllegraEraOnwards era
babbageEraOnwardsToAllegraEraOnwards BabbageEraOnwardsBabbage = AllegraEraOnwardsBabbage
babbageEraOnwardsToAllegraEraOnwards BabbageEraOnwardsConway = AllegraEraOnwardsConway

-- | Total the execution units in a transaction.
findExUnits
  :: TxBody era
  -- ^ The transaction body.
  -> ExUnits
  -- ^ The execution units.
findExUnits (ShelleyTxBody ShelleyBasedEraShelley _ _ _ _ _) = mempty
findExUnits (ShelleyTxBody ShelleyBasedEraAllegra _ _ _ _ _) = mempty
findExUnits (ShelleyTxBody ShelleyBasedEraMary _ _ _ _ _) = mempty
findExUnits (ShelleyTxBody ShelleyBasedEraAlonzo _ _ (TxBodyScriptData _ _ (Redeemers redeemers)) _ _) =
  mconcat . fmap snd . M.elems $ redeemers
findExUnits (ShelleyTxBody ShelleyBasedEraAlonzo _ _ _ _ _) = mempty
findExUnits (ShelleyTxBody ShelleyBasedEraBabbage _ _ (TxBodyScriptData _ _ (Redeemers redeemers)) _ _) =
  mconcat . fmap snd . M.elems $ redeemers
findExUnits (ShelleyTxBody ShelleyBasedEraBabbage _ _ _ _ _) = mempty
findExUnits (ShelleyTxBody ShelleyBasedEraConway _ _ (TxBodyScriptData _ _ (Redeemers redeemers)) _ _) =
  mconcat . fmap snd . M.elems $ redeemers
findExUnits (ShelleyTxBody ShelleyBasedEraConway _ _ _ _ _) = mempty

-- | Sign and submit a transaction.
submit
  :: (MonadError CliError m, C.IsShelleyBasedEra era)
  => (MonadIO m)
  => (MonadReader (CliEnv era) m)
  => LocalNodeConnectInfo
  -- ^ The connection info for the local node.
  -> TxBodyFile
  -- ^ The transaction body file.
  -> [SigningKeyFile]
  -- ^ The signing key files.
  -> Second
  -- ^ Number of seconds to wait for the transaction to be confirmed.
  -> m TxId
  -- ^ The action to submit the transaction.
submit connection (TxBodyFile bodyFile) signingKeyFiles timeout =
  do
    era <- askEra
    body <- doWithCardanoEra $ liftCliIO $ readFileTextEnvelope (AsTxBody $ toAsType era) $ File bodyFile
    signings <- mapM readSigningKey signingKeyFiles
    let txBuildupCtx = mkNodeTxBuildup connection (Just timeout)
    submitTxBody txBuildupCtx body signings

-- Internal helper which throw when we try to submit invalid transaction.
submitBody
  :: (MonadError CliError m)
  => (MonadIO m)
  => (MonadReader (CliEnv era) m)
  => TxBuildupContext era
  -- ^ The connection info for the local node.
  -> TxBody era
  -- ^ The transaction body.
  -> [SomePaymentSigningKey]
  -- ^ The signing keys.
  -> Bool
  -- ^ Assertion that the transaction is invalid.
  -> m C.TxId
  -- ^ The action to submit the transaction.
submitBody txBuildupCtx txBody signings invalid = do
  let possiblyThrow = when invalid do
        throwError "Refusing to submit an invalid transaction: collateral would be lost."
  case txBuildupCtx of
    (NodeTxBuildup _ (DoSubmit _)) -> possiblyThrow
    (PureTxBuildup _ _) -> possiblyThrow
    _ -> pure ()
  submitTxBody txBuildupCtx txBody signings

-- | TxIn for transaction body.
type TxInEra era = (TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn era))

scriptWitness
  :: forall era lang m
   . (MonadError CliError m)
  => (C.IsPlutusScriptLanguage lang)
  => BabbageEraOnwards era
  -> PayFromScript lang
  -- ^ The payment information.
  -> m (C.BuildTxWith C.BuildTx (Witness WitCtxTxIn era))
scriptWitness era PayFromScript{..} = do
  scriptInEra <- liftCliMaybe "Script language not supported in era" $ toScriptLanguageInEra era
  let datum' = case datum of
        Just d -> ScriptDatumForTxIn . C.unsafeHashableScriptData . fromPlutusData $ toData d
        Nothing -> InlineScriptDatum
  pure $
    BuildTxWith . ScriptWitness ScriptWitnessForSpending $
      C.PlutusScriptWitness
        scriptInEra
        C.plutusScriptVersion
        script
        datum'
        (C.unsafeHashableScriptData $ fromPlutusData $ toData redeemer)
        (ExecutionUnits 0 0)

-- | Compute the transaction input for paying from a script.
redeemScript
  :: forall era lang m
   . (MonadError CliError m)
  => (C.IsPlutusScriptLanguage lang)
  => BabbageEraOnwards era
  -> PayFromScript lang
  -- ^ The payment information.
  -> m (TxInEra era, TxInsReference BuildTx era)
  -- ^ The transaction input.
redeemScript era p@PayFromScript{..} = do
  witness <- scriptWitness era p
  let refTxIn = case script of
        C.PScript{} -> TxInsReferenceNone
        C.PReferenceScript r _ -> C.TxInsReference era [r]
  pure ((txIn, witness), refTxIn)

-- | Compute the transaction output for paying to a script.
payScript
  :: BabbageEraOnwards era
  -> PayToScript era
  -- ^ The payment information.
  -> [TxOut CtxTx era]
  -- ^ The transaction input.
payScript era PayToScript{..} =
  [ TxOut
      address
      (mkTxOutValue era value)
      datumOut
      ReferenceScriptNone
  ]

-- | Compute transaction input for building a transaction.
makeTxIn
  :: TxIn
  -- ^ The transaction input.
  -> (TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn era))
  -- ^ The building for the transaction input.
makeTxIn = (,BuildTxWith $ KeyWitness KeyWitnessForSpending)

-- | Compute transaction output for building a transaction.
makeTxOut
  :: (MonadReader (CliEnv era) m)
  => AddressInEra era
  -- ^ The output address.
  -> TxOutDatum CtxTx era
  -- ^ The datum, if any.
  -> Value
  -- ^ The output value.
  -> ReferenceScript era
  -> m (TxOut CtxTx era)
  -- ^ Action for building the transaction output.
makeTxOut address datum value referenceScript = asksEra \era ->
  TxOut
    address
    (mkTxOutValue era value)
    datum
    referenceScript

-- | Compute transaction output for building a transaction.
makeTxOut'
  :: (MonadReader (CliEnv era) m)
  => AddressInEra era
  -- ^ The output address.
  -> TxOutDatum CtxTx era
  -- ^ The datum, if any.
  -> Value
  -- ^ The output value.
  -> m (TxOut CtxTx era)
  -- ^ Action for building the transaction output.
makeTxOut' address datum value = makeTxOut address datum value ReferenceScriptNone

makeBalancedTxOut
  :: (MonadReader (CliEnv era) m)
  => BabbageEraOnwards era
  -> LedgerProtocolParameters era
  -> AddressInEra era
  -> TxOutDatum CtxTx era
  -> Value
  -> ReferenceScript era
  -> m (TxOut CtxTx era)
makeBalancedTxOut era protocol address datum value referenceScript = do
  let (_, value') = adjustMinimumUTxO era protocol address datum value referenceScript
  makeTxOut address datum value' referenceScript

-- | Find the UTxOs at an address.
queryUtxos
  :: (MonadError CliError m)
  => (MonadIO m)
  => (MonadReader (CliEnv era) m)
  => LocalNodeConnectInfo
  -- ^ The connection info for the local node.
  -> AddressInEra era
  -- ^ The address.
  -> m (UTxO era)
  -- ^ Action query the UTxOs.
queryUtxos connection =
  queryInEra connection
    . QueryUTxO
    . QueryUTxOByAddress
    . S.singleton
    . toAddressAny'

filterUtxos :: OutputQuery era result -> UTxO era -> result
filterUtxos = do
  let filterByValue check (UTxO candidates) = do
        let query' (_, TxOut _ v _ _) = check $ txOutValueToValue v
            fromList = UTxO . M.fromList
            (oqrMatching, oqrNonMatching) =
              (fromList *** fromList)
                . partition query'
                $ M.toList candidates
        OutputQueryResult{..}
  \case
    LovelaceOnly amountCheck -> do
      filterByValue \v -> let l = selectLovelace v in lovelaceToValue l == v && amountCheck l
    AssetOnly asset -> do
      filterByValue \v -> length (valueToList v) == 2 && selectAsset v asset >= 1
    PolicyIdOnly policyId -> do
      filterByValue \v -> do
        let C.ValueNestedRep v' = C.valueToNestedRep v
            policyIds = [pId | C.ValueNestedBundle pId _ <- v']
        policyIds == [policyId]
    FindReferenceScript pv scriptHash -> do
      let hashScriptInAnyLang (C.ScriptInAnyLang _ script) = C.hashScript script
      \(UTxO candidates) -> tillFirstMatch (M.toList candidates) \case
        t@(_, TxOut _ _ _ (ReferenceScript _ script)) ->
          if hashScriptInAnyLang script == scriptHash
            then case (pv, script) of
              (PlutusScriptV2, C.ScriptInAnyLang _ (C.PlutusScript PlutusScriptV2 script')) -> Just (AnUTxO t, script')
              -- FIXME: Improve error reporting
              _ -> Nothing
            else Nothing
        _ -> Nothing

-- | Select a UTxOs at an address.
selectUtxosImpl
  :: (MonadError CliError m)
  => (MonadIO m)
  => (MonadReader (CliEnv era) m)
  => QueryExecutionContext era
  -- ^ The connection info qualified for the local node.
  -> AddressInEra era
  -- ^ The address.
  -> OutputQuery era result
  -- ^ Filter for the results.
  -> m result
selectUtxosImpl queryCtx address query =
  filterUtxos query <$> queryByAddress queryCtx address

-- | Select a UTxOs at an address.
selectUtxos
  :: (MonadError CliError m)
  => (MonadIO m)
  => (MonadReader (CliEnv era) m)
  => QueryExecutionContext era
  -- ^ The connection info for the local node.
  -> AddressInEra era
  -- ^ The address.
  -> Maybe (OutputQuery era (OutputQueryResult era))
  -- ^ Filter for the results.
  -> m ()
selectUtxos queryCtx address query =
  do
    matching <- case query of
      Nothing -> queryByAddress queryCtx address
      Just query' -> oqrMatching <$> selectUtxosImpl queryCtx address query'
    liftIO
      . mapM_ (print . fst)
      . M.toList
      . C.unUTxO
      $ matching

-- | Query the slot configuration parameters.
querySlotConfig
  :: (MonadError CliError m)
  => (MonadIO m)
  => (MonadReader (CliEnv era) m)
  => LocalNodeConnectInfo
  -- ^ The connection info for the local node.
  -> m SlotConfig
  -- ^ Action to extract the slot configuration.
querySlotConfig connection =
  do
    epochNo <- queryInEra connection QueryEpoch
    systemStart <-
      liftCliIO $
        queryNodeLocalState connection VolatileTip QuerySystemStart
    EraHistory interpreter <-
      liftCliIO
        . queryNodeLocalState connection VolatileTip
        $ QueryEraHistory
    let epochInfo =
          hoistEpochInfo (liftCli . runExcept) $
            interpreterToEpochInfo interpreter
    (slot0, slot1) <- epochInfoRange epochInfo epochNo
    time0 <- utcTimeToPOSIXSeconds <$> epochInfoSlotToUTCTime epochInfo systemStart slot0
    time1 <- utcTimeToPOSIXSeconds <$> epochInfoSlotToUTCTime epochInfo systemStart slot1
    let toMilliseconds x = 1_000 * (nominalDiffTimeToSeconds x `div'` 1)
        fromSlotNo = toInteger . unSlotNo
        deltaSlots = fromSlotNo $ slot1 - slot0
        deltaSeconds = time1 - time0
        scSlotLength = round $ toMilliseconds deltaSeconds % deltaSlots
        scSlotZeroTime = POSIXTime $ toMilliseconds time0 - scSlotLength * fromSlotNo slot0
    pure SlotConfig{..}

-- | Query the slot configuration parameters.
querySlotting
  :: (MonadError CliError m)
  => (MonadIO m)
  => (MonadReader (CliEnv era) m)
  => LocalNodeConnectInfo
  -- ^ The connection info for the local node.
  -> Maybe FilePath
  -- ^ The output file for the slot configuration.
  -> m ()
  -- ^ Action to extract the slot configuration.
querySlotting connection outputFile =
  querySlotConfig connection
    >>= maybeWriteJson outputFile

-- | Compute the maximum fee for any transaction.
maximumFee
  :: ProtocolParameters
  -> Lovelace
maximumFee ProtocolParameters{..} =
  let txFee :: Lovelace
      txFee = protocolParamTxFeeFixed + protocolParamTxFeePerByte * fromIntegral protocolParamMaxTxSize
      executionFee :: Rational
      executionFee =
        case (protocolParamPrices, protocolParamMaxTxExUnits) of
          (Just ExecutionUnitPrices{..}, Just ExecutionUnits{..}) ->
            priceExecutionSteps * fromIntegral executionSteps
              + priceExecutionMemory * fromIntegral executionMemory
          _ -> 0
   in txFee + round executionFee

-- | Calculate the minimum UTxO requirement for a value.
findMinUtxo
  :: forall m era
   . (MonadError CliError m)
  => (MonadReader (CliEnv era) m)
  => LedgerProtocolParameters era
  -> (AddressInEra era, Maybe Datum, Value)
  -> m Lovelace
findMinUtxo protocol (address, datum, value) =
  do
    era <- askEra
    let value' :: Value
        value' = value <> lovelaceToValue (max 500_000 (selectLovelace value) - selectLovelace value)
        trial :: TxOut CtxTx era
        trial =
          TxOut
            address
            (mkTxOutValue era value')
            ( maybe
                TxOutDatumNone
                (TxOutDatumInTx (babbageEraOnwardsToAlonzoEraOnwards era) . C.unsafeHashableScriptData . fromPlutusData . toData)
                datum
            )
            ReferenceScriptNone
    pure $ calculateMinimumUTxO (babbageEraOnwardsToShelleyBasedEra era) trial $ unLedgerProtocolParameters protocol

-- | Ensure that the minimum UTxO requirement is satisfied for outputs.
ensureMinUtxo
  :: forall m era
   . (MonadError CliError m)
  => (MonadReader (CliEnv era) m)
  => LedgerProtocolParameters era
  -> (AddressInEra era, C.TxOutDatum C.CtxTx era, Value)
  -> m (AddressInEra era, C.TxOutDatum C.CtxTx era, Value)
ensureMinUtxo protocol (address, datum, value) =
  do
    era <- askEra
    let value' :: Value
        value' = value <> lovelaceToValue (max 500_000 (selectLovelace value) - selectLovelace value)
        trial :: TxOut CtxTx era
        trial =
          TxOut
            address
            (mkTxOutValue era value')
            datum
            ReferenceScriptNone
        value'' = calculateMinimumUTxO (babbageEraOnwardsToShelleyBasedEra era) trial $ unLedgerProtocolParameters protocol
    pure
      ( address
      , datum
      , value <> (lovelaceToValue $ max value'' (selectLovelace value) - selectLovelace value)
      )

-- | Build a non-Marlowe transaction that cleans an address.
selectCoins
  :: forall m era
   . (MonadError CliError m)
  => (MonadIO m)
  => (MonadReader (CliEnv era) m)
  => QueryExecutionContext era
  -- ^ The connection info for the local node.
  -> Value
  -- ^ The value of the input from the script, if any.
  -> [TxOut CtxTx era]
  -- ^ The transaction outputs.
  -> Maybe (PayToScript era)
  -- ^ Otherwise unlisted outputs to the script address.
  -> AddressInEra era
  -- ^ The change address.
  -> CoinSelectionStrategy
  -- ^ Indicate which `UTxO`'s to ignore.
  -> Maybe (UTxO era)
  -- ^ The UTxO to use for the transaction.
  -> m (TxIn, [TxIn], [TxOut CtxTx era])
  -- ^ Action select the collateral, inputs, and outputs.
selectCoins queryCtx inputs outputs pay changeAddress CoinSelectionStrategy{..} possibleUTxOs =
  do
    era <- askEra
    let isInlineDatum C.TxOutDatumInline{} = True
        isInlineDatum _ = False

        include (txIn, TxOut _ _ datum ref) =
          (not csPreserveReferenceScripts || ref == ReferenceScriptNone)
            && (not csPreserveInlineDatums || not (isInlineDatum datum))
            && notElem txIn csPreserveTxIns

    -- Find the UTxOs that we have to work with.
    utxos <- fmap (filter include . M.toList . unUTxO) $ case possibleUTxOs of
      Just utxos -> pure utxos
      Nothing -> queryByAddress queryCtx changeAddress

    -- Fetch the protocol parameters
    protocol <- getProtocolParams queryCtx
    protocol' <- liftCli $ convertToLedgerProtocolParameters (babbageEraOnwardsToShelleyBasedEra era) protocol
    -- We want to consume as few UTxOs as possible, in order to keep the script context smaller.
    let -- Extract the value of a UTxO.
        txOutToValue :: TxOut ctx era -> Value
        txOutToValue (TxOut _ value _ _) = txOutValueToValue value
        -- Compute the value of all of the available UTxOs.
        universe :: Value
        universe = foldMap (txOutToValue . snd) utxos
        -- Bound the possible fee.
        fee :: Value
        fee = lovelaceToValue $ 2 * maximumFee protocol
        -- Test whether value only contains lovelace.
        onlyLovelace :: Value -> Bool
        onlyLovelace value = lovelaceToValue (selectLovelace value) == value
    -- Select the collateral.
    collateral <-
      case filter
        ( \candidate -> let value = txOutToValue $ snd candidate in onlyLovelace value && selectLovelace value >= selectLovelace fee
        )
        utxos of
        utxo : _ -> pure $ fst utxo
        [] -> do
          let adaOnlyUtxos = filter (\candidate -> let value = txOutToValue $ snd candidate in onlyLovelace value) utxos
          throwError . CliError $
            "No collateral found in " <> show utxos <> ". Ada only utxos: " <> show adaOnlyUtxos <> ". Fee is " <> show fee <> "."
        :: m TxIn
    -- Bound the lovelace that must be included with change
    minUtxo <-
      (<>)
        <$> findMinUtxo protocol' (changeAddress, Nothing, universe) -- Output to native tokens.
        <*> findMinUtxo protocol' (changeAddress, Nothing, mempty) -- Pure lovelace to change address.
    let -- Compute the value of the outputs.
        outgoing :: Value
        outgoing = foldMap txOutToValue outputs <> maybe mempty PayToScript.value pay
        -- Find the net additional input that is needed.
        incoming :: Value
        incoming = outgoing <> fee <> lovelaceToValue minUtxo <> negateValue inputs
        -- Remove the lovelace from a value.
        deleteLovelace :: Value -> Value
        deleteLovelace value = value <> negateValue (lovelaceToValue $ selectLovelace value)
        -- Compute the excess and missing tokens in a value.
        matchingCoins :: Value -> Value -> (Int, Int)
        matchingCoins required candidate =
          let delta :: [Quantity]
              delta =
                fmap snd
                  . valueToList
                  . deleteLovelace
                  $ candidate <> negateValue required
              excess :: Int
              excess = length $ filter (> 0) delta
              deficit :: Int
              deficit = length $ filter (< 0) delta
           in (excess, deficit)
    -- Ensure that coin selection for tokens is possible.
    unless (snd (matchingCoins incoming universe) == 0)
      . throwError
      . CliError
      $ "Insufficient tokens available for coin selection: "
        <> show incoming
        <> " required, but "
        <> show universe
        <> " available."
    -- Ensure that coin selection for lovelace is possible.
    unless (selectLovelace incoming <= selectLovelace universe)
      . throwError
      . CliError
      $ "Insufficient lovelace available for coin selection: "
        <> show incoming
        <> " required, but "
        <> show universe
        <> " available."
    -- Satisfy the native-token requirements.
    let -- Sort the UTxOs by their deficit, excess, and lovelace in priority order.
        priority :: Value -> (TxIn, TxOut CtxUTxO era) -> (Int, Int, Bool, Bool)
        priority required candidate =
          let candidate' :: Value
              candidate' = txOutToValue $ snd candidate
              excess :: Int
              deficit :: Int
              (excess, deficit) = matchingCoins required candidate'
              notOnlyLovelace :: Bool
              notOnlyLovelace = not $ onlyLovelace candidate'
              insufficientLovelace :: Bool
              insufficientLovelace = selectLovelace candidate' < selectLovelace required
           in ( deficit -- It's most important to not miss any required coins,
              , excess -- but we don't want extra coins;
              , notOnlyLovelace -- prefer lovelace-only UTxOs if there is no deficit,
              , insufficientLovelace -- and prefer UTxOs with sufficient lovelace.
              )
        -- Use a simple greedy algorithm to select coins.
        select :: Value -> [(TxIn, TxOut CtxUTxO era)] -> [(TxIn, TxOut CtxUTxO era)]
        select _ [] = []
        select required candidates =
          let -- Choose the best UTxO from the candidates.
              next :: (TxIn, TxOut CtxUTxO era)
              next = minimumBy (compare `on` priority required) candidates
              -- Determine the remaining candidates.
              candidates' :: [(TxIn, TxOut CtxUTxO era)]
              candidates' = delete next candidates
              -- Ignore negative quantities.
              filterPositive :: Value -> Value
              filterPositive = valueFromList . filter ((> 0) . snd) . valueToList
              -- Compute the remaining requirement.
              required' :: Value
              required' = filterPositive $ required <> negateValue (txOutToValue $ snd next)
           in -- Decide whether to continue.
              if required' == mempty
                then -- The requirements have been met.
                  pure next
                else -- The requirements have not been met.
                  next : select required' candidates'
        -- Select the coins.
        selection :: [(TxIn, TxOut CtxUTxO era)]
        selection = select incoming utxos
        -- Compute the native token change, if any.
        change :: Value
        change =
          -- This is the change required to balance native tokens.
          deleteLovelace $ -- The lovelace are irrelevant because pure-lovelace change is handled during the final balancing.
            (mconcat $ txOutToValue . snd <$> selection) -- The inputs selected by the algorithm for spending many include native tokens that weren't in the required `outputs`.
              <> negateValue incoming -- The tokens required by `outputs` (as represented in the `incoming` requirement) shouldn't be included as change.
              -- Compute the change that contains native tokens used for balancing, omitting ones explicitly specified in the outputs.
    output <-
      if change == mempty
        then pure []
        else do
          (a, d, v) <- ensureMinUtxo protocol' (changeAddress, C.TxOutDatumNone, change)
          (: []) <$> makeTxOut' a d v
    -- Return the coin selection.
    pure
      ( collateral
      , fst <$> selection
      , outputs <> output
      )
