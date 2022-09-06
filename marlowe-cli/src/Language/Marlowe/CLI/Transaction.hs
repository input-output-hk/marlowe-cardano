-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Submitting Marlowe transactions.
--
-----------------------------------------------------------------------------


{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE TypeFamilies       #-}


module Language.Marlowe.CLI.Transaction (
-- * Types
  TxInEra
-- * Building
, buildSimple
, buildIncoming
, buildContinuing
, buildOutgoing
, buildClean
, buildFaucet
, buildFaucet'
, buildMinting
, querySlotting
-- * Submitting
, submit
-- * Low-Level Functions
, buildBody
, buildPayFromScript
, buildPayToScript
, hashSigningKey
, queryInEra
, queryUtxos
, selectUtxos
, submitBody
, querySlotConfig
-- * Balancing
, findMinUtxo
, ensureMinUtxo
, selectCoins
) where


import Cardano.Api (AddressInEra (..), AsType (..), AssetId (..), AssetName (..), BalancedTxBody (..), BuildTx,
                    BuildTxWith (..), CardanoMode, ConsensusModeIsMultiEra (..), CtxTx, CtxUTxO, EraHistory (..),
                    ExecutionUnits (..), Hash, KeyWitnessInCtx (..), LocalNodeConnectInfo (..), Lovelace,
                    PaymentCredential (PaymentCredentialByScript), PaymentKey, PlutusScript, PlutusScriptV1,
                    PlutusScriptVersion (..), PolicyId (..), Quantity (..), QueryInEra (..), QueryInMode (..),
                    QueryInShelleyBasedEra (..), QueryUTxOFilter (..), Script (..), ScriptDataSupportedInEra,
                    ScriptDatum (..), ScriptHash, ScriptValidity (ScriptInvalid), ScriptWitness (..),
                    ScriptWitnessInCtx (..), ShelleyBasedEra (..), ShelleyWitnessSigningKey (..), SimpleScript (..),
                    SimpleScriptV2, SimpleScriptVersion (..), SlotNo (..), StakeAddressReference (NoStakeAddress),
                    TimeLocksSupported (..), TxAuxScripts (..), TxBody (..), TxBodyContent (..),
                    TxBodyErrorAutoBalance (..), TxBodyScriptData (..), TxCertificates (..), TxExtraKeyWitnesses (..),
                    TxFee (..), TxId, TxIn (..), TxInMode (..), TxInsCollateral (..),
                    TxInsReference (TxInsReferenceNone), TxIx (..), TxMetadataInEra (..),
                    TxMetadataJsonSchema (TxMetadataJsonNoSchema), TxMintValue (..), TxOut (..), TxOutDatum (..),
                    TxOutValue (..), TxReturnCollateral (TxReturnCollateralNone), TxScriptValidity (..),
                    TxTotalCollateral (TxTotalCollateralNone), TxUpdateProposal (..), TxValidityLowerBound (..),
                    TxValidityUpperBound (..), TxWithdrawals (..), UTxO (..), Value, WitCtxTxIn, Witness (..),
                    calculateMinimumUTxO, castVerificationKey, getTxId, getVerificationKey, hashScript, hashScriptData,
                    lovelaceToValue, makeShelleyAddressInEra, makeTransactionBodyAutoBalance, metadataFromJson,
                    negateValue, queryNodeLocalState, readFileTextEnvelope, selectAsset, selectLovelace,
                    serialiseToCBOR, serialiseToRawBytesHex, shelleyBasedEra, signShelleyTransaction,
                    submitTxToNodeLocal, txOutValueToValue, valueFromList, valueToList, valueToLovelace,
                    verificationKeyHash, writeFileTextEnvelope)
import Cardano.Api.Shelley (ExecutionUnitPrices (..), PlutusScriptOrReferenceInput (PScript), ProtocolParameters (..),
                            ReferenceScript (ReferenceScriptNone), SimpleScriptOrReferenceInput (SScript),
                            TxBody (ShelleyTxBody), fromPlutusData, protocolParamMaxBlockExUnits,
                            protocolParamMaxTxExUnits, protocolParamMaxTxSize)
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..))
import Cardano.Ledger.Alonzo.TxWitness (Redeemers (..))
import Cardano.Slotting.EpochInfo.API (epochInfoRange, epochInfoSlotToUTCTime, hoistEpochInfo)
import Control.Concurrent (threadDelay)
import Control.Monad (forM_, unless, void, when)
import Control.Monad.Except (MonadError, MonadIO, liftIO, runExcept, throwError)
import Data.Fixed (div')
import Data.Function (on)
import Data.List (delete, minimumBy)
import Data.Maybe (isNothing, maybeToList)
import Data.Ratio ((%))
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Language.Marlowe.CLI.IO (decodeFileBuiltinData, decodeFileStrict, liftCli, liftCliIO, maybeWriteJson,
                                readMaybeMetadata, readSigningKey)
import Language.Marlowe.CLI.Types (CliEnv, CliError (..), OutputQuery (..), PayFromScript (..), PayToScript (..),
                                   SomePaymentSigningKey, askEra, asksEra, doWithCardanoEra, toAddressAny', toAsType,
                                   toCollateralSupportedInEra, toEraInMode, toExtraKeyWitnessesSupportedInEra,
                                   toMultiAssetSupportedInEra, toPlutusScriptV1LanguageInEra, toShelleyBasedEra,
                                   toSimpleScriptV2LanguageInEra, toTxFeesExplicitInEra, toTxMetadataSupportedInEra,
                                   toTxScriptValiditySupportedInEra, toValidityLowerBoundSupportedInEra,
                                   toValidityNoUpperBoundSupportedInEra, toValidityUpperBoundSupportedInEra,
                                   withCardanoEra, withShelleyBasedEra)
import Ouroboros.Consensus.HardFork.History (interpreterToEpochInfo)
import Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult (..))
import Plutus.V1.Ledger.Api (Datum (..), POSIXTime (..), Redeemer (..), TokenName (..), fromBuiltin, toData)
import Plutus.V1.Ledger.SlotConfig (SlotConfig (..))
import System.IO (hPutStrLn, stderr)

import Control.Monad.Reader (MonadReader)
import qualified Data.Aeson as A (Value (Object))
import qualified Data.Aeson.Key as Aeson.Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS (length)
import qualified Data.ByteString.Char8 as BS8 (unpack)
import qualified Data.Map.Strict as M (elems, keysSet, singleton, toList)
import qualified Data.Set as S (empty, fromList, singleton)
import qualified Language.Marlowe.CLI.Types as PayToScript (PayToScript (value))

-- | Build a non-Marlowe transaction.
buildSimple :: MonadError CliError m
            => MonadIO m
            => MonadReader (CliEnv era) m
            => LocalNodeConnectInfo CardanoMode    -- ^ The connection info for the local node.
            -> [FilePath]                          -- ^ The files for required signing keys.
            -> [TxIn]                              -- ^ The transaction inputs.
            -> [(AddressInEra era, Maybe Datum, Value)]  -- ^ The transaction outputs.
            -> AddressInEra era                          -- ^ The change address.
            -> Maybe FilePath                      -- ^ The file containing JSON metadata, if any.
            -> FilePath                            -- ^ The output file for the transaction body.
            -> Maybe Int                           -- ^ Number of seconds to wait for the transaction to be confirmed, if it is to be confirmed.
            -> Bool                                -- ^ Whether to print statistics about the transaction.
            -> Bool                                -- ^ Assertion that the transaction is invalid.
            -> m TxId                              -- ^ Action to build the transaction body.
buildSimple connection signingKeyFiles inputs outputs changeAddress metadataFile bodyFile timeout printStats invalid =
  do
    metadata <- readMaybeMetadata metadataFile
    signingKeys <- mapM readSigningKey signingKeyFiles
    body <-
      buildBody
        connection
        []
        Nothing
        [] inputs outputs Nothing changeAddress
        Nothing
        []
        TxMintNone
        metadata
        printStats
        invalid
    doWithCardanoEra $ liftCliIO $ writeFileTextEnvelope bodyFile Nothing body
    forM_ timeout
      $ if invalid
          then const $ throwError "Refusing to submit an invalid transaction: collateral would be lost."
          else submitBody connection body signingKeys
    pure
      $ getTxId body


-- | Build a non-Marlowe transaction that cleans an address.
buildClean :: MonadError CliError m
           => MonadIO m
           => MonadReader (CliEnv era) m
           => LocalNodeConnectInfo CardanoMode  -- ^ The connection info for the local node.
           -> [FilePath]                        -- ^ The files for required signing keys.
           -> Lovelace                          -- ^ The value to be sent to addresses with tokens.
           -> AddressInEra era                        -- ^ The change address.
           -> Maybe (SlotNo, SlotNo)            -- ^ The valid slot range, if any.
           -> TxMintValue BuildTx era    -- ^ The mint value.
           -> TxMetadataInEra era        -- ^ The metadata.
           -> FilePath                          -- ^ The output file for the transaction body.
           -> Maybe Int                         -- ^ Number of seconds to wait for the transaction to be confirmed, if it is to be confirmed.
           -> m TxId                            -- ^ Action to build the transaction body.
buildClean connection signingKeyFiles lovelace changeAddress range mintValue metadata bodyFile timeout =
  do
    signingKeys <- mapM readSigningKey signingKeyFiles
    utxos <-
      fmap (M.toList . unUTxO)
        .  queryInEra connection
        . QueryUTxO
        . QueryUTxOByAddress
        . S.singleton
        $ toAddressAny' changeAddress
    let
      minting =
        case mintValue of
          TxMintValue _ minting' _ -> minting'
          _                        -> mempty
      inputs = fst <$> utxos
      extractValue (TxOut _ value _ _) = txOutValueToValue value
      total = mconcat $ extractValue . snd <$> utxos
      outputs =
        [
          (changeAddress, Nothing, value <> lovelaceToValue lovelace)
        |
          value <- valueFromList . pure <$> valueToList (total <> minting)
        , isNothing $ valueToLovelace value
        ]
    body <-
      buildBody
        connection
        []
        Nothing
        [] inputs outputs Nothing changeAddress
        range
        []
        mintValue
        metadata
        False
        False
    doWithCardanoEra $ liftCliIO $ writeFileTextEnvelope bodyFile Nothing body
    forM_ timeout
      $ submitBody connection body signingKeys
    pure
      $ getTxId body


-- | Build a non-Marlowe transaction that fills and address from a faucet.
buildFaucet :: MonadError CliError m
            => MonadIO m
            => MonadReader (CliEnv era) m
            => LocalNodeConnectInfo CardanoMode  -- ^ The connection info for the local node.
            -> Value                             -- ^ The value to be sent to the funded addresses.
            -> [AddressInEra era]                      -- ^ The addresses to receive funds.
            -> AddressInEra era                        -- ^ The faucet address.
            -> FilePath                          -- ^ The required signing key.
            -> Maybe Int                         -- ^ Number of seconds to wait for the transaction to be confirmed, if it is to be confirmed.
            -> m TxId                            -- ^ Action to build the transaction body.
buildFaucet connection value destAddresses fundAddress fundSigningKeyFile timeout =
  do
    utxos <-
      fmap (M.toList . unUTxO)
        . queryInEra connection
        . QueryUTxO
        . QueryUTxOByAddress
        . S.singleton
        $ toAddressAny' fundAddress
    fundSigningKey <- readSigningKey fundSigningKeyFile
    let
      inputs = fst <$> utxos
      extractValue (TxOut _ v _ _) = txOutValueToValue v
      total = mconcat $ extractValue . snd <$> utxos
      lovelace = lovelaceToValue . toEnum . (`div` 2) . fromEnum $ selectLovelace total
      value' = mconcat $ replicate (length destAddresses) value
      outputs =
        (fundAddress, Nothing, total <> negateValue value' <> negateValue lovelace)
          : [(destAddress, Nothing, value) | destAddress <- destAddresses]
    body <-
      buildBody
        connection
        []
        Nothing
        [] inputs outputs Nothing fundAddress
        Nothing
        []
        TxMintNone
        TxMetadataNone
        False
        False
    forM_ timeout
      $ submitBody connection body [fundSigningKey]
    pure
      $ getTxId body


-- | Build a non-Marlowe transaction that fills and address from a faucet.
buildFaucet' :: MonadError CliError m
             => MonadIO m
             => MonadReader (CliEnv era) m
             => LocalNodeConnectInfo CardanoMode  -- ^ The connection info for the local node.
             -> Value                             -- ^ The value to be sent to the funded addresses.
             -> [AddressInEra era]                      -- ^ The funded addresses.
             -> FilePath                          -- ^ The output file for the transaction body.
             -> Maybe Int                         -- ^ Number of seconds to wait for the transaction to be confirmed, if it is to be confirmed.
             -> m TxId                            -- ^ Action to build the transaction body.
buildFaucet' connection value addresses bodyFile timeout =
  do
    era <- askEra
    let
      network = localNodeNetworkId connection
      script = RequireAllOf []
      witness =
        BuildTxWith
          . ScriptWitness ScriptWitnessForSpending
          $ SimpleScriptWitness (toSimpleScriptV2LanguageInEra era) SimpleScriptV2 (SScript script)
      changeAddress = withShelleyBasedEra era $ makeShelleyAddressInEra
          network
          (PaymentCredentialByScript . hashScript . SimpleScript SimpleScriptV2 $ script)
          NoStakeAddress
    utxos <-
      fmap (M.toList . unUTxO)
        . queryInEra connection
        . QueryUTxO
        . QueryUTxOByAddress
        . S.singleton
        $ toAddressAny' changeAddress
    let
      inputs = [(txIn, witness) | txIn <- fst <$> utxos]
      extractValue (TxOut _ v _ _) = txOutValueToValue v
      total = mconcat $ extractValue . snd <$> utxos
      lovelace = lovelaceToValue . toEnum . (`div` 2) . fromEnum $ selectLovelace total
      value' = mconcat $ replicate (length addresses) value
      outputs =
        (changeAddress, Nothing, total <> negateValue value' <> negateValue lovelace)
          : [(fundedAddress, Nothing, value) | fundedAddress <- addresses]
    body <-
      buildBody
        connection
        []
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
    withCardanoEra era $ liftCliIO $ writeFileTextEnvelope bodyFile Nothing body
    forM_ timeout
      $ submitBody connection body []
    pure
      $ getTxId body


-- | Build a non-Marlowe transaction that mints tokens.
buildMinting :: MonadError CliError m
             => MonadIO m
             => MonadReader (CliEnv era) m
             => LocalNodeConnectInfo CardanoMode  -- ^ The connection info for the local node.
             -> FilePath                          -- ^ The file for required signing key.
             -> [TokenName]                       -- ^ The token names.
             -> Maybe FilePath                    -- ^ The CIP-25 metadata for the minting, with keys for each token name.
             -> Integer                           -- ^ The number of each token to mint.
             -> Maybe SlotNo                      -- ^ The slot number after which minting is no longer possible.
             -> Lovelace                          -- ^ The value to be sent to addresses with tokens.
             -> AddressInEra era                        -- ^ The change address.
             -> FilePath                          -- ^ The output file for the transaction body.
             -> Maybe Int                         -- ^ Number of seconds to wait for the transaction to be confirmed, if it is to be confirmed.
             -> m ()                              -- ^ Action to build the transaction body.
buildMinting connection signingKeyFile tokenNames metadataFile count expires lovelace changeAddress bodyFile timeout =
  do
    metadata <- sequence $ decodeFileStrict <$> metadataFile
    signingKey <- readSigningKey signingKeyFile
    era <- askEra
    let
      verification =
        verificationKeyHash
          $ case signingKey of
              Left  k -> getVerificationKey k
              Right k -> castVerificationKey $ getVerificationKey k
      (script, scriptHash) = mintingScript verification expires
      policy = PolicyId scriptHash
      minting =
        valueFromList
          [
            (
              AssetId policy (AssetName $ fromBuiltin name)
            , Quantity count
            )
          |
            TokenName name <- tokenNames
          ]
      mintValue =
        TxMintValue (toMultiAssetSupportedInEra era) minting
          . BuildTxWith
          . M.singleton policy
          $ SimpleScriptWitness (toSimpleScriptV2LanguageInEra era) SimpleScriptV2 (SScript script)
    metadata' <-
      case metadata of
        Just (A.Object metadata'') -> fmap (TxMetadataInEra (toTxMetadataSupportedInEra era))
                                        . liftCli
                                        . metadataFromJson TxMetadataJsonNoSchema
                                        . A.Object
                                        . KeyMap.singleton "721"
                                        . A.Object
                                        . KeyMap.singleton (Aeson.Key.fromString . BS8.unpack $ serialiseToRawBytesHex policy)
                                        $ A.Object metadata''
        _                          -> pure TxMetadataNone
    void
      $ buildClean
          connection
          [signingKeyFile]
          lovelace
          changeAddress
          ((0, ) <$> expires)
          mintValue
          metadata'
          bodyFile
          timeout
    liftIO . putStrLn $ "PolicyID " <> show policy


-- | Create a minting script.
mintingScript :: Hash PaymentKey                           -- ^ The hash of the payment key.
              -> Maybe SlotNo                              -- ^ The last slot on which minting can occur, if any.
              -> (SimpleScript SimpleScriptV2, ScriptHash) -- ^ The script and its hash.
mintingScript hash Nothing =
  let
    script = RequireSignature hash
  in
    (
      script
    , hashScript $ SimpleScript SimpleScriptV2 script
    )
mintingScript hash (Just slot) =
  let
    script =
      RequireAllOf
        [
          RequireSignature hash
        , RequireTimeBefore TimeLocksInSimpleScriptV2 slot
        ]
  in
    (
      script
    , hashScript $ SimpleScript SimpleScriptV2 script
    )


-- | Build a transaction paying into a Marlowe contract.
buildIncoming :: MonadError CliError m
              => MonadIO m
              => MonadReader (CliEnv era) m
              => LocalNodeConnectInfo CardanoMode    -- ^ The connection info for the local node.
              -> AddressInEra era                          -- ^ The script address.
              -> [FilePath]                          -- ^ The files for required signing keys.
              -> FilePath                            -- ^ The file containing the datum for the payment to the script.
              -> Value                               -- ^ The value to be paid to the script.
              -> [TxIn]                              -- ^ The transaction inputs.
              -> [(AddressInEra era, Maybe Datum, Value)]  -- ^ The transaction outputs.
              -> AddressInEra era                          -- ^ The change address.
              -> Maybe FilePath                      -- ^ The file containing JSON metadata, if any.
              -> FilePath                            -- ^ The output file for the transaction body.
              -> Maybe Int                           -- ^ Number of seconds to wait for the transaction to be confirmed, if it is to be confirmed.
              -> Bool                                -- ^ Whether to print statistics about the transaction.
              -> Bool                                -- ^ Assertion that the transaction is invalid.
              -> m TxId                              -- ^ Action to build the transaction body.
buildIncoming connection scriptAddress signingKeyFiles outputDatumFile outputValue inputs outputs changeAddress metadataFile bodyFile timeout printStats invalid =
  do
    metadata <- readMaybeMetadata metadataFile
    outputDatum <- Datum <$> decodeFileBuiltinData outputDatumFile
    signingKeys <- mapM readSigningKey signingKeyFiles
    body <-
      buildBody
        connection
        []
        (Just $ buildPayToScript scriptAddress outputValue outputDatum)
        [] inputs outputs Nothing changeAddress
        Nothing
        []
        TxMintNone
        metadata
        printStats
        invalid
    doWithCardanoEra $ liftCliIO $ writeFileTextEnvelope bodyFile Nothing body
    forM_ timeout
      $ if invalid
          then const $ throwError "Refusing to submit an invalid transaction: collateral would be lost."
          else submitBody connection body signingKeys
    pure
      $ getTxId body


-- | Build a transaction that spends from and pays to a Marlowe contract.
buildContinuing :: MonadError CliError m
                => MonadIO m
                => MonadReader (CliEnv era) m
                => LocalNodeConnectInfo CardanoMode    -- ^ The connection info for the local node.
                -> AddressInEra era                          -- ^ The script address.
                -> FilePath                            -- ^ The file containing the script validator.
                -> FilePath                            -- ^ The file containing the redeemer.
                -> FilePath                            -- ^ The file containing the datum for spending from the script.
                -> [FilePath]                          -- ^ The files for required signing keys.
                -> TxIn                                -- ^ The script eUTxO to be spent.
                -> FilePath                            -- ^ The file containing the datum for the payment to the script.
                -> Value                               -- ^ The value to be paid to the script.
                -> [TxIn]                              -- ^ The transaction inputs.
                -> [(AddressInEra era, Maybe Datum, Value)]  -- ^ The transaction outputs.
                -> TxIn                                -- ^ The collateral.
                -> AddressInEra era                          -- ^ The change address.
                -> SlotNo                              -- ^ The first valid slot for the transaction.
                -> SlotNo                              -- ^ The last valid slot for the transaction.
                -> Maybe FilePath                      -- ^ The file containing JSON metadata, if any.
                -> FilePath                            -- ^ The output file for the transaction body.
                -> Maybe Int                           -- ^ Number of seconds to wait for the transaction to be confirmed, if it is to be confirmed.
                -> Bool                                -- ^ Whether to print statistics about the transaction.
                -> Bool                                -- ^ Assertion that the transaction is invalid.
                -> m TxId                              -- ^ Action to build the transaction body.
buildContinuing connection scriptAddress validatorFile redeemerFile inputDatumFile signingKeyFiles txIn outputDatumFile outputValue inputs outputs collateral changeAddress minimumSlot maximumSlot metadataFile bodyFile timeout printStats invalid =
  do
    metadata <- readMaybeMetadata metadataFile
    validator <- liftCliIO (readFileTextEnvelope (AsPlutusScript AsPlutusScriptV1) validatorFile)
    redeemer <- Redeemer <$> decodeFileBuiltinData redeemerFile
    inputDatum <- Datum <$> decodeFileBuiltinData inputDatumFile
    outputDatum <- Datum <$> decodeFileBuiltinData outputDatumFile
    signingKeys <- mapM readSigningKey signingKeyFiles
    body <-
      buildBody
        connection
        [buildPayFromScript validator inputDatum redeemer txIn]
        (Just $ buildPayToScript scriptAddress outputValue outputDatum)
        [] inputs outputs (Just collateral) changeAddress
        (Just (minimumSlot, maximumSlot))
        (hashSigningKey <$> signingKeys)
        TxMintNone
        metadata
        printStats
        invalid
    doWithCardanoEra $ liftCliIO $ writeFileTextEnvelope bodyFile Nothing body
    forM_ timeout
      $ if invalid
          then const $ throwError "Refusing to submit an invalid transaction: collateral would be lost."
          else submitBody connection body signingKeys
    pure
      $ getTxId body


-- | Build a transaction spending from a Marlowe contract.
buildOutgoing :: MonadError CliError m
              => MonadIO m
              => MonadReader (CliEnv era) m
              => LocalNodeConnectInfo CardanoMode    -- ^ The connection info for the local node.
              -> FilePath                            -- ^ The file containing the script validator.
              -> FilePath                            -- ^ The file containing the redeemer.
              -> FilePath                            -- ^ The file containing the datum for spending from the script.
              -> [FilePath]                          -- ^ The files for required signing keys.
              -> TxIn                                -- ^ The script eUTxO to be spent.
              -> [TxIn]                              -- ^ The transaction inputs.
              -> [(AddressInEra era, Maybe Datum, Value)]  -- ^ The transaction outputs.
              -> TxIn                                -- ^ The collateral.
              -> AddressInEra era                          -- ^ The change address.
              -> SlotNo                              -- ^ The first valid slot for the transaction.
              -> SlotNo                              -- ^ The last valid slot for the transaction.
              -> Maybe FilePath                      -- ^ The file containing JSON metadata, if any.
              -> FilePath                            -- ^ The output file for the transaction body.
              -> Maybe Int                           -- ^ Number of seconds to wait for the transaction to be confirmed, if it is to be confirmed.
              -> Bool                                -- ^ Whether to print statistics about the transaction.
              -> Bool                                -- ^ Assertion that the transaction is invalid.
              -> m TxId                              -- ^ Action to build the transaction body.
buildOutgoing connection validatorFile redeemerFile inputDatumFile signingKeyFiles txIn inputs outputs collateral changeAddress minimumSlot maximumSlot metadataFile bodyFile timeout printStats invalid =
  do
    metadata <- readMaybeMetadata metadataFile
    validator <- liftCliIO (readFileTextEnvelope (AsPlutusScript AsPlutusScriptV1) validatorFile)
    redeemer <- Redeemer <$> decodeFileBuiltinData redeemerFile
    inputDatum <- Datum <$> decodeFileBuiltinData inputDatumFile
    signingKeys <- mapM readSigningKey signingKeyFiles
    body <-
      buildBody
        connection
        [buildPayFromScript validator inputDatum redeemer txIn]
        Nothing
        [] inputs outputs (Just collateral) changeAddress
        (Just (minimumSlot, maximumSlot))
        (hashSigningKey <$> signingKeys)
        TxMintNone
        metadata
        printStats
        invalid
    doWithCardanoEra $ liftCliIO $ writeFileTextEnvelope bodyFile Nothing body
    forM_ timeout
      $ if invalid
          then const $ throwError "Refusing to submit an invalid transaction: collateral would be lost."
          else submitBody connection body signingKeys
    pure $ getTxId body


-- | Collect information on paying from a script.
buildPayFromScript :: PlutusScript PlutusScriptV1  -- ^ The script.
                   -> Datum                        -- ^ The datum.
                   -> Redeemer                     -- ^ The redeemer.
                   -> TxIn                         -- ^ The eUTxO to be spent.
                   -> PayFromScript                -- ^ Payment information.
buildPayFromScript script datum redeemer txIn = PayFromScript{..}


-- | Collect information on paying to a script.
buildPayToScript :: AddressInEra era  -- ^ The script address.
                 -> Value             -- ^ The value to be paid.
                 -> Datum             -- ^ The datum.
                 -> PayToScript era   -- ^ The payment information.
buildPayToScript address value datum =
  let
    datumOut  = fromPlutusData . toData $ datum
    datumHash = hashScriptData datumOut
  in
    PayToScript{..}


-- | Hash a signing key.
hashSigningKey :: SomePaymentSigningKey  -- ^ The key.
               -> Hash PaymentKey        -- ^ The hash.
hashSigningKey =
  verificationKeyHash
    . either
        getVerificationKey
        (castVerificationKey . getVerificationKey)


-- | Build a balanced transaction body.
buildBody :: MonadError CliError m
          => MonadIO m
          => MonadReader (CliEnv era) m
          => LocalNodeConnectInfo CardanoMode    -- ^ The connection info for the local node.
          -> [PayFromScript]                     -- ^ Payment information from the script, if any.
          -> Maybe (PayToScript era)      -- ^ Payment information to the script, if any.
          -> [TxInEra era]                       -- ^ Transaction inputs.
          -> [TxIn]                              -- ^ Transaction inputs.
          -> [(AddressInEra era, Maybe Datum, Value)]  -- ^ Transaction outputs.
          -> Maybe TxIn                          -- ^ Collateral, if any.
          -> AddressInEra era                    -- ^ The change address.
          -> Maybe (SlotNo, SlotNo)              -- ^ The valid slot range, if any.
          -> [Hash PaymentKey]                   -- ^ The extra required signatures.
          -> TxMintValue BuildTx era      -- ^ The mint value.
          -> TxMetadataInEra era          -- ^ The metadata.
          -> Bool                                -- ^ Whether to print statistics about the transaction.
          -> Bool                                -- ^ Assertion that the transaction is invalid.
          -> m (TxBody era)               -- ^ The action to build the transaction body.
buildBody connection payFromScript payToScript extraInputs inputs outputs collateral changeAddress slotRange extraSigners mintValue metadata printStats invalid =
  do
    start <- queryAny connection   QuerySystemStart
    history <- queryAny connection $ QueryEraHistory CardanoModeIsMultiEra
    protocol <- queryInEra connection QueryProtocolParameters
    era <- askEra
    let
      protocol' = (\pp -> pp {protocolParamMaxTxExUnits = protocolParamMaxBlockExUnits pp}) protocol
      txInsCollateral    = TxInsCollateral (toCollateralSupportedInEra era) $ maybeToList collateral
      txReturnCollateral = TxReturnCollateralNone
      txTotalCollateral  = TxTotalCollateralNone
      txFee              = TxFeeExplicit (toTxFeesExplicitInEra era) 0
      txValidityRange    = (
                             maybe
                               TxValidityNoLowerBound
                               (TxValidityLowerBound (toValidityLowerBoundSupportedInEra era) . fst)
                               slotRange
                           , maybe
                               (TxValidityNoUpperBound (toValidityNoUpperBoundSupportedInEra era))
                               (TxValidityUpperBound (toValidityUpperBoundSupportedInEra era) . snd)
                               slotRange
                           )
      txMetadata         = metadata
      txAuxScripts       = TxAuxScriptsNone
      txExtraKeyWits     = TxExtraKeyWitnesses (toExtraKeyWitnessesSupportedInEra era) extraSigners
      txProtocolParams   = BuildTxWith $ Just protocol'
      txWithdrawals      = TxWithdrawalsNone
      txCertificates     = TxCertificatesNone
      txUpdateProposal   = TxUpdateProposalNone
      txInsReference     = TxInsReferenceNone
      txMintValue        = mintValue
      txScriptValidity   = if invalid
                             then TxScriptValidity (toTxScriptValiditySupportedInEra era) ScriptInvalid
                             else TxScriptValidityNone
      scriptTxIn = redeemScript era <$> payFromScript
      txIns = extraInputs <> scriptTxIn <> fmap makeTxIn inputs
      scriptTxOut = maybe [] (payScript era) payToScript
    txOuts <- (scriptTxOut <>) <$>  mapM (uncurry3 makeTxOut) outputs
    utxo <-
      queryInEra connection
        . QueryUTxO
        . QueryUTxOByTxIn
        . S.fromList
        $ fst
        <$> txIns
    let eraInMode = toEraInMode era
    -- Compute the change.
    BalancedTxBody _ change _ <-
      liftCli
        $ withShelleyBasedEra era
        $ makeTransactionBodyAutoBalance
            eraInMode
            start
            history
            protocol'
            S.empty
            utxo
            TxBodyContent{..}
            changeAddress
            Nothing
    let
      -- Recompute execution units with full set of UTxOs, including change.
      trial =
        withShelleyBasedEra era $ makeTransactionBodyAutoBalance
          eraInMode
          start
          history
          protocol'
          S.empty
          utxo
          (TxBodyContent{..} {txOuts = change : txOuts})
          changeAddress
          Nothing
      -- Correct for a negative balance in cases where execution units, and hence fees, have increased.
      change' =
        case (change, trial) of
          (TxOut _ (TxOutValue _ value) _ _, Left (TxBodyErrorAdaBalanceNegative delta)) ->
            TxOut changeAddress (TxOutValue (toMultiAssetSupportedInEra era) $ value <> lovelaceToValue delta) TxOutDatumNone ReferenceScriptNone
          _ -> change
    -- Construct the body with correct execution units and fees.
    BalancedTxBody txBody _ lovelace <-
      liftCli
        $ withShelleyBasedEra era
        $ makeTransactionBodyAutoBalance
            eraInMode
            start
            history
            protocol'
            S.empty
            utxo
            (TxBodyContent{..} {txOuts = change' : txOuts, txInsReference = TxInsReferenceNone })
            changeAddress
            Nothing
    when printStats
      . liftIO
      $ do
        hPutStrLn stderr ""
        hPutStrLn stderr $ "Fee: " <> show lovelace
        let
          size = BS.length $ withCardanoEra era $ serialiseToCBOR txBody
          maxSize = fromIntegral $ protocolParamMaxTxSize protocol
          fractionSize = 100 * size `div` maxSize
        hPutStrLn stderr $ "Size: " <> show size <> " / " <> show maxSize <> " = " <> show fractionSize <> "%"
        let
          ExUnits memory steps = findExUnits txBody
          maxExecutionUnits = protocolParamMaxTxExUnits protocol
          fractionMemory = 100 * memory `div` maybe 0 executionMemory maxExecutionUnits
          fractionSteps  = 100 * steps  `div` maybe 0 executionSteps  maxExecutionUnits
        hPutStrLn stderr "Execution units:"
        hPutStrLn stderr $ "  Memory: " <> show memory <> " / " <> show (maybe 0 executionMemory maxExecutionUnits) <> " = " <> show fractionMemory <> "%"
        hPutStrLn stderr $ "  Steps: "  <> show steps  <> " / " <> show (maybe 0 executionSteps  maxExecutionUnits) <> " = " <> show fractionSteps  <> "%"
    return txBody


-- | Total the execution units in a transaction.
findExUnits :: TxBody era  -- ^ The transaction body.
            -> ExUnits            -- ^ The execution units.
findExUnits (ShelleyTxBody ShelleyBasedEraBabbage  _ _ (TxBodyScriptData _ _ (Redeemers redeemers)) _ _) =
  mconcat . fmap snd . M.elems $ redeemers
findExUnits (ShelleyTxBody ShelleyBasedEraAlonzo   _ _ (TxBodyScriptData _ _ (Redeemers redeemers)) _ _) =
  mconcat . fmap snd . M.elems $ redeemers
findExUnits _ = mempty


-- | Sign and submit a transaction.
submit :: MonadError CliError m
       => MonadIO m
       => MonadReader (CliEnv era) m
       => LocalNodeConnectInfo CardanoMode  -- ^ The connection info for the local node.
       -> FilePath                          -- ^ The transaction body file.
       -> [FilePath]                        -- ^ The signing key files.
       -> Int                               -- ^ Number of seconds to wait for the transaction to be confirmed.
       -> m TxId                            -- ^ The action to submit the transaction.
submit connection bodyFile signingKeyFiles timeout =
  do
    era <- askEra
    body <- doWithCardanoEra $ liftCliIO $ readFileTextEnvelope (AsTxBody $ toAsType era) bodyFile
    signings <- mapM readSigningKey signingKeyFiles
    submitBody connection body signings timeout


-- | Sign and submit a transaction.
submitBody :: MonadError CliError m
           => MonadIO m
           => MonadReader (CliEnv era) m
           => LocalNodeConnectInfo CardanoMode  -- ^ The connection info for the local node.
           -> TxBody era                 -- ^ The transaction body.
           -> [SomePaymentSigningKey]           -- ^ The signing keys.
           -> Int                               -- ^ Number of seconds to wait for the transaction to be confirmed.
           -> m TxId                            -- ^ The action to submit the transaction.
submitBody connection body signings timeout =
  do
    era <- askEra
    let
      tx =
        withShelleyBasedEra era $ signShelleyTransaction body
          $ either WitnessPaymentKey WitnessPaymentExtendedKey
          <$> signings
    result <-
      liftIO
        . submitTxToNodeLocal connection
        . TxInMode tx
        $ toEraInMode era
    case result of
      SubmitSuccess     -> do
                             let
                               txId = getTxId body
                             when (timeout > 0)
                               $ waitForUtxos connection timeout [TxIn txId $ TxIx 0]
                             pure txId
      SubmitFail reason -> throwError . CliError $ show reason


-- | Wait for transactions to be confirmed as UTxOs.
waitForUtxos :: MonadError CliError m
             => MonadIO m
             => MonadReader (CliEnv era) m
             => LocalNodeConnectInfo CardanoMode  -- ^ The connection info for the local node.
             -> Int                               -- ^ Number of seconds to wait for the transaction to be confirmed.
             -> [TxIn]                            -- ^ The transactions to wait for.
             -> m ()                              -- ^ Action to wait for the transaction confirmations.
waitForUtxos connection timeout txIns =
  let
    pause = 5
    txIns' = S.fromList txIns
    go 0 = throwError "Timeout waiting for transaction to be confirmed."
    go n = do
             liftIO . threadDelay $ pause * 1_000_000
             utxos <-
               queryInEra connection
                 . QueryUTxO
                 . QueryUTxOByTxIn
                 $ txIns'
             if M.keysSet (unUTxO utxos) == txIns'
               then pure ()
               else go (n - 1 :: Int)
  in
    go . ceiling $ fromIntegral timeout / (fromIntegral pause :: Double)


-- | TxIn for transaction body.
type TxInEra era = (TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn era))


-- | Compute the transaction input for paying from a script.
redeemScript :: ScriptDataSupportedInEra era
             -> PayFromScript  -- ^ The payment information.
             -> TxInEra era    -- ^ The transaction input.
redeemScript era PayFromScript{..} =
  (
    txIn
  , BuildTxWith
      . ScriptWitness ScriptWitnessForSpending
      $ PlutusScriptWitness
        (toPlutusScriptV1LanguageInEra era)
        PlutusScriptV1
        (PScript script)
        (ScriptDatumForTxIn . fromPlutusData $ toData datum)
        (fromPlutusData $ toData redeemer)
        (ExecutionUnits 0 0)
  )


-- | Compute the transaction output for paying to a script.
payScript :: ScriptDataSupportedInEra era
          -> PayToScript era   -- ^ The payment information.
          -> [TxOut CtxTx era] -- ^ The transaction input.
payScript era PayToScript{..} =
  [
    TxOut
      address
      (TxOutValue (toMultiAssetSupportedInEra era) value)
      (TxOutDatumInTx era datumOut)
      ReferenceScriptNone
  ]


-- | Compute transaction input for building a transaction.
makeTxIn :: TxIn                                                        -- ^ The transaction input.
         -> (TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn era)) -- ^ The building for the transaction input.
makeTxIn = (, BuildTxWith $ KeyWitness KeyWitnessForSpending)


-- | Uncurry a triplet.
uncurry3 :: (a1 -> a2 -> a3 -> b)
         -> (a1, a2, a3)
         -> b
uncurry3 f (x, y, z) = f x y z


-- | Compute transaction output for building a transaction.
makeTxOut :: MonadReader (CliEnv era) m
          => AddressInEra era                 -- ^ The output address.
          -> Maybe Datum                -- ^ The datum, if any.
          -> Value                      -- ^ The output value.
          -> m (TxOut CtxTx era) -- ^ Action for building the transaction output.
makeTxOut address datum value = asksEra \era -> TxOut
  address
  (TxOutValue (toMultiAssetSupportedInEra era) value)
  (maybe TxOutDatumNone (TxOutDatumInTx era . fromPlutusData . toData) datum)
  ReferenceScriptNone


-- | Query a node.
queryAny :: MonadError CliError m
         => MonadIO m
         => LocalNodeConnectInfo CardanoMode -- ^ The connection info for the local node.
         -> QueryInMode CardanoMode a        -- ^ The query.
         -> m a                              -- ^ Action for running the query.
queryAny connection =
 liftCliIO
   . queryNodeLocalState connection Nothing


-- | Query a node in an era.
queryInEra :: MonadError CliError m
            => MonadIO m
            => MonadReader (CliEnv era) m
            => LocalNodeConnectInfo CardanoMode     -- ^ The connection info for the local node.
            -> QueryInShelleyBasedEra era a  -- ^ The query.
            -> m a                                  -- ^ Action for running the query.
queryInEra connection q = do
  era <- askEra
  res <- liftCliIO
    $ queryNodeLocalState connection Nothing
    $ QueryInEra (toEraInMode era)
    $ QueryInShelleyBasedEra (toShelleyBasedEra era) q
  liftCli res


-- | Find the UTxOs at an address.
queryUtxos :: MonadError CliError m
           => MonadIO m
           => MonadReader (CliEnv era) m
           => LocalNodeConnectInfo CardanoMode  -- ^ The connection info for the local node.
           -> AddressInEra era                        -- ^ The address.
           -> m (UTxO era)               -- ^ Action query the UTxOs.
queryUtxos connection =
  queryInEra connection
    . QueryUTxO
    . QueryUTxOByAddress
    . S.singleton
    . toAddressAny'


-- | Select a UTxOs at an address.
selectUtxos :: MonadError CliError m
            => MonadIO m
            => MonadReader (CliEnv era) m
            => LocalNodeConnectInfo CardanoMode  -- ^ The connection info for the local node.
            -> AddressInEra era                        -- ^ The address.
            -> OutputQuery                       -- ^ Filter for the results.
            -> m ()                              -- ^ Action query the UTxOs.
selectUtxos connection address query =
  do
    UTxO candidates <- queryUtxos connection address
    let
      query' (_, TxOut _ value' _ _) =
        let
          value = txOutValueToValue value'
          count = length $ valueToList value
        in
          case query of
            AllOutput        -> True
            LovelaceOnly{..} -> count == 1 && selectLovelace value    >= lovelace
            AssetOnly{..}    -> count == 2 && selectAsset value asset >= 1
    liftIO
      . mapM_ (print . fst)
      . filter query'
      $ M.toList candidates


-- | Query the slot configuration parameters.
querySlotConfig :: MonadError CliError m
                => MonadIO m
                => MonadReader (CliEnv era) m
                => LocalNodeConnectInfo CardanoMode  -- ^ The connection info for the local node.
                -> m SlotConfig                      -- ^ Action to extract the slot configuration.
querySlotConfig connection =
  do
    epochNo <- queryInEra connection QueryEpoch
    systemStart <-
      liftCliIO
        $ queryNodeLocalState connection Nothing QuerySystemStart
    EraHistory _ interpreter <-
      liftCliIO
        . queryNodeLocalState connection Nothing
        $ QueryEraHistory CardanoModeIsMultiEra
    let
      epochInfo =
        hoistEpochInfo (liftCli . runExcept)
          $ interpreterToEpochInfo interpreter
    (slot0, slot1) <- epochInfoRange epochInfo epochNo
    time0 <- utcTimeToPOSIXSeconds <$> epochInfoSlotToUTCTime epochInfo systemStart slot0
    time1 <- utcTimeToPOSIXSeconds <$> epochInfoSlotToUTCTime epochInfo systemStart slot1
    let
      toMilliseconds x = 1000 * (nominalDiffTimeToSeconds x `div'` 1)
      fromSlotNo = toInteger . unSlotNo
      deltaSlots = fromSlotNo $ slot1 - slot0
      deltaSeconds = time1 - time0
      scSlotLength = round $ toMilliseconds deltaSeconds % deltaSlots
      scSlotZeroTime = POSIXTime $ toMilliseconds time0 - scSlotLength * fromSlotNo slot0
    pure SlotConfig{..}


-- | Query the slot configuration parameters.
querySlotting :: MonadError CliError m
              => MonadIO m
              => MonadReader (CliEnv era) m
              => LocalNodeConnectInfo CardanoMode  -- ^ The connection info for the local node.
              -> Maybe FilePath                    -- ^ The output file for the slot configuration.
              -> m ()                              -- ^ Action to extract the slot configuration.
querySlotting connection outputFile =
  querySlotConfig connection
    >>= maybeWriteJson outputFile


-- | Compute the maximum fee for any transaction.
maximumFee :: ProtocolParameters
           -> Lovelace
maximumFee ProtocolParameters{..} =
  let
    txFee :: Lovelace
    txFee = fromIntegral $ protocolParamTxFeeFixed + protocolParamTxFeePerByte * protocolParamMaxTxSize
    executionFee :: Rational
    executionFee =
      case (protocolParamPrices, protocolParamMaxTxExUnits) of
        (Just ExecutionUnitPrices{..}, Just ExecutionUnits{..}) -> priceExecutionSteps  * fromIntegral executionSteps
                                                                 + priceExecutionMemory * fromIntegral executionMemory
        _                                                       -> 0
  in
    txFee + round executionFee


-- | Calculate the minimum UTxO requirement for a value.
findMinUtxo :: forall m era
            .  MonadError CliError m
            => MonadReader (CliEnv era) m
            => ProtocolParameters
            -> (AddressInEra era, Maybe Datum, Value)
            -> m Value
findMinUtxo protocol (address, datum, value) =
  do
    era <- askEra
    let
      value' :: Value
      value' = value <> lovelaceToValue (maximum [500_000, selectLovelace value] - selectLovelace value)
      trial :: TxOut CtxTx era
      trial =
        TxOut
          address
          (TxOutValue (toMultiAssetSupportedInEra era) value')
          (maybe TxOutDatumNone (TxOutDatumInTx era . fromPlutusData . toData) datum)
          ReferenceScriptNone
    case calculateMinimumUTxO (withShelleyBasedEra era shelleyBasedEra) trial protocol of
       Right value'' -> pure value''
       Left  e       -> throwError . CliError $ show e


-- | Ensure that the minimum UTxO requirement is satisfied for outputs.
ensureMinUtxo :: forall m era
              .  MonadError CliError m
              => MonadReader (CliEnv era) m
              => ProtocolParameters
              -> (AddressInEra era, Maybe Datum, Value)
              -> m (AddressInEra era, Maybe Datum, Value)
ensureMinUtxo protocol (address, datum, value) =
  do
    era <- askEra
    let
      value' :: Value
      value' = value <> lovelaceToValue (maximum [500_000, selectLovelace value] - selectLovelace value)
      trial :: TxOut CtxTx era
      trial =
        TxOut
          address
          (TxOutValue (toMultiAssetSupportedInEra era) value')
          (maybe TxOutDatumNone (TxOutDatumInTx era . fromPlutusData . toData) datum)
          ReferenceScriptNone
    case calculateMinimumUTxO (withShelleyBasedEra era shelleyBasedEra) trial protocol of
       Right value'' -> pure (address, datum, value <> (lovelaceToValue $ maximum [selectLovelace value'', selectLovelace value] - selectLovelace value))
       Left  e       -> throwError . CliError $ show e


-- | Build a non-Marlowe transaction that cleans an address.
selectCoins :: forall m era
            .  MonadError CliError m
            => MonadIO m
            => MonadReader (CliEnv era) m
            => LocalNodeConnectInfo CardanoMode                            -- ^ The connection info for the local node.
            -> Value                                                       -- ^ The value of the input from the script, if any.
            -> [(AddressInEra era, Maybe Datum, Value)]                    -- ^ The transaction outputs.
            -> Maybe (PayToScript era)                                     -- ^ Otherwise unlisted outputs to the script address.
            -> AddressInEra era                                            -- ^ The change address.
            -> m (TxIn, [TxIn], [(AddressInEra era, Maybe Datum, Value)])  -- ^ Action select the collateral, inputs, and outputs.
selectCoins connection inputs outputs pay changeAddress =
  do
    -- Find the UTxOs that we have to work with.
    utxos <-
      fmap (M.toList . unUTxO)
        . queryInEra connection
        . QueryUTxO
        . QueryUTxOByAddress
        . S.singleton
        $ toAddressAny' changeAddress
        :: m [(TxIn, TxOut CtxUTxO era)]
    -- Fetch the protocol parameters
    protocol <- queryInEra connection QueryProtocolParameters
             :: m ProtocolParameters
    -- We want to consume as few UTxOs as possible, in order to keep the script context smaller.
    let
      -- Extract the value of a UTxO.
      txOutToValue :: TxOut CtxUTxO era -> Value
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
      case filter (\candidate -> let value = txOutToValue $ snd candidate in onlyLovelace value && selectLovelace value >= selectLovelace fee) utxos of
        utxo : _ -> pure $ fst utxo
        []       -> throwError . CliError $ "No collateral found in " <> show utxos <> "."
      :: m TxIn
    -- Bound the lovelace that must be included with change
    minUtxo <-
      (<>) <$> findMinUtxo protocol (changeAddress, Nothing, universe)  -- Output to native tokens.
           <*> findMinUtxo protocol (changeAddress, Nothing, mempty  )  -- Pure lovelace to change address.
      :: m Value
    let
      -- Compute the value of the outputs.
      outgoing :: Value
      outgoing = foldMap (\(_, _, value) -> value) outputs <> maybe mempty PayToScript.value pay
      -- Find the net additional input that is needed.
      incoming :: Value
      incoming = outgoing <> fee <> minUtxo <> negateValue inputs
      -- Remove the lovelace from a value.
      deleteLovelace :: Value -> Value
      deleteLovelace value = value <> negateValue (lovelaceToValue $ selectLovelace value)
      -- Compute the excess and missing tokens in a value.
      matchingCoins :: Value -> Value -> (Int, Int)
      matchingCoins required candidate =
        let
          delta =
            fmap snd
              . valueToList
              . deleteLovelace
              $ candidate <> negateValue required
          excess  = length $ filter (> 0) delta
          deficit = length $ filter (< 0) delta
        in
          (excess, deficit)
    -- Ensure that coin selection for tokens is possible.
    unless (snd (matchingCoins incoming universe) == 0)
      . throwError
      . CliError
      $ "Insufficient tokens available for coin selection: "
      <> show incoming <> " required, but " <> show universe <> " available."
    -- Ensure that coin selection for lovelace is possible.
    unless (selectLovelace incoming <= selectLovelace universe)
      . throwError
      . CliError
      $ "Insufficient lovelace available for coin selection: "
      <> show incoming <> " required, but " <> show universe <> " available."
    -- Satisfy the native-token requirements.
    let
      -- Sort the UTxOs by their deficit, excess, and lovelace in priority order.
      priority :: Value -> (TxIn, TxOut CtxUTxO era) -> (Int, Int, Bool, Bool)
      priority required candidate =
        let
          candidate' = txOutToValue $ snd candidate
          (excess, deficit) = matchingCoins required candidate'
          notOnlyLovelace = not $ onlyLovelace candidate'
          insufficientLovelace = selectLovelace candidate' < selectLovelace required
        in
          (
            deficit               -- It's most important to not miss any required coins,
          , excess                -- but we don't want extra coins;
          , notOnlyLovelace       -- prefer lovelace-only UTxOs if there is no deficit,
          , insufficientLovelace  -- and prefer UTxOs with sufficient lovelace.
          )
      -- Use a simple greedy algorithm to select coins.
      select :: Value -> [(TxIn, TxOut CtxUTxO era)] -> [(TxIn, TxOut CtxUTxO era)]
      select _ [] = []  --
      select required candidates =
        let
          -- Choose the best UTxO from the candidates.
          next = minimumBy (compare `on` priority required) candidates
          -- Determine the remaining candidates.
          candidates' = delete next candidates
          -- Ignore negative quantities.
          filterPositive = valueFromList . filter ((> 0) . snd) . valueToList
          -- Compute the remaining requirement.
          required' = filterPositive $ required <> negateValue (txOutToValue $ snd next)
        in
          -- Decide whether to continue.
          if required' == mempty
            -- The requirements have been met.
            then pure next
            -- The requirements have not been met.
            else next : select required' candidates'
      -- Select the coins.
      selection :: [(TxIn, TxOut CtxUTxO era)]
      selection = select incoming utxos
      -- Compute the native token change, if any.
      change :: Value
      change =
        deleteLovelace
          $ (mconcat $ txOutToValue . snd <$> selection)
          <> negateValue incoming
    -- Compute the change that contains native tokens used for balancing, omitting ones explicitly specified in the outputs.
    output <-
      if change == mempty
        then pure []
        else (: []) <$> ensureMinUtxo protocol (changeAddress, Nothing, change)
      :: m [(AddressInEra era, Maybe Datum, Value)]
    -- Return the coin selection.
    pure
      (
        collateral
      , fst <$> selection
      , outputs <> output
      )
    -- FIXME: There are pathological failures that could happen if there are very many native tokens.
