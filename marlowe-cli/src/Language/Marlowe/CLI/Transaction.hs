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


{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE TypeFamilies       #-}


module Language.Marlowe.CLI.Transaction (
-- * Types
  TxInAlonzo
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
, queryAlonzo
, queryUtxos
, selectUtxos
, submitBody
, querySlotConfig
) where


import Cardano.Api (AddressAny, AddressInEra (..), AlonzoEra, AsType (..), AssetId (..), AssetName (..),
                    BalancedTxBody (..), BuildTx, BuildTxWith (..), CardanoEra (..), CardanoMode,
                    CollateralSupportedInEra (..), ConsensusModeIsMultiEra (..), CtxTx, EraHistory (..), EraInMode (..),
                    ExecutionUnits (..), Hash, KeyWitnessInCtx (..), LocalNodeConnectInfo (..), Lovelace,
                    MultiAssetSupportedInEra (..), PaymentCredential (PaymentCredentialByScript), PaymentKey,
                    PlutusScript, PlutusScriptV1, PlutusScriptVersion (..), PolicyId (..), Quantity (..),
                    QueryInEra (..), QueryInMode (..), QueryInShelleyBasedEra (..), QueryUTxOFilter (..), Script (..),
                    ScriptDataSupportedInEra (..), ScriptDatum (..), ScriptHash, ScriptLanguageInEra (..),
                    ScriptValidity (ScriptInvalid), ScriptWitness (..), ScriptWitnessInCtx (..), ShelleyBasedEra (..),
                    ShelleyWitnessSigningKey (..), SimpleScript (..), SimpleScriptV2, SimpleScriptVersion (..),
                    SlotNo (..), StakeAddressReference (NoStakeAddress), TimeLocksSupported (..), TxAuxScripts (..),
                    TxBody (..), TxBodyContent (..), TxBodyErrorAutoBalance (..), TxBodyScriptData (..),
                    TxCertificates (..), TxExtraKeyWitnesses (..), TxExtraKeyWitnessesSupportedInEra (..), TxFee (..),
                    TxFeesExplicitInEra (..), TxId, TxIn (..), TxInMode (..), TxInsCollateral (..), TxIx (..),
                    TxMetadataInEra (..), TxMetadataJsonSchema (TxMetadataJsonNoSchema),
                    TxMetadataSupportedInEra (TxMetadataInAlonzoEra), TxMintValue (..), TxOut (..), TxOutDatum (..),
                    TxOutValue (..), TxScriptValidity (..),
                    TxScriptValiditySupportedInEra (TxScriptValiditySupportedInAlonzoEra), TxUpdateProposal (..),
                    TxValidityLowerBound (..), TxValidityUpperBound (..), TxWithdrawals (..), UTxO (..),
                    ValidityLowerBoundSupportedInEra (..), ValidityNoUpperBoundSupportedInEra (..),
                    ValidityUpperBoundSupportedInEra (..), Value, WitCtxTxIn, Witness (..), anyAddressInEra,
                    castVerificationKey, getTxId, getVerificationKey, hashScript, hashScriptData, lovelaceToValue,
                    makeShelleyAddressInEra, makeTransactionBodyAutoBalance, metadataFromJson, negateValue,
                    queryNodeLocalState, readFileTextEnvelope, selectAsset, selectLovelace, serialiseToCBOR,
                    serialiseToRawBytesHex, signShelleyTransaction, submitTxToNodeLocal, toAddressAny,
                    txOutValueToValue, valueFromList, valueToList, valueToLovelace, verificationKeyHash,
                    writeFileTextEnvelope)
import Cardano.Api.Shelley (TxBody (ShelleyTxBody), fromPlutusData, protocolParamMaxBlockExUnits,
                            protocolParamMaxTxExUnits, protocolParamMaxTxSize)
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..))
import Cardano.Ledger.Alonzo.TxWitness (Redeemers (..))
import Cardano.Slotting.EpochInfo.API (epochInfoRange, epochInfoSlotToUTCTime, hoistEpochInfo)
import Control.Concurrent (threadDelay)
import Control.Monad (forM_, void, when, (<=<))
import Control.Monad.Except (MonadError, MonadIO, liftIO, runExcept, throwError)
import Data.Fixed (div')
import Data.Maybe (isNothing, maybeToList)
import Data.Ratio ((%))
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Language.Marlowe.CLI.IO (decodeFileBuiltinData, decodeFileStrict, liftCli, liftCliIO, maybeWriteJson,
                                readMaybeMetadata, readSigningKey)
import Language.Marlowe.CLI.Types (CliError (..), OutputQuery (..), PayFromScript (..), PayToScript (..),
                                   SomePaymentSigningKey)
import Ledger.TimeSlot (SlotConfig (..))
import Ouroboros.Consensus.HardFork.History (interpreterToEpochInfo)
import Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult (..))
import Plutus.V1.Ledger.Api (Datum (..), POSIXTime (..), Redeemer (..), TokenName (..), fromBuiltin, toData)
import System.IO (hPutStrLn, stderr)

import qualified Data.Aeson as A (Value (Object))
import qualified Data.ByteString as BS (length)
import qualified Data.ByteString.Char8 as BS8 (unpack)
import qualified Data.HashMap.Strict as H (singleton)
import qualified Data.Map.Strict as M (elems, keysSet, singleton, toList)
import qualified Data.Set as S (empty, fromList, singleton)
import qualified Data.Text as T (pack)


-- | Build a non-Marlowe transaction.
buildSimple :: MonadError CliError m
            => MonadIO m
            => LocalNodeConnectInfo CardanoMode    -- ^ The connection info for the local node.
            -> [FilePath]                          -- ^ The files for required signing keys.
            -> [TxIn]                              -- ^ The transaction inputs.
            -> [(AddressAny, Maybe Datum, Value)]  -- ^ The transaction outputs.
            -> AddressAny                          -- ^ The change address.
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
      buildBody connection
        []
        Nothing
        [] inputs outputs Nothing changeAddress
        Nothing
        []
        TxMintNone
        metadata
        printStats
        invalid
    liftCliIO
      $ writeFileTextEnvelope bodyFile Nothing body
    forM_ timeout
      $ if invalid
          then const $ throwError "Refusing to submit an invalid transaction: collateral would be lost."
          else submitBody connection body signingKeys
    pure
      $ getTxId body


-- | Build a non-Marlowe transaction that cleans an address.
buildClean :: MonadError CliError m
           => MonadIO m
           => LocalNodeConnectInfo CardanoMode  -- ^ The connection info for the local node.
           -> [FilePath]                        -- ^ The files for required signing keys.
           -> Lovelace                          -- ^ The value to be sent to addresses with tokens.
           -> AddressAny                        -- ^ The change address.
           -> Maybe (SlotNo, SlotNo)            -- ^ The valid slot range, if any.
           -> TxMintValue BuildTx AlonzoEra     -- ^ The mint value.
           -> TxMetadataInEra AlonzoEra         -- ^ The metadata.
           -> FilePath                          -- ^ The output file for the transaction body.
           -> Maybe Int                         -- ^ Number of seconds to wait for the transaction to be confirmed, if it is to be confirmed.
           -> m TxId                            -- ^ Action to build the transaction body.
buildClean connection signingKeyFiles lovelace changeAddress range mintValue metadata bodyFile timeout =
  do
    signingKeys <- mapM readSigningKey signingKeyFiles
    utxos <-
      fmap (M.toList . unUTxO)
        .  queryAlonzo connection
        . QueryUTxO
        . QueryUTxOByAddress
        . S.singleton
        $ changeAddress
    let
      minting =
        case mintValue of
          TxMintValue _ minting' _ -> minting'
          _                        -> mempty
      inputs = fst <$> utxos
      extractValue (TxOut _ value _) = txOutValueToValue value
      total = mconcat $ extractValue . snd <$> utxos
      outputs =
        [
          (changeAddress, Nothing, value <> lovelaceToValue lovelace)
        |
          value <- valueFromList . pure <$> valueToList (total <> minting)
        , isNothing $ valueToLovelace value
        ]
    body <-
      buildBody connection
        []
        Nothing
        [] inputs outputs Nothing changeAddress
        range
        []
        mintValue
        metadata
        False
        False
    liftCliIO
      $ writeFileTextEnvelope bodyFile Nothing body
    forM_ timeout
      $ submitBody connection body signingKeys
    pure
      $ getTxId body


-- | Build a non-Marlowe transaction that fills and address from a faucet.
buildFaucet :: MonadError CliError m
            => MonadIO m
            => LocalNodeConnectInfo CardanoMode  -- ^ The connection info for the local node.
            -> Value                             -- ^ The value to be sent to the funded addresses.
            -> AddressAny                        -- ^ The funded address.
            -> AddressAny                        -- ^ The faucet address.
            -> SomePaymentSigningKey             -- ^ The required signing key.
            -> Maybe Int                         -- ^ Number of seconds to wait for the transaction to be confirmed, if it is to be confirmed.
            -> m TxId                            -- ^ Action to build the transaction body.
buildFaucet connection value fundedAddress changeAddress signingKey timeout =
  do
    utxos <-
      fmap (M.toList . unUTxO)
        . queryAlonzo connection
        . QueryUTxO
        . QueryUTxOByAddress
        . S.singleton
        $ changeAddress
    let
      inputs = fst <$> utxos
      extractValue (TxOut _ v _) = txOutValueToValue v
      total = mconcat $ extractValue . snd <$> utxos
      lovelace = lovelaceToValue . toEnum . (`div` 2) . fromEnum $ selectLovelace total
      outputs =
        [
          (fundedAddress, Nothing, value)
        , (changeAddress, Nothing, total <> negateValue value <> negateValue lovelace)
        ]
    body <-
      buildBody connection
        []
        Nothing
        [] inputs outputs Nothing changeAddress
        Nothing
        []
        TxMintNone
        TxMetadataNone
        False
        False
    forM_ timeout
      $ submitBody connection body [signingKey]
    pure
      $ getTxId body


-- | Build a non-Marlowe transaction that fills and address from a faucet.
buildFaucet' :: MonadError CliError m
             => MonadIO m
             => LocalNodeConnectInfo CardanoMode  -- ^ The connection info for the local node.
             -> Value                             -- ^ The value to be sent to the funded addresses.
             -> [AddressAny]                      -- ^ The funded addresses.
             -> FilePath                          -- ^ The output file for the transaction body.
             -> Maybe Int                         -- ^ Number of seconds to wait for the transaction to be confirmed, if it is to be confirmed.
             -> m TxId                            -- ^ Action to build the transaction body.
buildFaucet' connection value addresses bodyFile timeout =
  do
    let
      toAddressAny' :: AddressInEra AlonzoEra -> AddressAny
      toAddressAny' (AddressInEra _ address) = toAddressAny address
      network = localNodeNetworkId connection
      script = RequireAllOf []
      witness =
        BuildTxWith
          . ScriptWitness ScriptWitnessForSpending
          $ SimpleScriptWitness SimpleScriptV2InAlonzo SimpleScriptV2 script
      changeAddress =
        toAddressAny'
          $ makeShelleyAddressInEra
          network
          (PaymentCredentialByScript . hashScript . SimpleScript SimpleScriptV2 $ script)
          NoStakeAddress
    utxos <-
      fmap (M.toList . unUTxO)
        . queryAlonzo connection
        . QueryUTxO
        . QueryUTxOByAddress
        . S.singleton
        $ changeAddress
    let
      inputs = [(txIn, witness) | txIn <- fst <$> utxos]
      extractValue (TxOut _ v _) = txOutValueToValue v
      total = mconcat $ extractValue . snd <$> utxos
      lovelace = lovelaceToValue . toEnum . (`div` 2) . fromEnum $ selectLovelace total
      value' = mconcat $ replicate (length addresses) value
      outputs =
        (changeAddress, Nothing, total <> negateValue value' <> negateValue lovelace)
          : [(fundedAddress, Nothing, value) | fundedAddress <- addresses]
    body <-
      buildBody connection
        []
        Nothing
        inputs [] outputs Nothing changeAddress
        Nothing
        []
        TxMintNone
        TxMetadataNone
        False
        False
    liftCliIO
      $ writeFileTextEnvelope bodyFile Nothing body
    forM_ timeout
      $ submitBody connection body []
    pure
      $ getTxId body


-- | Build a non-Marlowe transaction that mints tokens.
buildMinting :: MonadError CliError m
             => MonadIO m
             => LocalNodeConnectInfo CardanoMode  -- ^ The connection info for the local node.
             -> FilePath                          -- ^ The file for required signing key.
             -> [TokenName]                       -- ^ The token names.
             -> Maybe FilePath                    -- ^ The CIP-25 metadata for the minting, with keys for each token name.
             -> Integer                           -- ^ The number of each token to mint.
             -> Maybe SlotNo                      -- ^ The slot number after which minting is no longer possible.
             -> Lovelace                          -- ^ The value to be sent to addresses with tokens.
             -> AddressAny                        -- ^ The change address.
             -> FilePath                          -- ^ The output file for the transaction body.
             -> Maybe Int                         -- ^ Number of seconds to wait for the transaction to be confirmed, if it is to be confirmed.
             -> m ()                              -- ^ Action to build the transaction body.
buildMinting connection signingKeyFile tokenNames metadataFile count expires lovelace changeAddress bodyFile timeout =
  do
    metadata <- sequence $ decodeFileStrict <$> metadataFile
    signingKey <- readSigningKey signingKeyFile
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
        TxMintValue MultiAssetInAlonzoEra minting
          . BuildTxWith
          . M.singleton policy
          $ SimpleScriptWitness SimpleScriptV2InAlonzo SimpleScriptV2 script
    metadata' <-
      case metadata of
        Just (A.Object metadata'') -> fmap (TxMetadataInEra TxMetadataInAlonzoEra)
                                        . liftCli
                                        . metadataFromJson TxMetadataJsonNoSchema
                                        . A.Object
                                        . H.singleton "721"
                                        . A.Object
                                        . H.singleton (T.pack . BS8.unpack $ serialiseToRawBytesHex policy)
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
              => LocalNodeConnectInfo CardanoMode    -- ^ The connection info for the local node.
              -> AddressAny                          -- ^ The script address.
              -> [FilePath]                          -- ^ The files for required signing keys.
              -> FilePath                            -- ^ The file containing the datum for the payment to the script.
              -> Value                               -- ^ The value to be paid to the script.
              -> [TxIn]                              -- ^ The transaction inputs.
              -> [(AddressAny, Maybe Datum, Value)]  -- ^ The transaction outputs.
              -> AddressAny                          -- ^ The change address.
              -> Maybe FilePath                      -- ^ The file containing JSON metadata, if any.
              -> FilePath                            -- ^ The output file for the transaction body.
              -> Maybe Int                           -- ^ Number of seconds to wait for the transaction to be confirmed, if it is to be confirmed.
              -> Bool                                -- ^ Whether to print statistics about the transaction.
              -> Bool                                -- ^ Assertion that the transaction is invalid.
              -> m TxId                              -- ^ Action to build the transaction body.
buildIncoming connection scriptAddress signingKeyFiles outputDatumFile outputValue inputs outputs changeAddress metadataFile bodyFile timeout printStats invalid =
  do
    metadata <- readMaybeMetadata metadataFile
    scriptAddress' <- asAlonzoAddress "Failed to converting script address to Alonzo era." scriptAddress
    outputDatum <- Datum <$> decodeFileBuiltinData outputDatumFile
    signingKeys <- mapM readSigningKey signingKeyFiles
    body <-
      buildBody connection
        []
        (Just $ buildPayToScript scriptAddress' outputValue outputDatum)
        [] inputs outputs Nothing changeAddress
        Nothing
        []
        TxMintNone
        metadata
        printStats
        invalid
    liftCliIO
      $ writeFileTextEnvelope bodyFile Nothing body
    forM_ timeout
      $ if invalid
          then const $ throwError "Refusing to submit an invalid transaction: collateral would be lost."
          else submitBody connection body signingKeys
    pure
      $ getTxId body


-- | Build a transaction that spends from and pays to a Marlowe contract.
buildContinuing :: MonadError CliError m
                => MonadIO m
                => LocalNodeConnectInfo CardanoMode    -- ^ The connection info for the local node.
                -> AddressAny                          -- ^ The script address.
                -> FilePath                            -- ^ The file containing the script validator.
                -> FilePath                            -- ^ The file containing the redeemer.
                -> FilePath                            -- ^ The file containing the datum for spending from the script.
                -> [FilePath]                          -- ^ The files for required signing keys.
                -> TxIn                                -- ^ The script eUTxO to be spent.
                -> FilePath                            -- ^ The file containing the datum for the payment to the script.
                -> Value                               -- ^ The value to be paid to the script.
                -> [TxIn]                              -- ^ The transaction inputs.
                -> [(AddressAny, Maybe Datum, Value)]  -- ^ The transaction outputs.
                -> TxIn                                -- ^ The collateral.
                -> AddressAny                          -- ^ The change address.
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
    scriptAddress' <- asAlonzoAddress "Failed to converting script address to Alonzo era." scriptAddress
    validator <- liftCliIO (readFileTextEnvelope (AsPlutusScript AsPlutusScriptV1) validatorFile)
    redeemer <- Redeemer <$> decodeFileBuiltinData redeemerFile
    inputDatum <- Datum <$> decodeFileBuiltinData inputDatumFile
    outputDatum <- Datum <$> decodeFileBuiltinData outputDatumFile
    signingKeys <- mapM readSigningKey signingKeyFiles
    body <-
      buildBody connection
        [buildPayFromScript validator inputDatum redeemer txIn]
        (Just $ buildPayToScript scriptAddress' outputValue outputDatum)
        [] inputs outputs (Just collateral) changeAddress
        (Just (minimumSlot, maximumSlot))
        (hashSigningKey <$> signingKeys)
        TxMintNone
        metadata
        printStats
        invalid
    liftCliIO
      $ writeFileTextEnvelope bodyFile Nothing body
    forM_ timeout
      $ if invalid
          then const $ throwError "Refusing to submit an invalid transaction: collateral would be lost."
          else submitBody connection body signingKeys
    pure
      $ getTxId body


-- | Build a transaction spending from a Marlowe contract.
buildOutgoing :: MonadError CliError m
              => MonadIO m
              => LocalNodeConnectInfo CardanoMode    -- ^ The connection info for the local node.
              -> FilePath                            -- ^ The file containing the script validator.
              -> FilePath                            -- ^ The file containing the redeemer.
              -> FilePath                            -- ^ The file containing the datum for spending from the script.
              -> [FilePath]                          -- ^ The files for required signing keys.
              -> TxIn                                -- ^ The script eUTxO to be spent.
              -> [TxIn]                              -- ^ The transaction inputs.
              -> [(AddressAny, Maybe Datum, Value)]  -- ^ The transaction outputs.
              -> TxIn                                -- ^ The collateral.
              -> AddressAny                          -- ^ The change address.
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
      buildBody connection
        [buildPayFromScript validator inputDatum redeemer txIn]
        Nothing
        [] inputs outputs (Just collateral) changeAddress
        (Just (minimumSlot, maximumSlot))
        (hashSigningKey <$> signingKeys)
        TxMintNone
        metadata
        printStats
        invalid
    liftCliIO
      $ writeFileTextEnvelope bodyFile Nothing body
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
          => LocalNodeConnectInfo CardanoMode    -- ^ The connection info for the local node.
          -> [PayFromScript]                     -- ^ Payment information from the script, if any.
          -> Maybe (PayToScript AlonzoEra)       -- ^ Payment information to the script, if any.
          -> [TxInAlonzo]                        -- ^ Transaction inputs.
          -> [TxIn]                              -- ^ Transaction inputs.
          -> [(AddressAny, Maybe Datum, Value)]  -- ^ Transaction outputs.
          -> Maybe TxIn                          -- ^ Collateral, if any.
          -> AddressAny                          -- ^ The change address.
          -> Maybe (SlotNo, SlotNo)              -- ^ The valid slot range, if any.
          -> [Hash PaymentKey]                   -- ^ The extra required signatures.
          -> TxMintValue BuildTx AlonzoEra       -- ^ The mint value.
          -> TxMetadataInEra AlonzoEra           -- ^ The metadata.
          -> Bool                                -- ^ Whether to print statistics about the transaction.
          -> Bool                                -- ^ Assertion that the transaction is invalid.
          -> m (TxBody AlonzoEra)                -- ^ The action to build the transaction body.
buildBody connection payFromScript payToScript extraInputs inputs outputs collateral changeAddress slotRange extraSigners mintValue metadata printStats invalid =
  do
    changeAddress' <- asAlonzoAddress "Failed converting change address to Alonzo era." changeAddress
    start <- queryAny connection   QuerySystemStart
    history <- queryAny connection $ QueryEraHistory CardanoModeIsMultiEra
    protocol <- queryAlonzo connection QueryProtocolParameters
    let
      protocol' = (\pp -> pp {protocolParamMaxTxExUnits = protocolParamMaxBlockExUnits pp}) protocol
      txInsCollateral   = TxInsCollateral CollateralInAlonzoEra $ maybeToList collateral
      txFee             = TxFeeExplicit TxFeesExplicitInAlonzoEra 0
      txValidityRange   = (
                            maybe
                              TxValidityNoLowerBound
                              (TxValidityLowerBound ValidityLowerBoundInAlonzoEra . fst)
                              slotRange
                          , maybe
                              (TxValidityNoUpperBound ValidityNoUpperBoundInAlonzoEra)
                              (TxValidityUpperBound ValidityUpperBoundInAlonzoEra . snd)
                              slotRange
                          )
      txMetadata        = metadata
      txAuxScripts      = TxAuxScriptsNone
      txExtraKeyWits    = TxExtraKeyWitnesses ExtraKeyWitnessesInAlonzoEra extraSigners
      txProtocolParams  = BuildTxWith $ Just protocol'
      txWithdrawals     = TxWithdrawalsNone
      txCertificates    = TxCertificatesNone
      txUpdateProposal  = TxUpdateProposalNone
      txMintValue       = mintValue
      txScriptValidity  = if invalid
                            then TxScriptValidity TxScriptValiditySupportedInAlonzoEra ScriptInvalid
                            else TxScriptValidityNone
      scriptTxIn = redeemScript <$> payFromScript
      txIns = extraInputs <> scriptTxIn <> fmap makeTxIn inputs
      scriptTxOut = maybe [] payScript payToScript
    txOuts <- (scriptTxOut <>) <$> mapM (uncurry3 makeTxOut) outputs
    utxo <-
      queryAlonzo connection
        . QueryUTxO
        . QueryUTxOByTxIn
        . S.fromList
        $ fst
        <$> txIns
    -- Compute the change.
    BalancedTxBody _ change _ <-
      liftCli
        $ makeTransactionBodyAutoBalance
            AlonzoEraInCardanoMode
            start
            history
            protocol'
            S.empty
            utxo
            TxBodyContent{..}
            changeAddress'
            Nothing
    let
      -- Recompute execution units with full set of UTxOs, including change.
      trial =
        makeTransactionBodyAutoBalance
          AlonzoEraInCardanoMode
          start
          history
          protocol'
          S.empty
          utxo
          (TxBodyContent{..} {txOuts = change : txOuts})
          changeAddress'
          Nothing
      -- Correct for a negative balance in cases where execution units, and hence fees, have increased.
      change' =
        case (change, trial) of
          (TxOut _ (TxOutValue _ value) _, Left (TxBodyErrorAdaBalanceNegative delta)) ->
            TxOut changeAddress' (TxOutValue MultiAssetInAlonzoEra $ value <> lovelaceToValue delta) TxOutDatumNone
          _ -> change
    -- Construct the body with correct execution units and fees.
    BalancedTxBody txBody _ lovelace <-
      liftCli
        $ makeTransactionBodyAutoBalance
            AlonzoEraInCardanoMode
            start
            history
            protocol'
            S.empty
            utxo
            (TxBodyContent{..} {txOuts = change' : txOuts})
            changeAddress'
            Nothing
    when printStats
      . liftIO
      $ do
        hPutStrLn stderr ""
        hPutStrLn stderr $ "Fee: " <> show lovelace
        let
          size = BS.length $ serialiseToCBOR txBody
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
findExUnits :: TxBody AlonzoEra  -- ^ The transaction body.
            -> ExUnits           -- ^ The execution units.
findExUnits (ShelleyTxBody ShelleyBasedEraAlonzo  _ _ (TxBodyScriptData _ _ (Redeemers redeemers)) _ _) =
  mconcat . fmap snd . M.elems $ redeemers
findExUnits _ = mempty


-- | Sign and submit a transaction.
submit :: MonadError CliError m
       => MonadIO m
       => LocalNodeConnectInfo CardanoMode  -- ^ The connection info for the local node.
       -> FilePath                          -- ^ The transaction body file.
       -> [FilePath]                        -- ^ The signing key files.
       -> Int                               -- ^ Number of seconds to wait for the transaction to be confirmed.
       -> m TxId                            -- ^ The action to submit the transaction.
submit connection bodyFile signingKeyFiles timeout =
  do
    body <- liftCliIO $ readFileTextEnvelope (AsTxBody AsAlonzoEra) bodyFile
    signings <- mapM readSigningKey signingKeyFiles
    submitBody connection body signings timeout


-- | Sign and submit a transaction.
submitBody :: MonadError CliError m
           => MonadIO m
           => LocalNodeConnectInfo CardanoMode  -- ^ The connection info for the local node.
           -> TxBody AlonzoEra                  -- ^ The transaction body.
           -> [SomePaymentSigningKey]           -- ^ The signing keys.
           -> Int                               -- ^ Number of seconds to wait for the transaction to be confirmed.
           -> m TxId                            -- ^ The action to submit the transaction.
submitBody connection body signings timeout =
  do
    let
      tx =
        signShelleyTransaction body
          $ either WitnessPaymentKey WitnessPaymentExtendedKey
          <$> signings
    result <-
      liftIO
        . submitTxToNodeLocal connection
        $ TxInMode tx AlonzoEraInCardanoMode
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
               queryAlonzo connection
                 . QueryUTxO
                 . QueryUTxOByTxIn
                 $ txIns'
             if M.keysSet (unUTxO utxos) == txIns'
               then pure ()
               else go (n - 1 :: Int)
  in
    go . ceiling $ fromIntegral timeout / (fromIntegral pause :: Double)


-- | TxIn for Alonzo transaction body.
type TxInAlonzo = (TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn AlonzoEra))


-- | Compute the transaction input for paying from a script.
redeemScript :: PayFromScript  -- ^ The payment information.
             -> TxInAlonzo     -- ^ The transaction input.
redeemScript PayFromScript{..} =
  (
    txIn
  , BuildTxWith
      . ScriptWitness ScriptWitnessForSpending
      $ PlutusScriptWitness
        PlutusScriptV1InAlonzo
        PlutusScriptV1
        script
        (ScriptDatumForTxIn . fromPlutusData $ toData datum)
        (fromPlutusData $ toData redeemer)
        (ExecutionUnits 0 0)
  )


-- | Compute the transaction output for paying to a script.
payScript :: PayToScript AlonzoEra  -- ^ The payment information.
          -> [TxOut CtxTx AlonzoEra]      -- ^ The transaction input.
payScript PayToScript{..} =
  [
    TxOut
      address
      (TxOutValue MultiAssetInAlonzoEra value)
      (TxOutDatum ScriptDataInAlonzoEra datumOut)
  ]


-- | Compute transaction input for building a transaction.
makeTxIn :: TxIn                                                        -- ^ The transaction input.
         -> (TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn AlonzoEra))  -- ^ The building for the transaction input.
makeTxIn = (, BuildTxWith $ KeyWitness KeyWitnessForSpending)


-- | Uncurry a triplet.
uncurry3 :: (a1 -> a2 -> a3 -> b)
         -> (a1, a2, a3)
         -> b
uncurry3 f (x, y, z) = f x y z


-- | Compute transaction output for building a transaction.
makeTxOut :: MonadError CliError m
          => AddressAny                 -- ^ The output address.
          -> Maybe Datum                -- ^ The datum, if any.
          -> Value                      -- ^ The output value.
          -> m (TxOut CtxTx AlonzoEra)  -- ^ Action for building the transaction output.
makeTxOut address datum value =
  do
    address' <- asAlonzoAddress "Failed converting output address to Alonzo era." address
    pure
      $ TxOut
        address'
        (TxOutValue MultiAssetInAlonzoEra value)
        (maybe TxOutDatumNone (TxOutDatum ScriptDataInAlonzoEra . fromPlutusData . toData) datum)


-- | Convert an address to Alonzo era.
asAlonzoAddress :: MonadError CliError m
                => String                     -- ^ The error message.
                -> AddressAny                 -- ^ The address.
                -> m (AddressInEra AlonzoEra) -- ^ Action for converting the address.
asAlonzoAddress message =
  liftCli
    . maybe (Left message) Right
    . anyAddressInEra AlonzoEra


-- | Query a node.
queryAny :: MonadError CliError m
         => MonadIO m
         => LocalNodeConnectInfo CardanoMode -- ^ The connection info for the local node.
         -> QueryInMode CardanoMode a        -- ^ The query.
         -> m a                              -- ^ Action for running the query.
queryAny connection =
 liftCliIO
   . queryNodeLocalState connection Nothing


-- | Query an Alonzo-era node.
queryAlonzo :: MonadError CliError m
            => MonadIO m
            => LocalNodeConnectInfo CardanoMode   -- ^ The connection info for the local node.
            -> QueryInShelleyBasedEra AlonzoEra a -- ^ The query.
            -> m a                                -- ^ Action for running the query.
queryAlonzo connection =
  liftCli
    <=< (
          liftCliIO
          . queryNodeLocalState connection Nothing
          . QueryInEra AlonzoEraInCardanoMode
          . QueryInShelleyBasedEra ShelleyBasedEraAlonzo
        )


-- | Find the UTxOs at an address.
queryUtxos :: MonadError CliError m
           => MonadIO m
           => LocalNodeConnectInfo CardanoMode  -- ^ The connection info for the local node.
           -> AddressAny                        -- ^ The address.
           -> m (UTxO AlonzoEra)                -- ^ Action query the UTxOs.
queryUtxos connection =
  queryAlonzo connection
    . QueryUTxO
    . QueryUTxOByAddress
    . S.singleton


-- | Select a UTxOs at an address.
selectUtxos :: MonadError CliError m
            => MonadIO m
            => LocalNodeConnectInfo CardanoMode  -- ^ The connection info for the local node.
            -> AddressAny                        -- ^ The address.
            -> OutputQuery                       -- ^ Filter for the results.
            -> m ()                              -- ^ Action query the UTxOs.
selectUtxos connection address query =
  do
    UTxO candidates <- queryUtxos connection address
    let
      query' (_, TxOut _ value' _) =
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
                => LocalNodeConnectInfo CardanoMode  -- ^ The connection info for the local node.
                -> m SlotConfig                      -- ^ Action to extract the slot configuration.
querySlotConfig connection =
  do
    epochNo <- queryAlonzo connection QueryEpoch
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
              => LocalNodeConnectInfo CardanoMode  -- ^ The connection info for the local node.
              -> Maybe FilePath                    -- ^ The output file for the slot configuration.
              -> m ()                              -- ^ Action to extract the slot configuration.
querySlotting connection outputFile =
  querySlotConfig connection
    >>= maybeWriteJson outputFile
