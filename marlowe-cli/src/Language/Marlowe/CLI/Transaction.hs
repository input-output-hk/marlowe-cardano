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
{-# LANGUAGE ViewPatterns #-}


module Language.Marlowe.CLI.Transaction
  ( -- * Types
    TxInEra
    -- * Building
  , buildClean
  , buildContinuing
  , buildFaucet
  , buildFaucet'
  , buildFaucetImpl
  , buildIncoming
  , buildMinting
  , buildMintingImpl
  , buildOutgoing
  , buildPublishing
  , buildPublishingImpl
  , buildSimple
  , publishImpl
  , querySlotting
    -- * Submitting
  , submit
    -- * Quering
  , findMarloweScriptsRefs
  , findPublished
    -- * Low-Level Functions
  , buildBody
  , buildBodyWithContent
  , buildPayFromScript
  , buildPayToScript
  , hashSigningKey
  , makeBalancedTxOut
  , makeTxOut
  , makeTxOut'
  , querySlotConfig
  , queryUtxos
  , selectUtxos
  , selectUtxosImpl
  , submitBody
  , submitBody'
    -- * Balancing
  , ensureMinUtxo
  , findMinUtxo
  , selectCoins
  ) where


import Cardano.Api
  ( AddressInEra(..)
  , AsType(..)
  , AssetId(..)
  , AssetName(..)
  , BalancedTxBody(..)
  , BuildTx
  , BuildTxWith(..)
  , CardanoMode
  , ConsensusModeIsMultiEra(..)
  , CtxTx
  , CtxUTxO
  , EraHistory(..)
  , ExecutionUnits(..)
  , Hash
  , KeyWitnessInCtx(..)
  , LocalNodeConnectInfo(..)
  , Lovelace
  , PaymentCredential(PaymentCredentialByScript)
  , PaymentKey
  , PlutusScript
  , PlutusScriptVersion(..)
  , PolicyId(..)
  , Quantity(..)
  , QueryInMode(..)
  , QueryInShelleyBasedEra(..)
  , QueryUTxOFilter(..)
  , Script(..)
  , ScriptDataSupportedInEra
  , ScriptDatum(..)
  , ScriptHash
  , ScriptValidity(ScriptInvalid)
  , ScriptWitness(..)
  , ScriptWitnessInCtx(..)
  , ShelleyBasedEra(..)
  , ShelleyWitnessSigningKey(..)
  , SimpleScript(..)
  , SimpleScriptV2
  , SimpleScriptVersion(..)
  , SlotNo(..)
  , StakeAddressReference(NoStakeAddress)
  , TimeLocksSupported(..)
  , TxAuxScripts(..)
  , TxBody(..)
  , TxBodyContent(..)
  , TxBodyErrorAutoBalance(..)
  , TxBodyScriptData(..)
  , TxCertificates(..)
  , TxExtraKeyWitnesses(..)
  , TxFee(..)
  , TxId
  , TxIn(..)
  , TxInMode(..)
  , TxInsCollateral(..)
  , TxInsReference(TxInsReferenceNone)
  , TxIx(..)
  , TxMetadataInEra(..)
  , TxMetadataJsonSchema(TxMetadataJsonNoSchema)
  , TxMintValue(..)
  , TxOut(..)
  , TxOutDatum(..)
  , TxOutValue(..)
  , TxReturnCollateral(TxReturnCollateralNone)
  , TxScriptValidity(..)
  , TxTotalCollateral(TxTotalCollateralNone)
  , TxUpdateProposal(..)
  , TxValidityLowerBound(..)
  , TxValidityUpperBound(..)
  , TxWithdrawals(..)
  , UTxO(..)
  , Value
  , WitCtxTxIn
  , Witness(..)
  , calculateMinimumUTxO
  , castVerificationKey
  , getTxId
  , getVerificationKey
  , hashScript
  , hashScriptData
  , lovelaceToValue
  , makeShelleyAddressInEra
  , makeTransactionBodyAutoBalance
  , metadataFromJson
  , negateValue
  , queryNodeLocalState
  , readFileTextEnvelope
  , selectAsset
  , selectLovelace
  , serialiseToCBOR
  , serialiseToRawBytesHex
  , shelleyBasedEra
  , signShelleyTransaction
  , submitTxToNodeLocal
  , toScriptInAnyLang
  , txOutValueToValue
  , valueFromList
  , valueToList
  , valueToLovelace
  , verificationKeyHash
  , writeFileTextEnvelope
  )
import qualified Cardano.Api as C
import Cardano.Api.Shelley
  ( ExecutionUnitPrices(..)
  , ProtocolParameters(..)
  , ReferenceScript(ReferenceScript, ReferenceScriptNone)
  , SimpleScriptOrReferenceInput(SScript)
  , TxBody(ShelleyTxBody)
  , fromPlutusData
  , protocolParamMaxBlockExUnits
  , protocolParamMaxTxExUnits
  , protocolParamMaxTxSize
  )
import qualified Cardano.Api.Shelley as C
import Cardano.Ledger.Alonzo.Scripts (ExUnits(..))
import Cardano.Ledger.Alonzo.TxWitness (Redeemers(..))
import Cardano.Slotting.EpochInfo.API (epochInfoRange, epochInfoSlotToUTCTime, hoistEpochInfo)
import Control.Arrow ((***))
import Control.Concurrent (threadDelay)
import Control.Error (MaybeT(MaybeT, runMaybeT), hoistMaybe, note)
import Control.Monad (forM, forM_, unless, void, when)
import Control.Monad.Except (MonadError(catchError), MonadIO, liftEither, liftIO, runExcept, throwError)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (lift)
import Data.Aeson (ToJSON(toJSON))
import qualified Data.Aeson as A (Value(Null, Object), object)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson.Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS (length)
import qualified Data.ByteString.Char8 as BS8 (unpack)
import Data.Fixed (div')
import Data.Foldable (Foldable(fold), for_)
import Data.Function (on)
import Data.Functor (($>), (<&>))
import Data.List (delete, minimumBy, partition)
import Data.List.Extra (notNull)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Append as AM
import qualified Data.Map.Strict as M (elems, fromList, keysSet, lookup, singleton, toList)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, isNothing, maybeToList)
import Data.Ratio ((%))
import qualified Data.Set as S (empty, fromList, singleton)
import qualified Data.Text as T
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Units (Second, TimeUnit(toMicroseconds))
import Data.Traversable (for)
import Data.Tuple.Extra (uncurry3)
import GHC.Natural (Natural)
import Language.Marlowe.CLI.Cardano.Api
  (adjustMinimumUTxO, toPlutusProtocolVersion, toReferenceTxInsScriptsInlineDatumsSupportedInEra, txOutValueValue)
import qualified Language.Marlowe.CLI.Cardano.Api as MCA
import Language.Marlowe.CLI.Cardano.Api.Address.ProofOfBurn (permanentPublisher)
import Language.Marlowe.CLI.Cardano.Api.PlutusScript as PS
import Language.Marlowe.CLI.Data.Foldable (tillFirstMatch)
import Language.Marlowe.CLI.Export (buildValidatorInfo)
import Language.Marlowe.CLI.IO
  ( decodeFileBuiltinData
  , decodeFileStrict
  , getDefaultCostModel
  , liftCli
  , liftCliIO
  , liftCliMaybe
  , maybeWriteJson
  , queryInEra
  , readMaybeMetadata
  , readSigningKey
  )
import Language.Marlowe.CLI.Types
  ( AnUTxO(AnUTxO)
  , CliEnv
  , CliError(..)
  , CoinSelectionStrategy(CoinSelectionStrategy, csPreserveInlineDatums, csPreserveReferenceScripts, csPreserveTxIns)
  , CurrencyIssuer(CurrencyIssuer)
  , MarlowePlutusVersion
  , MarloweScriptsRefs(MarloweScriptsRefs)
  , MintingAction(BurnAll, Mint, maIssuer)
  , OutputQuery(..)
  , OutputQueryResult(..)
  , PayFromScript(..)
  , PayToScript(..)
  , PrintStats(PrintStats)
  , PublishMarloweScripts(PublishMarloweScripts)
  , PublishScript(..)
  , PublishingStrategy(..)
  , SigningKeyFile
  , SomePaymentSigningKey
  , SubmitMode(DoSubmit, DontSubmit)
  , TxBodyFile(TxBodyFile)
  , ValidatorInfo(ValidatorInfo, viHash, viScript)
  , askEra
  , asksEra
  , defaultCoinSelectionStrategy
  , doWithCardanoEra
  , marlowePlutusVersion
  , submitModeFromTimeout
  , toAddressAny'
  , toAsType
  , toCollateralSupportedInEra
  , toEraInMode
  , toExtraKeyWitnessesSupportedInEra
  , toMultiAssetSupportedInEra
  , toSimpleScriptV2LanguageInEra
  , toTxFeesExplicitInEra
  , toTxMetadataSupportedInEra
  , toTxScriptValiditySupportedInEra
  , toValidityLowerBoundSupportedInEra
  , toValidityNoUpperBoundSupportedInEra
  , toValidityUpperBoundSupportedInEra
  , validatorInfo'
  , withCardanoEra
  , withShelleyBasedEra
  )
import qualified Language.Marlowe.CLI.Types as PayToScript (PayToScript(value))
import Language.Marlowe.Scripts (marloweValidator, rolePayoutValidator)
import Ouroboros.Consensus.HardFork.History (interpreterToEpochInfo)
import Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult(..))
import Plutus.V1.Ledger.Api (Datum(..), POSIXTime(..), Redeemer(..), TokenName(..), fromBuiltin, toData)
import Plutus.V1.Ledger.SlotConfig (SlotConfig(..))
import System.IO (hPrint, hPutStrLn, stderr)


-- | Build a non-Marlowe transaction.
buildSimple :: forall era m
             . MonadError CliError m
            => MonadIO m
            => MonadReader (CliEnv era) m
            => LocalNodeConnectInfo CardanoMode          -- ^ The connection info for the local node.
            -> [SigningKeyFile]                          -- ^ The files for required signing keys.
            -> [TxIn]                                    -- ^ The transaction inputs.
            -> [(AddressInEra era, Maybe Datum, Value)]  -- ^ The transaction outputs.
            -> AddressInEra era                          -- ^ The change address.
            -> Maybe FilePath                            -- ^ The file containing JSON metadata, if any.
            -> TxBodyFile                                -- ^ The output file for the transaction body.
            -> Maybe Second                              -- ^ Number of seconds to wait for the transaction to be confirmed, if it is to be confirmed.
            -> Bool                                      -- ^ Whether to print statistics about the transaction.
            -> Bool                                      -- ^ Assertion that the transaction is invalid.
            -> m TxId                                    -- ^ Action to build the transaction body.
buildSimple connection signingKeyFiles inputs outputs changeAddress metadataFile (TxBodyFile bodyFile) timeout printStats invalid =
  do
    metadata <- readMaybeMetadata metadataFile
    signingKeys <- mapM readSigningKey signingKeyFiles
    outputs' <- mapM (uncurry3 makeTxOut') outputs
    body <-
      buildBody
        connection
        ([] :: [PayFromScript C.PlutusScriptV1])
        Nothing
        []
        inputs outputs' Nothing changeAddress
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
           -> [SigningKeyFile]                  -- ^ The files for required signing keys.
           -> Lovelace                          -- ^ The value to be sent to addresses with tokens.
           -> AddressInEra era                  -- ^ The change address.
           -> Maybe (SlotNo, SlotNo)            -- ^ The valid slot range, if any.
           -> TxMintValue BuildTx era           -- ^ The mint value.
           -> TxMetadataInEra era               -- ^ The metadata.
           -> TxBodyFile                        -- ^ The output file for the transaction body.
           -> Maybe Second                      -- ^ Number of seconds to wait for the transaction to be confirmed, if it is to be confirmed.
           -> m TxId                            -- ^ Action to build the transaction body.
buildClean connection signingKeyFiles lovelace changeAddress range mintValue metadata (TxBodyFile bodyFile) timeout =
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
    outputs <- sequence
        [
          makeTxOut changeAddress Nothing (value <> lovelaceToValue lovelace) ReferenceScriptNone
        |
          value <- valueFromList . pure <$> valueToList (total <> minting)
        , isNothing $ valueToLovelace value
        ]

    body <-
      buildBody
        connection
        ([] :: [PayFromScript C.PlutusScriptV1])
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
            -> Maybe Value                       -- ^ The value to be sent to the funded addresses. By default we drain the source.
            -> [AddressInEra era]                -- ^ The addresses to receive funds.
            -> AddressInEra era                  -- ^ The faucet address.
            -> SigningKeyFile                    -- ^ The required signing key.
            -> Maybe Second                      -- ^ Number of seconds to wait for the transaction to be confirmed, if it is to be confirmed.
            -> m TxId                            -- ^ Action to build the transaction body.
buildFaucet connection possibleValue destAddresses fundAddress fundSigningKeyFile timeout =
  do
    let
      singleton x = x : mempty
    fundSigningKey <- readSigningKey fundSigningKeyFile
    body <-
      buildFaucetImpl
        connection
        (singleton <$> possibleValue)
        destAddresses
        fundAddress
        fundSigningKey
        defaultCoinSelectionStrategy
        (submitModeFromTimeout timeout)
    pure
      $ getTxId body

-- | Build a non-Marlowe transaction that fills and address from a faucet.
buildFaucetImpl :: MonadError CliError m
            => MonadIO m
            => MonadReader (CliEnv era) m
            => LocalNodeConnectInfo CardanoMode   -- ^ The connection info for the local node.
            -> Maybe [Value]                      -- ^ The list of values to be sent to the funded addresses as separate outputs.
            -> [AddressInEra era]                 -- ^ The addresses to receive funds.
            -> AddressInEra era                   -- ^ The faucet address.
            -> SomePaymentSigningKey              -- ^ The required signing key.
            -> CoinSelectionStrategy
            -> SubmitMode                         -- ^ A possible number of seconds to wait for the transaction to be confirmed, if it is to be confirmed.
            -> m (TxBody era)                                -- ^ Action to build the transaction body.
buildFaucetImpl connection possibleValues destAddresses fundAddress fundSigningKey coinSelectionStrategy submitMode =
  do
    (inputs, outputs', changeAddress) <- case (possibleValues, destAddresses) of
      (Nothing, [destAddress]) -> do
        C.UTxO (Map.toList -> utxos) <- queryUtxos connection fundAddress
        let
          total = foldMap (txOutValueValue . snd) utxos
          nonAda = total <> negateValue (C.lovelaceToValue . C.selectLovelace $ total)

        outputs <- if nonAda /= mempty
          then do
            era <- askEra
            protocol <- queryInEra connection QueryProtocolParameters
            txOut <- makeBalancedTxOut era protocol destAddress Nothing nonAda ReferenceScriptNone
            pure [txOut]
          else
            pure []
        pure (map fst utxos, outputs, destAddress)
      (Nothing, _) -> do
        throwError "Total value transfer is only supported to a single destination."
      (Just values, _) -> do
        outputs <- sequence $
          [makeTxOut destAddress Nothing value ReferenceScriptNone | value <- values, destAddress <- destAddresses]

        (_, i, o) <- selectCoins
                                   connection
                                   mempty
                                   outputs
                                   Nothing
                                   fundAddress
                                   coinSelectionStrategy
        pure (i, o, fundAddress)

    body <-
      buildBody
        connection
        ([] :: [PayFromScript C.PlutusScriptV1])
        Nothing
        [] inputs outputs'
        Nothing changeAddress
        Nothing
        []
        TxMintNone
        TxMetadataNone
        False
        False
    case submitMode of
      DoSubmit timeout -> void $ submitBody connection body [fundSigningKey] timeout
      DontSubmit -> pure ()
    pure body

-- | Build a non-Marlowe transaction that fills and address from a faucet.
buildFaucet' :: MonadError CliError m
             => MonadIO m
             => MonadReader (CliEnv era) m
             => LocalNodeConnectInfo CardanoMode  -- ^ The connection info for the local node.
             -> Value                             -- ^ The value to be sent to the funded addresses.
             -> [AddressInEra era]                -- ^ The funded addresses.
             -> TxBodyFile                        -- ^ The output file for the transaction body.
             -> Maybe Second                      -- ^ Number of seconds to wait for the transaction to be confirmed, if it is to be confirmed.
             -> m TxId                            -- ^ Action to build the transaction body.
buildFaucet' connection value addresses (TxBodyFile bodyFile) timeout =
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
    outputs <- mapM (uncurry3 makeTxOut') $
        (changeAddress, Nothing, total <> negateValue value' <> negateValue lovelace)
        : [(fundedAddress, Nothing, value) | fundedAddress <- addresses]
    body <-
      buildBody
        connection
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
    withCardanoEra era $ liftCliIO $ writeFileTextEnvelope bodyFile Nothing body
    forM_ timeout
      $ submitBody connection body []
    pure
      $ getTxId body

-- | Build and submit a non-Marlowe transaction that mints tokens
-- | using files as a data exchange medium.
buildMinting :: MonadError CliError m
             => MonadIO m
             => MonadReader (CliEnv era) m
             => LocalNodeConnectInfo CardanoMode                    -- ^ The connection info for the local node.
             -> SigningKeyFile                                      -- ^ The file for required signing key.
             -> Either
                [(AddressInEra era, SigningKeyFile)]
                (NonEmpty (TokenName, Natural, AddressInEra era))
                                                                    -- ^ Minting policy related action.
                                                                    -- Pass token providers for burning or token distribution for minting.
             -> Maybe FilePath                                      -- ^ The CIP-25 metadata for the minting, with keys for each token name.
             -> Maybe SlotNo                                        -- ^ The slot number after which minting is no longer possible.
             -> AddressInEra era                                    -- ^ The change address.
             -> TxBodyFile                                          -- ^ The output file for the transaction body.
             -> Maybe Second                                        -- ^ Number of seconds to wait for the transaction to be confirmed, if it is to be confirmed.
             -> m ()                                                -- ^ Action to build the transaction body.
buildMinting connection signingKeyFile mintingAction metadataFile expires changeAddress (TxBodyFile bodyFile) timeout = do
  signingKey <- readSigningKey signingKeyFile
  let
    currencyIssuer = CurrencyIssuer changeAddress signingKey
  mintingAction' <- case mintingAction of
    Left (provider:providers) -> do
      let
        loadWallet (addr, skeyFile) = do
          skey <- readSigningKey skeyFile
          pure (addr, skey)
      provider' <- loadWallet provider
      providers' <- for providers loadWallet
      pure $ BurnAll currencyIssuer (provider' :| providers')
    Left _ -> do
      throwError "Token provider set is empty."
    Right tokenDistribution -> do
      pure $ Mint currencyIssuer $ tokenDistribution <&> \(name, amount, addr) -> (name, amount, addr, Nothing)
  metadataJson <- sequence $ decodeFileStrict <$> metadataFile
  metadata <- forM metadataJson \case
    A.Object metadataProps -> pure metadataProps
    _                      -> throwError "Metadata should file should contain a json object"
  let submitMode = submitModeFromTimeout timeout
  (body, policy) <- buildMintingImpl connection mintingAction' metadata expires submitMode (PrintStats True)
  doWithCardanoEra $ liftCliIO $ writeFileTextEnvelope bodyFile Nothing body
  liftIO . putStrLn $ "PolicyID " <> show policy

nonAdaValue :: Value -> Value
nonAdaValue value = value <> C.negateValue (C.lovelaceToValue (fromMaybe 0 $ C.valueToLovelace value))


-- | Build and submit a non-Marlowe transaction that mints tokens.
buildMintingImpl :: MonadError CliError m
             => MonadIO m
             => MonadReader (CliEnv era) m
             => LocalNodeConnectInfo CardanoMode                  -- ^ The connection info for the local node.
             -> MintingAction era                                 -- ^ The token names, amount and a possible receipient addresses.
             -> Maybe Aeson.Object                                -- ^ The CIP-25 metadata for the minting, with keys for each token name.
             -> Maybe SlotNo                                      -- ^ The slot number after which minting is no longer possible.
             -> SubmitMode                                         -- ^ Number of seconds to wait for the transaction to be confirmed, if it is to be confirmed.
             -> PrintStats
             -> m (TxBody era, PolicyId)                          -- ^ Action to build the transaction body.
buildMintingImpl connection mintingAction metadataProps expires submitMode (PrintStats printStats) =
  do
    era <- askEra
    protocol <- queryInEra connection QueryProtocolParameters
    let
      CurrencyIssuer changeAddress signingKey = maIssuer mintingAction
      verification =
        verificationKeyHash
          $ case signingKey of
              Left  k -> getVerificationKey k
              Right k -> castVerificationKey $ getVerificationKey k
      (script, scriptHash) = mintingScript verification expires

      policy = PolicyId scriptHash

    (inputs, outputs, signingKeys, mint) <- case mintingAction of
        Mint _ tokenDistribution -> do
            let
              tokenDistribution' = tokenDistribution <&> \(TokenName name, count, recipient, minAda) -> do
                let
                  value = valueFromList . pure $
                      (AssetId policy (AssetName $ fromBuiltin name) , C.Quantity $ toInteger count)
                (recipient, value, minAda)

            -- TODO: use sensible coin selection here. Currently coin selection fails in the context of minting.
            utxos <-
              fmap (M.toList . unUTxO)
                . queryInEra connection
                . QueryUTxO
                . QueryUTxOByAddress
                . S.singleton
                $ toAddressAny' changeAddress

            assetsOutputs <- fmap catMaybes $ for utxos \(_, TxOut addr txOutValue _ _) -> do
              let
                value = C.txOutValueToValue txOutValue
                assetsValue = nonAdaValue value
              if assetsValue /= mempty
                then Just <$> makeBalancedTxOut era protocol addr Nothing assetsValue ReferenceScriptNone
                else pure Nothing

            outputs' <- fmap NonEmpty.toList $ for tokenDistribution' \(address, mintedValue, minAda) -> case minAda of
              Just minAda' -> do
                let
                  adaValue = C.lovelaceToValue minAda'
                  value = adaValue <> mintedValue
                makeTxOut' address Nothing value
              Nothing ->
                makeBalancedTxOut era protocol address Nothing mintedValue ReferenceScriptNone

            pure (map fst utxos, assetsOutputs <> outputs', [signingKey], foldMap (\(_, m, _) -> m) tokenDistribution')

        BurnAll _ providers -> do
            let
              signingKeys' = signingKey : (NonEmpty.toList . fmap snd $ providers)
              addresses = NonEmpty.toList . fmap fst $ providers

            allUtxos <-
              fmap (M.toList . unUTxO)
                . queryInEra connection
                . QueryUTxO
                . QueryUTxOByAddress
                . S.fromList
                . fmap toAddressAny'
                $ addresses

            (inputs', tokensValueList, changes) <- fmap (unzip3 . catMaybes) $ for allUtxos \(txIn, TxOut addr txOutValue _ refScript) -> do
              let
                value = C.txOutValueToValue txOutValue
                toPolicyId C.AdaAssetId = Nothing
                toPolicyId (AssetId p _) = Just p
                tokensValue = C.valueFromList . filter (\(assetId, _) -> toPolicyId assetId == Just policy) . C.valueToList $ value
              if (tokensValue /= mempty || addr == changeAddress) && refScript == C.ReferenceScriptNone
                  then do
                    let
                      changeValue = value <> negateValue tokensValue
                      -- Accumulate change value per token provider address.
                      -- `AddressInEra` has no `Ord` instance so we fallback to the
                      -- `AdressAny` which complicates a bit the final `outputs` build up.
                      change = AM.AppendMap $ M.singleton (toAddressAny' addr)
                        -- We want to keep all the ADA of the submitter and postpone
                        -- this particular change calculation to the final
                        -- transaction balancing in the `buildBody` function.
                        if addr == changeAddress
                          then nonAdaValue changeValue
                          else changeValue
                    pure $ Just (txIn, tokensValue, change)
                  else
                    pure Nothing
            let
              tokensValue = fold tokensValueList
              changesMap = AM.unAppendMap $ fold changes

            outputs' <- fmap catMaybes $ for addresses \addr -> runMaybeT do
              value <- hoistMaybe (toAddressAny' addr `M.lookup` changesMap)
              MaybeT $ if value /= mempty
                then Just <$> makeBalancedTxOut era protocol addr Nothing value ReferenceScriptNone
                else pure Nothing
            when (tokensValue == mempty) $ do
              throwError . CliError $ "Unable to find currency " <> show policy <> " tokens."
            pure (inputs', outputs', signingKeys', C.negateValue tokensValue)

    let
      minting = TxMintValue (toMultiAssetSupportedInEra era) mint
        . BuildTxWith
        . M.singleton policy
        $ SimpleScriptWitness (toSimpleScriptV2LanguageInEra era) SimpleScriptV2 (SScript script)

    metadata' <-
      case metadataProps of
        Just metadataProps' -> fmap (TxMetadataInEra (toTxMetadataSupportedInEra era))
                              . liftCli
                              . metadataFromJson TxMetadataJsonNoSchema
                              . A.Object
                              . KeyMap.singleton "721"
                              . A.Object
                              . KeyMap.singleton (Aeson.Key.fromString . BS8.unpack $ serialiseToRawBytesHex policy)
                              $ A.Object metadataProps'
        _               -> pure TxMetadataNone


    (bodyContent, body) <-
      buildBodyWithContent
        connection
        ([] :: [PayFromScript C.PlutusScriptV1])
        Nothing
        [] inputs outputs Nothing changeAddress
        ((0, ) <$> expires)
        []
        minting
        metadata'
        printStats
        False

    body' <- case submitMode of
      DontSubmit -> pure body
      DoSubmit t -> do
        -- We attempt to increase fees by arbitrary amount on submission failure.
        submitBody' connection body bodyContent changeAddress signingKeys t $> body
    pure (body', policy)


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
              => LocalNodeConnectInfo CardanoMode          -- ^ The connection info for the local node.
              -> AddressInEra era                          -- ^ The script address.
              -> [SigningKeyFile]                          -- ^ The files for required signing keys.
              -> FilePath                                  -- ^ The file containing the datum for the payment to the script.
              -> Value                                     -- ^ The value to be paid to the script.
              -> [TxIn]                                    -- ^ The transaction inputs.
              -> [(AddressInEra era, Maybe Datum, Value)]  -- ^ The transaction outputs.
              -> AddressInEra era                          -- ^ The change address.
              -> Maybe FilePath                            -- ^ The file containing JSON metadata, if any.
              -> TxBodyFile                                -- ^ The output file for the transaction body.
              -> Maybe Second                              -- ^ Number of seconds to wait for the transaction to be confirmed, if it is to be confirmed.
              -> Bool                                      -- ^ Whether to print statistics about the transaction.
              -> Bool                                      -- ^ Assertion that the transaction is invalid.
              -> m TxId                                    -- ^ Action to build the transaction body.
buildIncoming connection scriptAddress signingKeyFiles outputDatumFile outputValue inputs outputs changeAddress metadataFile (TxBodyFile bodyFile) timeout printStats invalid =
  do
    metadata <- readMaybeMetadata metadataFile
    outputDatum <- Datum <$> decodeFileBuiltinData outputDatumFile
    signingKeys <- mapM readSigningKey signingKeyFiles
    outputs' <- mapM (uncurry3 makeTxOut') outputs
    body <-
      buildBody
        connection
        ([] :: [PayFromScript C.PlutusScriptV1])
        (Just $ buildPayToScript scriptAddress outputValue outputDatum)
        [] inputs outputs' Nothing changeAddress
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


buildReferenceScript :: forall era lang m. IsPlutusScriptLanguage lang => MonadError CliError m => MonadReader (CliEnv era) m => PlutusScript lang -> m (ReferenceScript era)
buildReferenceScript plutusScript = do
  era <- askEra
  referenceTxInsScriptsInlineDatumsSupportedInEra
    <- liftEither $ note "Reference scripts not supported in era" $ MCA.toReferenceTxInsScriptsInlineDatumsSupportedInEra era
  let
    refScript = ReferenceScript referenceTxInsScriptsInlineDatumsSupportedInEra
      . toScriptInAnyLang
      . PS.toScript
      $ plutusScript
  pure refScript


publisherAddress :: C.IsShelleyBasedEra era => ScriptHash -> PublishingStrategy era -> ScriptDataSupportedInEra era -> C.NetworkId -> AddressInEra era
publisherAddress scriptHash publishingStrategy era network = case publishingStrategy of
  PublishAtAddress addr -> addr
  PublishPermanently stake -> do
    let
      paymentCredentials = permanentPublisher scriptHash
    withShelleyBasedEra era $ makeShelleyAddressInEra network paymentCredentials stake


buildPublishScript :: forall era lang m
                    . MonadIO m
                    => MonadError CliError m
                    => C.IsShelleyBasedEra era
                    => MonadReader (CliEnv era) m
                    => IsPlutusScriptLanguage lang
                    => LocalNodeConnectInfo CardanoMode
                    -> PlutusScript lang
                    -> PublishingStrategy era
                    -> m (PublishScript lang era)
buildPublishScript connection plutusScript publishingStrategy = do
  era <- askEra
  protocol <- queryInEra connection QueryProtocolParameters
  costModel <- getDefaultCostModel
  let
    protocolVersion = toPlutusProtocolVersion $ protocolParamProtocolVersion protocol
    networkId = localNodeNetworkId connection
    scriptHash = hashScript . PS.toScript $ plutusScript
    publisher = publisherAddress scriptHash publishingStrategy era networkId

  -- Stake information in this context is probably meaningless. We assing real staking when we use a reference.
  referenceScriptInfo <- validatorInfo' plutusScript Nothing era protocolVersion costModel networkId NoStakeAddress
  referenceScript <- buildReferenceScript plutusScript

  (minAda, _) <- liftCli $ adjustMinimumUTxO era protocol publisher Nothing mempty referenceScript
  pure $ PublishScript minAda publisher referenceScriptInfo


buildPublishingImpl :: forall era m
                    . MonadError CliError m
                    => MonadIO m
                    => MonadReader (CliEnv era) m
                    => C.IsShelleyBasedEra era
                    => LocalNodeConnectInfo CardanoMode  -- ^ The connection info for the local node.
                    -> SomePaymentSigningKey             -- ^ The file for required signing key.
                    -> Maybe SlotNo                      -- ^ The slot number after which publishing is no longer possible.
                    -> AddressInEra era                  -- ^ The change address.
                    -> PublishingStrategy era
                    -> CoinSelectionStrategy
                    -> PrintStats
                    -> m (TxBody era, PublishMarloweScripts MarlowePlutusVersion era)
buildPublishingImpl connection signingKey expires changeAddress publishingStrategy coinSelectionStrategy (PrintStats printStats) = do
  pm <- buildPublishScript connection (fromV2TypedValidator marloweValidator) publishingStrategy
  pp <- buildPublishScript connection (fromV2TypedValidator rolePayoutValidator) publishingStrategy

  let
    buildPublishScriptTxOut PublishScript { psMinAda, psPublisher, psReferenceValidator } = do
      referenceScript <- buildReferenceScript $ viScript psReferenceValidator
      makeTxOut psPublisher Nothing (lovelaceToValue psMinAda) referenceScript

  outputs <- sequence
    [ buildPublishScriptTxOut pm
    , buildPublishScriptTxOut pp
    ]


  (_, inputs, outputs') <- selectCoins
                             connection
                             mempty
                             outputs
                             Nothing
                             changeAddress
                             coinSelectionStrategy

  txBody <- buildBody
    connection
    ([] :: [PayFromScript MarlowePlutusVersion])
    Nothing
    [] inputs outputs' Nothing changeAddress
    ((0, ) <$> expires)
    [hashSigningKey signingKey]
    TxMintNone
    TxMetadataNone
    printStats
    False

  let
    serialiseAddress = T.unpack . C.serialiseAddress

  when printStats $ liftIO do
    hPutStrLn stderr ""
    hPutStrLn stderr $
      "Marlowe script published at address: " <> serialiseAddress (psPublisher pm)
    hPutStrLn stderr ""
    hPutStrLn stderr $
      "Marlowe script hash: "
      <> show (viHash (psReferenceValidator pm))
    hPutStrLn stderr ""
    hPutStrLn stderr $
      "Marlowe ref script UTxO min ADA: " <> show (psMinAda pm)
    hPutStrLn stderr ""
    hPutStrLn stderr $
      "Payout script published at address: " <> serialiseAddress (psPublisher pp)
    hPutStrLn stderr ""
    hPutStrLn stderr $
      "Payout script hash: "
      <> show (viHash (psReferenceValidator pp))
    hPutStrLn stderr ""
    hPutStrLn stderr $
      "Payout ref script UTxO min ADA: " <> show (psMinAda pp)

  pure (txBody, PublishMarloweScripts pm pp)


-- CLI command handler.
buildPublishing :: forall era m
        . MonadError CliError m
        => MonadIO m
        => MonadReader (CliEnv era) m
        => C.IsShelleyBasedEra era
        => LocalNodeConnectInfo CardanoMode  -- ^ The connection info for the local node.
        -> SigningKeyFile                    -- ^ The file for required signing key.
        -> Maybe SlotNo                      -- ^ The slot number after which publishing is no longer possible.
        -> AddressInEra era                  -- ^ The change address.
        -> Maybe (PublishingStrategy era)
        -> TxBodyFile
        -> Maybe Second
        -> PrintStats
        -> m ()
buildPublishing connection signingKeyFile expires changeAddress strategy (TxBodyFile bodyFile) timeout printStats = do
  let
    strategy' = fromMaybe (PublishAtAddress changeAddress) strategy
  signingKey <- readSigningKey signingKeyFile
  (txBody, _) <- buildPublishingImpl
    connection
    signingKey
    expires
    changeAddress
    strategy'
    defaultCoinSelectionStrategy
    printStats

  doWithCardanoEra $ liftCliIO $ writeFileTextEnvelope bodyFile Nothing txBody
  for_ timeout \timeout' ->
    void $ submitBody connection txBody [signingKey] timeout'


publishImpl :: forall era m
             . MonadError CliError m
            => MonadIO m
            => MonadReader (CliEnv era) m
            => C.IsShelleyBasedEra era
            => LocalNodeConnectInfo CardanoMode  -- ^ The connection info for the local node.
            -> SomePaymentSigningKey             -- ^ The file for required signing key.
            -> Maybe SlotNo                      -- ^ The slot number after which publishing is no longer possible.
            -> AddressInEra era                  -- ^ The change address.
            -> PublishingStrategy era
            -> CoinSelectionStrategy
            -> Second
            -> PrintStats
            -> m (MarloweScriptsRefs MarlowePlutusVersion era)
publishImpl connection signingKey expires changeAddress publishingStrategy coinSelectionStrategy timeout printStats = do
  (txBody, _)  <- buildPublishingImpl
    connection
    signingKey
    expires
    changeAddress
    publishingStrategy
    coinSelectionStrategy
    printStats

  void $ submitBody connection txBody [signingKey] timeout
  findMarloweScriptsRefs connection publishingStrategy printStats >>= \case
    Nothing -> throwError . CliError $ "Unable to find just published scripts by tx:" <> show (getTxId txBody)
    Just m  -> pure m


findScriptRef
  :: MonadReader (CliEnv era) m
  => IsPlutusScriptLanguage lang
  => C.IsShelleyBasedEra era
  => MonadIO m
  => MonadError CliError m
  => LocalNodeConnectInfo CardanoMode
  -> ScriptHash
  -> PublishingStrategy era
  -> PlutusScriptVersion lang
  -> PrintStats
  -> m (Maybe (AnUTxO era, ValidatorInfo lang era))
findScriptRef connection scriptHash publishingStrategy plutusVersion (PrintStats printStats) = do
  era <- askEra
  let
    network = localNodeNetworkId connection
    publisher = publisherAddress scriptHash publishingStrategy era network

  when printStats $ liftIO do
    hPutStrLn stderr ""
    hPutStrLn stderr $
      "Searching for reference script at address: " <> (T.unpack . C.serialiseAddress $ publisher)
    hPutStrLn stderr ""
    hPutStrLn stderr $
      "Expected reference script hash: "
      <> show scriptHash

  runMaybeT do
    (u@(AnUTxO (txIn, _)), script) <- MaybeT $ selectUtxosImpl connection publisher (FindReferenceScript plutusVersion scriptHash)
    i <- lift $ buildValidatorInfo connection script (Just txIn) NoStakeAddress
    pure (u, i)


findMarloweScriptsRefs
  :: MonadReader (CliEnv era) m
  => MonadIO m
  => MonadError CliError m
  => C.IsShelleyBasedEra era
  => LocalNodeConnectInfo CardanoMode
  -> PublishingStrategy era
  -> PrintStats
  -> m (Maybe (MarloweScriptsRefs MarlowePlutusVersion era))
findMarloweScriptsRefs connection publishingStrategy printStats = do
  let
    marloweHash = hashScript (PS.toScript $ fromV2TypedValidator marloweValidator)
    payoutHash = hashScript (PS.toScript $ fromV2TypedValidator rolePayoutValidator)

  runMaybeT do
    m <- MaybeT $ findScriptRef connection marloweHash publishingStrategy marlowePlutusVersion printStats
    p <- MaybeT $ findScriptRef connection payoutHash publishingStrategy marlowePlutusVersion printStats
    pure $ MarloweScriptsRefs m p


-- | CLI Command handler.
findPublished :: (C.IsShelleyBasedEra era, MonadReader (CliEnv era) m, MonadIO m, MonadError CliError m) => LocalNodeConnectInfo CardanoMode -> Maybe (PublishingStrategy era) -> m ()
findPublished connection publishingStrategy = do
  let
    publishingStrategy' = fromMaybe (PublishPermanently NoStakeAddress) publishingStrategy

  findMarloweScriptsRefs connection publishingStrategy' (PrintStats True) >>= \case
    Just (MarloweScriptsRefs (mu, mi) (ru, ri)) -> do
      let
        refJSON (AnUTxO (i, _)) ValidatorInfo { viHash } = A.object
          [ ("txIn", toJSON i)
          , ("hash", toJSON viHash)
          ]

      maybeWriteJson Nothing $ A.object
        [ ("marlowe", refJSON mu mi)
        , ("payout", refJSON ru ri)
        ]
    Nothing -> maybeWriteJson Nothing A.Null


-- | TODO: Add support for constant validator.
-- | Build a transaction that spends from and pays to a Marlowe contract.
buildContinuing :: MonadError CliError m
                => MonadIO m
                => MonadReader (CliEnv era) m
                => LocalNodeConnectInfo CardanoMode    -- ^ The connection info for the local node.
                -> AddressInEra era                          -- ^ The script address.
                -> FilePath                            -- ^ The file containing the script validator.
                -> FilePath                            -- ^ The file containing the redeemer.
                -> FilePath                            -- ^ The file containing the datum for spending from the script.
                -> [SigningKeyFile]                    -- ^ The files for required signing keys.
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
                -> TxBodyFile                          -- ^ The output file for the transaction body.
                -> Maybe Second                        -- ^ Number of seconds to wait for the transaction to be confirmed, if it is to be confirmed.
                -> Bool                                -- ^ Whether to print statistics about the transaction.
                -> Bool                                -- ^ Assertion that the transaction is invalid.
                -> m TxId                              -- ^ Action to build the transaction body.
buildContinuing connection scriptAddress validatorFile redeemerFile inputDatumFile signingKeyFiles txIn outputDatumFile outputValue inputs outputs collateral changeAddress minimumSlot maximumSlot metadataFile (TxBodyFile bodyFile) timeout printStats invalid =
  do
    metadata <- readMaybeMetadata metadataFile
    validator <- liftCliIO (readFileTextEnvelope (AsPlutusScript AsPlutusScriptV2) validatorFile)
    redeemer <- Redeemer <$> decodeFileBuiltinData redeemerFile
    inputDatum <- Datum <$> decodeFileBuiltinData inputDatumFile
    outputDatum <- Datum <$> decodeFileBuiltinData outputDatumFile
    signingKeys <- mapM readSigningKey signingKeyFiles
    outputs' <- mapM (uncurry3 makeTxOut') outputs
    body <-
      buildBody
        connection
        [buildPayFromScript (C.PScript validator) inputDatum redeemer txIn]
        (Just $ buildPayToScript scriptAddress outputValue outputDatum)
        [] inputs outputs' (Just collateral) changeAddress
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


-- | TODO: Add support for constant validator.
-- | Build a transaction spending from a Marlowe contract.
buildOutgoing :: MonadError CliError m
              => MonadIO m
              => MonadReader (CliEnv era) m
              => LocalNodeConnectInfo CardanoMode    -- ^ The connection info for the local node.
              -> FilePath                            -- ^ The file containing the script validator.
              -> FilePath                            -- ^ The file containing the redeemer.
              -> FilePath                            -- ^ The file containing the datum for spending from the script.
              -> [SigningKeyFile]                    -- ^ The files for required signing keys.
              -> TxIn                                -- ^ The script eUTxO to be spent.
              -> [TxIn]                              -- ^ The transaction inputs.
              -> [(AddressInEra era, Maybe Datum, Value)]  -- ^ The transaction outputs.
              -> TxIn                                -- ^ The collateral.
              -> AddressInEra era                    -- ^ The change address.
              -> SlotNo                              -- ^ The first valid slot for the transaction.
              -> SlotNo                              -- ^ The last valid slot for the transaction.
              -> Maybe FilePath                      -- ^ The file containing JSON metadata, if any.
              -> TxBodyFile                          -- ^ The output file for the transaction body.
              -> Maybe Second                        -- ^ Number of seconds to wait for the transaction to be confirmed, if it is to be confirmed.
              -> Bool                                -- ^ Whether to print statistics about the transaction.
              -> Bool                                -- ^ Assertion that the transaction is invalid.
              -> m TxId                              -- ^ Action to build the transaction body.
buildOutgoing connection validatorFile redeemerFile inputDatumFile signingKeyFiles txIn inputs outputs collateral changeAddress minimumSlot maximumSlot metadataFile (TxBodyFile bodyFile) timeout printStats invalid =
  do
    metadata <- readMaybeMetadata metadataFile
    validator <- liftCliIO (readFileTextEnvelope (AsPlutusScript AsPlutusScriptV2) validatorFile)
    redeemer <- Redeemer <$> decodeFileBuiltinData redeemerFile
    inputDatum <- Datum <$> decodeFileBuiltinData inputDatumFile
    signingKeys <- mapM readSigningKey signingKeyFiles
    outputs' <- mapM (uncurry3 makeTxOut') outputs
    body <-
      buildBody
        connection
        [buildPayFromScript (C.PScript validator) inputDatum redeemer txIn]
        Nothing
        [] inputs outputs' (Just collateral) changeAddress
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
buildPayFromScript :: C.PlutusScriptOrReferenceInput lang       -- ^ The script.
                   -> Datum                                     -- ^ The datum.
                   -> Redeemer                                  -- ^ The redeemer.
                   -> TxIn                                      -- ^ The eUTxO to be spent.
                   -> PayFromScript lang                        -- ^ Payment information.
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
buildBody :: forall era lang m
           . MonadError CliError m
          => IsPlutusScriptLanguage lang
          => MonadIO m
          => MonadReader (CliEnv era) m
          => LocalNodeConnectInfo CardanoMode    -- ^ The connection info for the local node.
          -> [PayFromScript lang]                -- ^ Payment information from the script, if any.
          -> Maybe (PayToScript era)             -- ^ Payment information to the script, if any.
          -> [TxInEra era]                       -- ^ Transaction inputs with witnesses.
          -> [TxIn]                              -- ^ Transaction inputs.
          -> [TxOut CtxTx era]                   -- ^ Action for building the transaction output.
          -> Maybe TxIn                          -- ^ Collateral, if any.
          -> AddressInEra era                    -- ^ The change address.
          -> Maybe (SlotNo, SlotNo)              -- ^ The valid slot range, if any.
          -> [Hash PaymentKey]                   -- ^ The extra required signatures.
          -> TxMintValue BuildTx era             -- ^ The mint value.
          -> TxMetadataInEra era                 -- ^ The metadata.
          -> Bool                                -- ^ Whether to print statistics about the transaction.
          -> Bool                                -- ^ Assertion that the transaction is invalid.
          -> m (TxBody era)                      -- ^ The action to build the transaction body.
buildBody connection payFromScript payToScript extraInputs inputs outputs collateral changeAddress slotRange extraSigners mintValue metadata printStats invalid =
  snd <$> buildBodyWithContent connection payFromScript payToScript extraInputs inputs outputs collateral changeAddress slotRange extraSigners mintValue metadata printStats invalid

-- We need the `TxContext` when we want to resubmit a failing transaction with adjusted fees.
buildBodyWithContent :: forall era lang m
                      . MonadError CliError m
                     => IsPlutusScriptLanguage lang
                     => MonadIO m
                     => MonadReader (CliEnv era) m
                     => LocalNodeConnectInfo CardanoMode    -- ^ The connection info for the local node.
                     -> [PayFromScript lang]                -- ^ Payment information from the script, if any.
                     -> Maybe (PayToScript era)             -- ^ Payment information to the script, if any.
                     -> [TxInEra era]                       -- ^ Transaction inputs with witnesses.
                     -> [TxIn]                              -- ^ Transaction inputs.
                     -> [TxOut CtxTx era]                   -- ^ Action for building the transaction output.
                     -> Maybe TxIn                          -- ^ Collateral, if any.
                     -> AddressInEra era                    -- ^ The change address.
                     -> Maybe (SlotNo, SlotNo)              -- ^ The valid slot range, if any.
                     -> [Hash PaymentKey]                   -- ^ The extra required signatures.
                     -> TxMintValue BuildTx era             -- ^ The mint value.
                     -> TxMetadataInEra era                 -- ^ The metadata.
                     -> Bool                                -- ^ Whether to print statistics about the transaction.
                     -> Bool                                -- ^ Assertion that the transaction is invalid.
                     -> m (TxBodyContent C.BuildTx era, TxBody era)                   -- ^ The action to build the transaction body together with context.
buildBodyWithContent connection payFromScript payToScript extraInputs inputs outputs collateral changeAddress slotRange extraSigners mintValue metadata printStats invalid =
  do
    start <- queryAny connection   QuerySystemStart
    history <- queryAny connection $ QueryEraHistory CardanoModeIsMultiEra
    protocol <- queryInEra connection QueryProtocolParameters
    era <- askEra
    (scriptTxIn, txInsReferences) <- unzip <$> for payFromScript \s -> liftCli do
      redeemScript era s
    let
      txInsReference = do
        let
          step (C.TxInsReference _ refs) (C.TxInsReference w refs') = C.TxInsReference w (refs <> refs')
          step C.TxInsReferenceNone acc                             = acc
          step ref _                                                = ref
        foldr step C.TxInsReferenceNone txInsReferences

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
      txMintValue        = mintValue
      txScriptValidity   = if invalid
                             then TxScriptValidity (toTxScriptValiditySupportedInEra era) ScriptInvalid
                             else TxScriptValidityNone
      txIns = extraInputs <> scriptTxIn <> fmap makeTxIn inputs
      scriptTxOut = maybe [] (payScript era) payToScript
      txOuts = scriptTxOut <> outputs

    let
      txInsReferencesToTxIns C.TxInsReferenceNone      = []
      txInsReferencesToTxIns (C.TxInsReference _ refs) = refs

      refTxIns = txInsReferencesToTxIns txInsReference

      allTxIns = (fst <$> txIns) <> refTxIns

    -- This UTxO set is used for change calcuation.
    utxos <-
      queryInEra connection
        . QueryUTxO
        . QueryUTxOByTxIn
        . S.fromList
        $ allTxIns
    let eraInMode = toEraInMode era

    let
      foundTxIns = map fst . M.toList . unUTxO $ utxos
      missingTxIns = [ txIn | txIn <- allTxIns, txIn `notElem` foundTxIns ]
    when (notNull missingTxIns) do
      throwError . CliError $ "Some inputs are missing from the chain (possibly reference inputs): " <> show missingTxIns

    let
      mkChangeTxOut value = do
        let txOutValue = C.TxOutValue (toMultiAssetSupportedInEra era) value
        C.TxOut changeAddress txOutValue TxOutDatumNone ReferenceScriptNone

      balancingLoop :: Integer -> C.Value -> m (C.TxBodyContent C.BuildTx era, BalancedTxBody era)
      balancingLoop counter changeValue = do -- changeTxOut@(TxOut addr (txOutValueToValue -> changeValue) datum ref) = do
        when (counter == 0) $ throwError . CliError $ do
          "Unsuccessful balancing of the transaction: " <> show (TxBodyContent {..})
        let
          -- Recompute execution units with full set of UTxOs, including change.
          buildTxBodyContent = TxBodyContent{..} { txOuts = mkChangeTxOut changeValue : txOuts }
          trial =
            withShelleyBasedEra era $ makeTransactionBodyAutoBalance
              eraInMode
              start
              history
              protocol'
              S.empty
              utxos
              buildTxBodyContent
              changeAddress
              Nothing
        case trial of
          -- Correct for a negative balance in cases where execution units, and hence fees, have increased.
          Left (TxBodyErrorAdaBalanceNegative delta) -> do
            balancingLoop (counter - 1) (C.lovelaceToValue delta <> changeValue)
          Left err -> throwError . CliError $ show err
          Right balanced@(BalancedTxBody (TxBody TxBodyContent { txFee = fee }) _ _) -> do
            pure (buildTxBodyContent { txFee = fee }, balanced)

      totalIn = foldMap txOutValueValue . (M.elems . C.unUTxO) $ utxos
      totalOut = foldMap txOutValueValue txOuts
      totalMint = case txMintValue of
        C.TxMintValue _ value _ -> value
        _ -> mempty
      -- Initial setup is `fee = 0` - we output all the difference as a change and expect balancing error ;-)
      initialChange = totalIn <> totalMint <> C.negateValue totalOut

    (txBodyContent, BalancedTxBody txBody _ lovelace) <- balancingLoop 10 initialChange

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

    return (txBodyContent, txBody)


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
       -> TxBodyFile                        -- ^ The transaction body file.
       -> [SigningKeyFile]                  -- ^ The signing key files.
       -> Second                            -- ^ Number of seconds to wait for the transaction to be confirmed.
       -> m TxId                            -- ^ The action to submit the transaction.
submit connection (TxBodyFile bodyFile) signingKeyFiles timeout =
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
           -> TxBody era                        -- ^ The transaction body.
           -> [SomePaymentSigningKey]           -- ^ The signing keys.
           -> Second                            -- ^ Number of seconds to wait for the transaction to be confirmed.
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
      SubmitFail reason -> do
        liftIO $ hPutStrLn stderr "Submission of the transaction failed:"
        liftIO $ hPrint stderr body
        throwError . CliError $ show reason

-- A vesrion of submit which performs an attempt to extra fee balancing on failure
-- (we experienced failures on the cardano-node for already balanced transactions).
submitBody' :: MonadError CliError m
           => MonadIO m
           => MonadReader (CliEnv era) m
           => LocalNodeConnectInfo CardanoMode  -- ^ The connection info for the local node.
           -> TxBody era                        -- ^ The transaction body.
           -> C.TxBodyContent C.BuildTx era
           -> AddressInEra era                  -- ^ The change address.
           -> [SomePaymentSigningKey]           -- ^ The signing keys.
           -> Second                            -- ^ Number of seconds to wait for the transaction to be confirmed.
           -> m (TxId, TxBody era)              -- ^ The action to submit the transaction.
submitBody' connection body bodyContent changeAddress signingKeys timeout = do
  era <- askEra
  ((, body) <$> submitBody connection body signingKeys timeout) `catchError` \err -> do
    liftIO $ hPutStrLn stderr "Adjusting the fees and resubmitting failing transaction."
    liftIO $ hPrint stderr err
    let
      feeBalancingMargin = C.Lovelace 10000
      C.TxBodyContent{..} = bodyContent
      -- Find change UTxO and subtract the fee margin.
      step (TxOut addr value datum refScript) (False, outs)
        | addr == changeAddress && (fromMaybe 0 . C.valueToLovelace $ C.txOutValueToValue value) > feeBalancingMargin = do
        let
          value' = txOutValueToValue value <> C.negateValue (C.lovelaceToValue feeBalancingMargin)
          out' = TxOut addr (TxOutValue (toMultiAssetSupportedInEra era) value') datum refScript
        (True, out' : outs)
      step out (flag, outs) = (flag, out : outs)

      (adjusted, txOuts') = foldr step (False, []) txOuts
    -- Increase the transaction fee by the chosen margin.
    txFee' <- case (adjusted, txFee) of
      (True, TxFeeExplicit feesInEra value) -> pure $ TxFeeExplicit feesInEra (value + feeBalancingMargin)
      _ -> throwError . CliError $ "Unable to adjust the change during resubmission attempt."

    withCardanoEra era $ case C.makeTransactionBody (C.TxBodyContent{..} { txOuts = txOuts', txFee = txFee' }) of
      Left err' -> throwError . CliError $ "Failure during reconstruction of the failing tx body: " <> show err'
      Right body' -> do
        txId <- submitBody connection body' signingKeys timeout
        pure (txId, body')


-- | Wait for transactions to be confirmed as UTxOs.
waitForUtxos :: MonadError CliError m
             => MonadIO m
             => MonadReader (CliEnv era) m
             => LocalNodeConnectInfo CardanoMode  -- ^ The connection info for the local node.
             -> Second                            -- ^ Number of seconds to wait for the transaction to be confirmed.
             -> [TxIn]                            -- ^ The transactions to wait for.
             -> m ()                              -- ^ Action to wait for the transaction confirmations.
waitForUtxos connection timeout txIns =
  let
    pause = 5
    timeout' = fromInteger $ toMicroseconds timeout
    txIns' = S.fromList txIns
    go 0 = throwError "Timeout waiting for transaction to be confirmed."
    go n = do
             liftIO . threadDelay $ pause
             utxos <-
               queryInEra connection
                 . QueryUTxO
                 . QueryUTxOByTxIn
                 $ txIns'
             if M.keysSet (unUTxO utxos) == txIns'
               then pure ()
               else go (n - 1 :: Int)
  in
    go . ceiling $ timeout' / (fromIntegral pause :: Double)


-- | TxIn for transaction body.
type TxInEra era = (TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn era))


scriptWitness :: forall era lang m
               . MonadError CliError m
              => IsPlutusScriptLanguage lang
              => ScriptDataSupportedInEra era
              -> PayFromScript lang                            -- ^ The payment information.
              -> m (C.BuildTxWith  C.BuildTx (Witness WitCtxTxIn era))
scriptWitness era PayFromScript{..} = do
  scriptInEra <- liftCliMaybe "Script language not supported in era" $ toScriptLanguageInEra era
  pure $ BuildTxWith . ScriptWitness ScriptWitnessForSpending $ C.PlutusScriptWitness
    scriptInEra
    (plutusScriptVersion @lang)
    script
    (ScriptDatumForTxIn . fromPlutusData $ toData datum)
    (fromPlutusData $ toData redeemer)
    (ExecutionUnits 0 0)
  -- pure $ BuildTxWith $ KeyWitness KeyWitnessForSpending


-- | Compute the transaction input for paying from a script.
redeemScript :: forall era lang m
              . MonadError CliError m
             => IsPlutusScriptLanguage lang
             => ScriptDataSupportedInEra era
             -> PayFromScript lang                            -- ^ The payment information.
             -> m (TxInEra era, TxInsReference BuildTx era)   -- ^ The transaction input.
redeemScript era p@PayFromScript{..} = do
  witness <- scriptWitness era p
  w <- liftCliMaybe "Reference scripts not supported in era" $ toReferenceTxInsScriptsInlineDatumsSupportedInEra era
  let
    refTxIn = case script of
      C.PScript {}           -> TxInsReferenceNone
      C.PReferenceScript r _ -> C.TxInsReference w [r]
  pure ((txIn,  witness), refTxIn)


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
         -> (TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn era))        -- ^ The building for the transaction input.
makeTxIn = (, BuildTxWith $ KeyWitness KeyWitnessForSpending)


-- | Compute transaction output for building a transaction.
makeTxOut :: MonadReader (CliEnv era) m
          => AddressInEra era                 -- ^ The output address.
          -> Maybe Datum                -- ^ The datum, if any.
          -> Value                      -- ^ The output value.
          -> ReferenceScript era
          -> m (TxOut CtxTx era) -- ^ Action for building the transaction output.
makeTxOut address datum value referenceScript = asksEra \era -> TxOut
  address
  (TxOutValue (toMultiAssetSupportedInEra era) value)
  (maybe TxOutDatumNone (TxOutDatumInTx era . fromPlutusData . toData) datum)
  referenceScript


-- | Compute transaction output for building a transaction.
makeTxOut' :: MonadReader (CliEnv era) m
           => AddressInEra era                 -- ^ The output address.
           -> Maybe Datum                -- ^ The datum, if any.
           -> Value                      -- ^ The output value.
           -> m (TxOut CtxTx era) -- ^ Action for building the transaction output.
makeTxOut' address datum value = makeTxOut address datum value ReferenceScriptNone

makeBalancedTxOut :: MonadError CliError m
                  => MonadReader (CliEnv era) m
                  => ScriptDataSupportedInEra era
                  -> ProtocolParameters
                  -> AddressInEra era
                  -> Maybe Datum
                  -> Value
                  -> ReferenceScript era
                  -> m (TxOut CtxTx era)
makeBalancedTxOut era protocol address datum value referenceScript = do
  (_, value') <- liftCli $ adjustMinimumUTxO era protocol address datum value referenceScript
  makeTxOut address datum value' referenceScript


-- | Query a node.
queryAny :: MonadError CliError m
         => MonadIO m
         => LocalNodeConnectInfo CardanoMode -- ^ The connection info for the local node.
         -> QueryInMode CardanoMode a        -- ^ The query.
         -> m a                              -- ^ Action for running the query.
queryAny connection =
 liftCliIO
   . queryNodeLocalState connection Nothing


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


filterUtxos :: OutputQuery era result -> UTxO era -> result
filterUtxos = do
  let
    filterByValue check (UTxO candidates) = do
      let
        query' (_, TxOut _ v _ _) = check $ txOutValueToValue v
        fromList = UTxO . M.fromList
        (oqrMatching, oqrNonMatching) =
            (fromList *** fromList)
            . partition query'
            $ M.toList candidates
      OutputQueryResult {..}
  \case
    LovelaceOnly amountCheck -> do
      filterByValue \v -> let l = selectLovelace v in lovelaceToValue l == v && amountCheck l
    AssetOnly asset -> do
      filterByValue \v -> length (valueToList v) == 2 && selectAsset v asset >= 1
    FindReferenceScript pv scriptHash -> do
      let hashScriptInAnyLang (C.ScriptInAnyLang _ script) = C.hashScript script
      \(UTxO candidates) -> tillFirstMatch (M.toList candidates) \case
          t@(_, TxOut _ _ _ (ReferenceScript _ script)) -> if hashScriptInAnyLang script == scriptHash
            then case (pv, script) of
              (PlutusScriptV2, C.ScriptInAnyLang _ (C.PlutusScript PlutusScriptV2 script')) -> Just (AnUTxO t, script')
              -- FIXME: Improve error reporting
              _                                                                             -> Nothing
            else Nothing
          _ -> Nothing


-- | Select a UTxOs at an address.
selectUtxosImpl :: MonadError CliError m
                => MonadIO m
                => MonadReader (CliEnv era) m
                => LocalNodeConnectInfo CardanoMode  -- ^ The connection info for the local node.
                -> AddressInEra era                  -- ^ The address.
                -> OutputQuery era result            -- ^ Filter for the results.
                -> m result
selectUtxosImpl connection address query =
  filterUtxos query <$> queryUtxos connection address


-- | Select a UTxOs at an address.
selectUtxos :: MonadError CliError m
            => MonadIO m
            => MonadReader (CliEnv era) m
            => LocalNodeConnectInfo CardanoMode                   -- ^ The connection info for the local node.
            -> AddressInEra era                                   -- ^ The address.
            -> Maybe (OutputQuery era (OutputQueryResult era))    -- ^ Filter for the results.
            -> m ()
selectUtxos connection address query =
  do
    matching <- case query of
      Nothing     -> queryUtxos connection address
      Just query' -> oqrMatching <$> selectUtxosImpl connection address query'
    liftIO
      . mapM_ (print . fst)
      . M.toList
      . C.unUTxO
      $ matching


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
            => LocalNodeConnectInfo CardanoMode                             -- ^ The connection info for the local node.
            -> Value                                                        -- ^ The value of the input from the script, if any.
            -> [TxOut CtxTx era]                                            -- ^ The transaction outputs.
            -> Maybe (PayToScript era)                                      -- ^ Otherwise unlisted outputs to the script address.
            -> AddressInEra era                                             -- ^ The change address.
            -> CoinSelectionStrategy                                        -- ^ Indicate which `UTxO`'s to ignore.
            -> m (TxIn, [TxIn], [TxOut CtxTx era])                          -- ^ Action select the collateral, inputs, and outputs.
selectCoins connection inputs outputs pay changeAddress CoinSelectionStrategy {..} =
  do
    let
      isInlineDatum C.TxOutDatumInline {} = True
      isInlineDatum _                     = False

      include (txIn, TxOut _ _ datum ref) =
           (not csPreserveReferenceScripts || ref == ReferenceScriptNone)
        && (not csPreserveInlineDatums || not (isInlineDatum datum))
        && notElem txIn csPreserveTxIns

    -- Find the UTxOs that we have to work with.
    utxos <-
          fmap (filter include . M.toList . unUTxO)
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
      case filter (\candidate -> let value = txOutToValue $ snd candidate in onlyLovelace value && selectLovelace value >= selectLovelace fee) utxos of
        utxo : _ -> pure $ fst utxo
        []       -> do
          let
            adaOnlyUtxos = filter (\candidate -> let value = txOutToValue $ snd candidate in onlyLovelace value) utxos
          throwError . CliError $ "No collateral found in " <> show utxos <> ". Ada only utxos: " <> show adaOnlyUtxos <> ". Fee is " <> show fee <> "."
      :: m TxIn
    -- Bound the lovelace that must be included with change
    minUtxo <-
      (<>) <$> findMinUtxo protocol (changeAddress, Nothing, universe)  -- Output to native tokens.
           <*> findMinUtxo protocol (changeAddress, Nothing, mempty  )  -- Pure lovelace to change address.
      :: m Value
    let
      -- Compute the value of the outputs.
      outgoing :: Value
      outgoing = foldMap txOutToValue outputs <> maybe mempty PayToScript.value pay
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
          delta :: [Quantity]
          delta =
            fmap snd
              . valueToList
              . deleteLovelace
              $ candidate <> negateValue required
          excess :: Int
          excess  = length $ filter (> 0) delta
          deficit :: Int
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
          candidate' :: Value
          candidate' = txOutToValue $ snd candidate
          excess :: Int
          deficit :: Int
          (excess, deficit) = matchingCoins required candidate'
          notOnlyLovelace :: Bool
          notOnlyLovelace = not $ onlyLovelace candidate'
          insufficientLovelace :: Bool
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
      select _ [] = []
      select required candidates =
        let
          -- Choose the best UTxO from the candidates.
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
      change =                                            -- This is the change required to balance native tokens.
        deleteLovelace                                    -- The lovelace are irrelevant because pure-lovelace change is handled during the final balancing.
          $ (mconcat $ txOutToValue . snd <$> selection)  -- The inputs selected by the algorithm for spending many include native tokens that weren't in the required `outputs`.
          <> negateValue incoming                         -- The tokens required by `outputs` (as represented in the `incoming` requirement) shouldn't be included as change.
    -- Compute the change that contains native tokens used for balancing, omitting ones explicitly specified in the outputs.
    output <-
      if change == mempty
        then pure []
        else do
          (a, d, v) <- ensureMinUtxo protocol (changeAddress, Nothing, change)
          (: []) <$> makeTxOut' a d v
    -- Return the coin selection.
    pure
      (
        collateral
      , fst <$> selection
      , outputs <> output
      )
    -- FIXME: There are pathological failures that could happen if there are very many native tokens.
