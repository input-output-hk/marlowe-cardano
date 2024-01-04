{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Export information for Marlowe contracts and roles.
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
module Language.Marlowe.CLI.Export (
  buildAddress,
  buildValidatorInfo,
  exportAddress,

  -- * Contract and Transaction
  buildMarlowe,
  exportMarlowe,
  printMarlowe,

  -- * Address
  buildMarloweAddress,
  exportMarloweAddress,

  -- * Validator
  exportMarloweValidator,
  marloweValidatorInfo,

  -- * Datum
  buildMarloweDatum,
  exportDatum,

  -- * Redeemer
  buildRedeemer,
  exportRedeemer,

  -- * Roles Address
  buildRoleAddress,
  exportRoleAddress,

  -- * Role Validator
  exportRoleValidator,
  payoutValidatorInfo,
  openRoleValidatorInfo,

  -- * Role Datum
  buildRoleDatum,
  exportRoleDatum,

  -- * Role Redeemer
  buildRoleRedeemer,
  exportRoleRedeemer,
) where

import Cardano.Api (
  AddressInEra,
  NetworkId,
  PaymentCredential (..),
  Script (PlutusScript),
  ScriptDataJsonSchema (..),
  ScriptDataSupportedInEra (..),
  SerialiseAsRawBytes (..),
  StakeAddressReference (..),
  hashScript,
  hashScriptDataBytes,
  makeShelleyAddressInEra,
  scriptDataToJson,
  serialiseAddress,
  unsafeHashableScriptData,
 )
import Cardano.Api.Shelley (fromPlutusData)
import Control.Monad (join, when)
import Control.Monad.Except (MonadError, MonadIO, liftEither, liftIO)
import Data.Aeson (encode)
import Language.Marlowe.CLI.IO (
  decodeFileStrict,
  getPV2CostModelParams,
  getProtocolParams,
  maybeWriteJson,
  maybeWriteTextEnvelope,
 )
import Language.Marlowe.CLI.Types (
  CliEnv,
  CliError (..),
  DatumInfo (..),
  MarloweInfo (..),
  QueryExecutionContext (..),
  RedeemerInfo (..),
  ValidatorInfo (..),
  askEra,
  asksEra,
  doWithCardanoEra,
  doWithShelleyBasedEra,
  queryContextNetworkId,
  validatorInfo',
  withCardanoEra,
  withShelleyBasedEra,
 )
import Language.Marlowe.Core.V1.Semantics (MarloweData (..), MarloweParams)
import Language.Marlowe.Core.V1.Semantics.Types (Contract (..), Input, State (..), Token (Token))
import PlutusLedgerApi.V2 (BuiltinData, Datum (..), Redeemer (..))
import PlutusTx (builtinDataToData, toBuiltinData)
import System.IO (hPutStrLn, stderr)

import Data.ByteString.Lazy qualified as LBS (toStrict)
import Data.ByteString.Lazy.Char8 qualified as LBS8 (unpack)
import Data.Text qualified as T (unpack)

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as CS
import Codec.Serialise (serialise)
import Control.Monad.Reader (MonadReader)
import Data.ByteString.Short qualified as SBS
import Language.Marlowe.CLI.Cardano.Api qualified as C
import Language.Marlowe.CLI.Cardano.Api.PlutusScript (withPlutusScriptVersion)
import Language.Marlowe.Scripts (marloweValidator, openRolesValidator, payoutValidator)
import Language.Marlowe.Scripts.Types (marloweTxInputsFromInputs)
import PlutusLedgerApi.Common (ProtocolVersion)
import PlutusLedgerApi.V1 (DatumHash (..), toBuiltin, toData)

-- | Build comprehensive information about a Marlowe contract and transaction.
buildMarlowe
  :: (MonadIO m)
  => MarloweParams
  -> ScriptDataSupportedInEra era
  -> ProtocolVersion
  -> [Integer]
  -- ^ The cost model parameters.
  -> NetworkId
  -- ^ The network ID.
  -> StakeAddressReference
  -- ^ The stake address.
  -> Contract
  -- ^ The contract.
  -> State
  -- ^ The contract's state.
  -> [Input]
  -- ^ The contract's input,
  -> m (Either CliError (MarloweInfo CS.PlutusScriptV2 era))
  -- ^ The contract and transaction information, or an error message.
buildMarlowe marloweParams era protocolVersion costModel network stake contract state inputs =
  do
    pure do
      miValidatorInfo <-
        validatorInfo' marloweValidator Nothing era protocolVersion costModel network stake
      let miDatumInfo = buildMarloweDatum marloweParams contract state
          miRedeemerInfo = buildRedeemer inputs
      pure MarloweInfo{..}

-- | Export to a file the comprehensive information about a Marlowe contract and transaction.
exportMarlowe
  :: forall m era
   . (MonadError CliError m, MonadIO m, MonadReader (CliEnv era) m)
  => MarloweParams
  -- ^ The Marlowe contract parameters.
  -> ProtocolVersion
  -> [Integer]
  -- ^ The cost model parameters.
  -> NetworkId
  -- ^ The network ID.
  -> StakeAddressReference
  -- ^ The stake address.
  -> FilePath
  -- ^ The JSON file containing the contract.
  -> FilePath
  -- ^ The JSON file containing the contract's state.
  -> [FilePath]
  -- ^ The JSON files containing the contract's inputs.
  -> Maybe FilePath
  -- ^ The output JSON file for Marlowe contract information.
  -> Bool
  -- ^ Whether to print statistics about the contract.
  -> m ()
  -- ^ Action to export the contract and transaction information to a file.
exportMarlowe marloweParams protocolVersion costModel network stake contractFile stateFile inputFiles outputFile printStats =
  do
    contract :: Contract <- decodeFileStrict contractFile
    state :: State <- decodeFileStrict stateFile
    inputs :: [Input] <- mapM decodeFileStrict inputFiles
    marloweInfo@MarloweInfo{..} <-
      liftEither
        =<< join
          (asksEra \era -> buildMarlowe @m marloweParams era protocolVersion costModel network stake contract state inputs)
    let ValidatorInfo{..} = miValidatorInfo
        DatumInfo{..} = miDatumInfo
        RedeemerInfo{..} = miRedeemerInfo
    doWithShelleyBasedEra $ maybeWriteJson outputFile marloweInfo
    liftIO
      . when printStats
      $ do
        hPutStrLn stderr ""
        hPutStrLn stderr $ "Bare-validator cost: " <> show viCost
        hPutStrLn stderr $ "Validator size: " <> show viSize
        case viTxIn of
          Just txIn -> hPutStrLn stderr $ "Validator script reference: " <> show txIn
          Nothing -> pure ()
        hPutStrLn stderr $ "Datum size: " <> show diSize
        hPutStrLn stderr $ "Redeemer size: " <> show riSize
        hPutStrLn stderr $ "Total size: " <> show (viSize + diSize + riSize)

-- | Print information about a Marlowe contract and transaction.
printMarlowe
  :: forall m lang era
   . (MonadError CliError m, MonadIO m, CS.IsPlutusScriptLanguage lang)
  => MarloweParams
  -- ^ The Marlowe contract parameters.
  -> ScriptDataSupportedInEra era
  -> ProtocolVersion
  -> [Integer]
  -- ^ The cost model parameters.
  -> NetworkId
  -- ^ The network ID.
  -> StakeAddressReference
  -- ^ The stake address.
  -> Contract
  -- ^ The contract.
  -> State
  -- ^ The contract's state.
  -> [Input]
  -- ^ The contract's input,
  -> m ()
  -- ^ Action to print the contract and transaction information.
printMarlowe marloweParams era protocolVersion costModel network stake contract state inputs =
  do
    MarloweInfo{..} <-
      liftEither =<< buildMarlowe @_ marloweParams era protocolVersion costModel network stake contract state inputs
    let ValidatorInfo{..} = miValidatorInfo
        DatumInfo{..} = miDatumInfo
        RedeemerInfo{..} = miRedeemerInfo
    liftIO $
      do
        putStrLn ""
        putStrLn $ "Contract: " <> show contract
        putStrLn ""
        putStrLn $ "State: " <> show state
        putStrLn ""
        putStrLn $ "Inputs: " <> show inputs
        putStrLn ""
        putStrLn $
          "Validator: "
            <> LBS8.unpack
              (withPlutusScriptVersion (CS.plutusScriptVersion @lang) $ encode $ C.serialiseToTextEnvelope Nothing viScript)
        putStrLn ""
        putStrLn $ "Validator address: " <> T.unpack (withCardanoEra era $ serialiseAddress viAddress)
        putStrLn ""
        putStrLn $ "Validator hash: " <> show viHash
        putStrLn ""
        putStrLn $ "Validator size: " <> show viSize
        putStrLn ""
        case viTxIn of
          Just txIn -> do
            putStrLn $ "Validator script reference: " <> show txIn
            putStrLn ""
          Nothing -> pure ()
        putStrLn $ "Bare-validator cost: " <> show viCost
        putStrLn ""
        putStrLn $ "Datum:" <> LBS8.unpack (encode diJson)
        putStrLn ""
        putStrLn $ "Datum hash: " <> show diHash
        putStrLn ""
        putStrLn $ "Datum size: " <> show diSize
        putStrLn ""
        putStrLn $ "Redeemer: " <> LBS8.unpack (encode riJson)
        putStrLn ""
        putStrLn $ "Redeemer size: " <> show riSize
        putStrLn ""
        putStrLn $ "Total size: " <> show (viSize + diSize + riSize)

-- | Compute the address of a validator.
buildAddress
  :: forall lang era
   . (CS.IsPlutusScriptLanguage lang)
  => CS.PlutusScript lang
  -- ^ The validator.
  -> ScriptDataSupportedInEra era
  -> NetworkId
  -- ^ The network ID.
  -> StakeAddressReference
  -- ^ The stake address.
  -> AddressInEra era
  -- ^ The script address.
buildAddress script era network stake =
  let viScript = PlutusScript CS.plutusScriptVersion script
   in withShelleyBasedEra era $
        makeShelleyAddressInEra
          network
          (PaymentCredentialByScript $ hashScript viScript)
          stake

-- | Compute the address of a Marlowe contract.
buildMarloweAddress
  :: forall m era
   . (MonadIO m)
  => ScriptDataSupportedInEra era
  -> NetworkId
  -- ^ The network ID.
  -> StakeAddressReference
  -- ^ The stake address.
  -> m (AddressInEra era)
  -- ^ The script address.
buildMarloweAddress era network stake = do
  pure $ buildAddress marloweValidator era network stake

-- | Print the address of a validator.
exportAddress
  :: forall era lang m
   . (MonadIO m, MonadReader (CliEnv era) m, CS.IsPlutusScriptLanguage lang)
  => CS.PlutusScript lang
  -- ^ The validator.
  -> NetworkId
  -- ^ The network ID.
  -> StakeAddressReference
  -- ^ The stake address.
  -> m ()
  -- ^ Action to print the script address.
exportAddress validator network stake = do
  era <- askEra
  let address = buildAddress validator era network stake
  doWithShelleyBasedEra $ liftIO $ putStrLn $ T.unpack $ serialiseAddress address

-- | Print the address of a Marlowe contract.
exportMarloweAddress
  :: forall m era
   . (MonadIO m, MonadReader (CliEnv era) m)
  => NetworkId
  -- ^ The network ID.
  -> StakeAddressReference
  -- ^ The stake address.
  -> m ()
  -- ^ Action to print the script address.
exportMarloweAddress network stake = do
  exportAddress marloweValidator network stake

buildValidatorInfo
  :: (MonadReader (CliEnv era) m, MonadIO m, MonadError CliError m, CS.IsPlutusScriptLanguage lang)
  => QueryExecutionContext era
  -> CS.PlutusScript lang
  -> Maybe C.TxIn
  -> StakeAddressReference
  -> m (ValidatorInfo lang era)
buildValidatorInfo queryCtx plutusScript txIn stake = do
  era <- askEra
  protocolParams <- getProtocolParams queryCtx
  costModel <- getPV2CostModelParams queryCtx
  let networkId = queryContextNetworkId queryCtx
      protocolVersion = C.toPlutusProtocolVersion $ CS.protocolParamProtocolVersion protocolParams
  validatorInfo' plutusScript txIn era protocolVersion costModel networkId stake

-- | Export to a file the validator information.
exportValidatorImpl
  :: forall lang era m
   . (MonadError CliError m, MonadReader (CliEnv era) m, CS.IsPlutusScriptLanguage lang)
  => (MonadIO m)
  => CS.PlutusScript lang
  -> ProtocolVersion
  -> [Integer]
  -- ^ The cost model parameters.
  -> NetworkId
  -- ^ The network ID.
  -> StakeAddressReference
  -- ^ The stake address.
  -> Maybe FilePath
  -- ^ The output JSON file for the validator information.
  -> Bool
  -- ^ Whether to print the validator hash.
  -> Bool
  -- ^ Whether to print statistics about the validator.
  -> m ()
  -- ^ Action to export the validator information to a file.
exportValidatorImpl plutusScript protocolVersion costModel network stake outputFile printHash printStats =
  do
    era <- askEra
    ValidatorInfo{..} <- validatorInfo' plutusScript Nothing era protocolVersion costModel network stake
    maybeWriteTextEnvelope outputFile $ C.PlutusScript C.plutusScriptVersion plutusScript
    doWithCardanoEra $
      liftIO $
        do
          hPutStrLn stderr $ T.unpack $ serialiseAddress viAddress
          when printHash $
            do
              hPutStrLn stderr ""
              hPutStrLn stderr $ "Validator hash: " <> show viHash
              case viTxIn of
                Just txIn -> do
                  hPutStrLn stderr ""
                  hPutStrLn stderr $ "Validator script reference: " <> show txIn
                Nothing -> pure ()
          when printStats $
            do
              hPutStrLn stderr ""
              hPutStrLn stderr $ "Validator size: " <> show viSize
              hPutStrLn stderr $ "Bare-validator cost: " <> show viCost

-- | Current Marlowe validator information.
marloweValidatorInfo
  :: (MonadIO m)
  => ScriptDataSupportedInEra era
  -- ^ The era to build he validator in.
  -> ProtocolVersion
  -> [Integer]
  -- ^ The cost model parameters.
  -> NetworkId
  -- ^ The network ID.
  -> StakeAddressReference
  -- ^ The stake address.
  -> m (Either CliError (ValidatorInfo CS.PlutusScriptV2 era))
  -- ^ The validator information, or an error message.
marloweValidatorInfo script prot costModel network stake = do
  pure $ validatorInfo' marloweValidator Nothing script prot costModel network stake

-- | Export to a file the validator information about a Marlowe contract.
exportMarloweValidator
  :: forall era m
   . (MonadError CliError m, MonadReader (CliEnv era) m)
  => (MonadIO m)
  => ProtocolVersion
  -> [Integer]
  -- ^ The cost model parameters.
  -> NetworkId
  -- ^ The network ID.
  -> StakeAddressReference
  -- ^ The stake address.
  -> Maybe FilePath
  -- ^ The output JSON file for the validator information.
  -> Bool
  -- ^ Whether to print the validator hash.
  -> Bool
  -- ^ Whether to print statistics about the validator.
  -> m ()
  -- ^ Action to export the validator information to a file.
exportMarloweValidator prot costModel network stake out printHash printStats = do
  exportValidatorImpl marloweValidator prot costModel network stake out printHash printStats

-- | Build the datum information about a Marlowe transaction.
buildDatumImpl
  :: BuiltinData
  -- ^ The datum.
  -> DatumInfo
  -- ^ Information about the transaction datum.
buildDatumImpl datum =
  let diDatum = Datum datum
      diBytes = SBS.toShort . LBS.toStrict . serialise $ diDatum
      diJson =
        scriptDataToJson ScriptDataJsonDetailedSchema
          . unsafeHashableScriptData
          . fromPlutusData
          $ PlutusTx.builtinDataToData datum
      diHash =
        DatumHash . toBuiltin . serialiseToRawBytes . hashScriptDataBytes . unsafeHashableScriptData . fromPlutusData $
          toData diDatum
      diSize = SBS.length diBytes
   in DatumInfo{..}

-- | Build the datum information about a Marlowe transaction.
buildMarloweDatum
  :: MarloweParams
  -> Contract
  -- ^ The contract.
  -> State
  -- ^ The contract's state.
  -> DatumInfo
  -- ^ Information about the transaction datum.
buildMarloweDatum marloweParams marloweContract marloweState =
  let marloweData = MarloweData{..}
      marloweDatum = PlutusTx.toBuiltinData marloweData
   in buildDatumImpl marloweDatum

-- | Export to a file the datum information about a Marlowe transaction.
exportDatumImpl
  :: (MonadError CliError m)
  => (MonadIO m)
  => BuiltinData
  -- ^ The datum
  -> Maybe FilePath
  -- ^ The output JSON file for the datum information.
  -> Bool
  -- ^ Whether to print statistics about the datum.
  -> m ()
  -- ^ Action to export the datum information to a file.
exportDatumImpl datum outputFile printStats =
  do
    let DatumInfo{..} = buildDatumImpl datum
    maybeWriteJson outputFile diJson
    liftIO $
      do
        print diHash
        when printStats $
          do
            hPutStrLn stderr ""
            hPutStrLn stderr $ "Datum size: " <> show diSize

-- | Export to a file the datum information about a Marlowe transaction.
exportDatum
  :: (MonadError CliError m)
  => (MonadIO m)
  => MarloweParams
  -- ^ `MarloweParams` used by the validator.
  -> FilePath
  -- ^ The JSON file containing the contract.
  -> FilePath
  -- ^ The JSON file containing the contract's state.
  -> Maybe FilePath
  -- ^ The output JSON file for the datum information.
  -> Bool
  -- ^ Whether to print statistics about the datum.
  -> m ()
  -- ^ Action to export the datum information to a file.
exportDatum marloweParams contractFile stateFile outputFile printStats =
  do
    marloweContract <- decodeFileStrict contractFile
    marloweState <- decodeFileStrict stateFile
    let marloweData = MarloweData{..}
        marloweDatum = PlutusTx.toBuiltinData marloweData
    exportDatumImpl marloweDatum outputFile printStats

-- | Build the redeemer information about a Marlowe transaction.
buildRedeemerImpl
  :: BuiltinData
  -- ^ The redeemer.
  -> RedeemerInfo
  -- ^ Information about the transaction redeemer.
buildRedeemerImpl redeemer =
  let riRedeemer = Redeemer redeemer
      riBytes = SBS.toShort . LBS.toStrict . serialise $ riRedeemer
      riJson =
        scriptDataToJson ScriptDataJsonDetailedSchema
          . unsafeHashableScriptData
          . fromPlutusData
          $ PlutusTx.builtinDataToData redeemer
      riSize = SBS.length riBytes
   in RedeemerInfo{..}

-- | Build the redeemer information about a Marlowe transaction.
buildRedeemer
  :: [Input]
  -- ^ The contract's input,
  -> RedeemerInfo
  -- ^ Information about the transaction redeemer.
buildRedeemer = buildRedeemerImpl . PlutusTx.toBuiltinData . marloweTxInputsFromInputs

-- | Export to a file the redeemer information about a Marlowe transaction.
exportRedeemerImpl
  :: (MonadError CliError m)
  => (MonadIO m)
  => BuiltinData
  -- ^ The redeemer.
  -> Maybe FilePath
  -- ^ The output JSON file for Marlowe contract information.
  -> Bool
  -- ^ Whether to print statistics on the contract.
  -> m ()
  -- ^ Action to export the redeemer information to a file.
exportRedeemerImpl redeemer outputFile printStats =
  do
    let RedeemerInfo{..} = buildRedeemerImpl redeemer
    maybeWriteJson outputFile riJson
    liftIO
      . when printStats
      $ do
        hPutStrLn stderr ""
        hPutStrLn stderr $ "Redeemer size: " <> show riSize

-- | Export to a file the redeemer information about a Marlowe transaction.
exportRedeemer
  :: (MonadError CliError m)
  => (MonadIO m)
  => [FilePath]
  -- ^ The files containing the contract's inputs.
  -> Maybe FilePath
  -- ^ The output JSON file for Marlowe contract information.
  -> Bool
  -- ^ Whether to print statistics on the contract.
  -> m ()
  -- ^ Action to export the redeemer information to a file.
exportRedeemer inputFiles outputFile printStats =
  do
    inputs <- mapM decodeFileStrict inputFiles
    exportRedeemerImpl (PlutusTx.toBuiltinData (inputs :: [Input])) outputFile printStats

-- -- | Compute the role address of a Marlowe contract.
buildRoleAddress
  :: forall era m
   . (MonadIO m)
  => ScriptDataSupportedInEra era
  -> NetworkId
  -- ^ The network ID.
  -> StakeAddressReference
  -- ^ The stake address.
  -> m (AddressInEra era)
  -- ^ The script address.
buildRoleAddress script network stake = do
  pure $ buildAddress payoutValidator script network stake

-- | Print the role address of a Marlowe contract.
exportRoleAddress
  :: forall era m
   . (MonadIO m, MonadReader (CliEnv era) m)
  => NetworkId
  -- ^ The network ID.
  -> StakeAddressReference
  -- ^ The stake address.
  -> m ()
  -- ^ Action to print the script address.
exportRoleAddress network stake = do
  exportAddress payoutValidator network stake

-- | Current Marlowe validator information.
payoutValidatorInfo
  :: (MonadIO m)
  => ScriptDataSupportedInEra era
  -- ^ The era to build he validator in.
  -> ProtocolVersion
  -> [Integer]
  -- ^ The cost model parameters.
  -> NetworkId
  -- ^ The network ID.
  -> StakeAddressReference
  -- ^ The stake address.
  -> m (Either CliError (ValidatorInfo CS.PlutusScriptV2 era))
  -- ^ The validator information, or an error message.
payoutValidatorInfo script prot cost network stake = do
  pure $ validatorInfo' payoutValidator Nothing script prot cost network stake

-- | Open role validator
openRoleValidatorInfo
  :: (MonadIO m)
  => ScriptDataSupportedInEra era
  -- ^ The era to build he validator in.
  -> ProtocolVersion
  -> [Integer]
  -- ^ The cost model parameters.
  -> NetworkId
  -- ^ The network ID.
  -> StakeAddressReference
  -- ^ The stake address.
  -> m (Either CliError (ValidatorInfo CS.PlutusScriptV2 era))
  -- ^ The validator information, or an error message.
openRoleValidatorInfo script prot cost network stake = do
  pure $ validatorInfo' openRolesValidator Nothing script prot cost network stake

-- | Export to a file the role validator information about a Marlowe contract.
exportRoleValidator
  :: forall era m
   . (MonadError CliError m, MonadReader (CliEnv era) m)
  => (MonadIO m)
  => ProtocolVersion
  -- ^ The currency symbol for Marlowe contract roles.
  -> [Integer]
  -- ^ The cost model parameters.
  -> NetworkId
  -- ^ The network ID.
  -> StakeAddressReference
  -- ^ The stake address.
  -> Maybe FilePath
  -- ^ The output JSON file for the validator information.
  -> Bool
  -- ^ Whether to print the validator hash.
  -> Bool
  -- ^ Whether to print statistics about the validator.
  -> m ()
  -- ^ Action to export the validator information to a file.
exportRoleValidator prot cost network stake out printHash printStats = do
  exportValidatorImpl payoutValidator prot cost network stake out printHash printStats

-- | Build the role datum information about a Marlowe transaction.
buildRoleDatum
  :: Token
  -> DatumInfo
  -- ^ Information about the transaction datum.
buildRoleDatum (Token currencySymbol tokenName) = buildDatumImpl $ PlutusTx.toBuiltinData (currencySymbol, tokenName)

-- | Export to a file the role datum information about a Marlowe transaction.
exportRoleDatum
  :: (MonadError CliError m)
  => (MonadIO m)
  => Token
  -- ^ The role token.
  -> Maybe FilePath
  -- ^ The output JSON file for the datum information.
  -> Bool
  -- ^ Whether to print statistics about the datum.
  -> m ()
  -- ^ Action to export the datum information to a file.
exportRoleDatum = exportDatumImpl . PlutusTx.toBuiltinData

-- | Build the role redeemer information about a Marlowe transaction.
buildRoleRedeemer :: RedeemerInfo
  -- ^ Information about the transaction redeemer.
buildRoleRedeemer = buildRedeemerImpl $ PlutusTx.toBuiltinData ()

-- | Export to a file the role redeemer information about a Marlowe transaction.
exportRoleRedeemer
  :: (MonadError CliError m)
  => (MonadIO m)
  => Maybe FilePath
  -- ^ The output JSON file for Marlowe contract information.
  -> Bool
  -- ^ Whether to print statistics on the contract.
  -> m ()
  -- ^ Action to export the redeemer information to a file.
exportRoleRedeemer = exportRedeemerImpl $ PlutusTx.toBuiltinData ()
