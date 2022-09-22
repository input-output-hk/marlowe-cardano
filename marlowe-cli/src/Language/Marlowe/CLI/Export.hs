-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Export information for Marlowe contracts and roles.
--
-----------------------------------------------------------------------------


{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Language.Marlowe.CLI.Export
  ( buildAddress
  , buildValidatorInfo
  , exportAddress
    -- * Contract and Transaction
  , buildMarlowe
  , exportMarlowe
  , printMarlowe
    -- * Address
  , buildMarloweAddress
  , exportMarloweAddress
    -- * Validator
  , exportMarloweValidator
  , marloweValidatorInfo
    -- * Datum
  , buildMarloweDatum
  , exportDatum
    -- * Redeemer
  , buildRedeemer
  , exportRedeemer
    -- * Roles Address
  , buildRoleAddress
  , exportRoleAddress
    -- * Role Validator
  , exportRoleValidator
  , roleValidatorInfo
    -- * Role Datum
  , buildRoleDatum
  , exportRoleDatum
    -- * Role Redeemer
  , buildRoleRedeemer
  , exportRoleRedeemer
  ) where


import Cardano.Api
  ( AddressInEra
  , NetworkId
  , PaymentCredential(..)
  , PlutusScriptV2
  , PlutusScriptVersion
  , Script(PlutusScript)
  , ScriptDataJsonSchema(..)
  , ScriptDataSupportedInEra(..)
  , StakeAddressReference(..)
  , hashScript
  , makeShelleyAddressInEra
  , scriptDataToJson
  , serialiseAddress
  )
import Cardano.Api.Shelley (fromPlutusData)
import Control.Monad (when)
import Control.Monad.Except (MonadError, MonadIO, liftEither, liftIO)
import Data.Aeson (encode)
import Language.Marlowe.CLI.IO
  (decodeFileStrict, getDefaultCostModel, maybeWriteJson, maybeWriteTextEnvelope, queryInEra)
import Language.Marlowe.CLI.Types
  ( CliEnv
  , CliError(..)
  , DatumInfo(..)
  , MarloweInfo(..)
  , RedeemerInfo(..)
  , ValidatorInfo(..)
  , askEra
  , asksEra
  , doWithCardanoEra
  , doWithShelleyBasedEra
  , validatorInfo'
  , withCardanoEra
  , withShelleyBasedEra
  )
import Language.Marlowe.Core.V1.Semantics (MarloweData(..), MarloweParams)
import Language.Marlowe.Core.V1.Semantics.Types (Contract(..), Input, State(..), Token(Token))
import Language.Marlowe.Scripts (marloweTxInputsFromInputs, marloweValidator, rolePayoutValidator)
import Ledger.Typed.Scripts ()
import Plutus.Script.Utils.Scripts (datumHash)
import Plutus.V2.Ledger.Api (BuiltinData, CostModelParams, Datum(..), Redeemer(..))
import PlutusTx (builtinDataToData, toBuiltinData)
import System.IO (hPutStrLn, stderr)

import qualified Data.ByteString.Lazy as LBS (toStrict)
import qualified Data.ByteString.Lazy.Char8 as LBS8 (unpack)
import qualified Data.Text as T (unpack)

import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS
import Codec.Serialise (serialise)
import Control.Monad.Reader (MonadReader)
import qualified Data.ByteString.Short as SBS
import qualified Language.Marlowe.CLI.Cardano.Api as C
import Language.Marlowe.CLI.Cardano.Api.PlutusScript
  (IsPlutusScriptLanguage(plutusScriptVersion), fromTypedValidator, fromV2TypedValidator)
import qualified Language.Marlowe.CLI.Cardano.Api.PlutusScript as PlutusScript
import Language.Marlowe.CLI.Plutus.Script.Utils (TypedValidator'(TypedValidatorV2))
import Plutus.ApiCommon (ProtocolVersion)


-- | Build comprehensive information about a Marlowe contract and transaction.
buildMarlowe :: MarloweParams
             -> ScriptDataSupportedInEra era
             -> ProtocolVersion
             -> CostModelParams                                   -- ^ The cost model parameters.
             -> NetworkId                                         -- ^ The network ID.
             -> StakeAddressReference                             -- ^ The stake address.
             -> Contract                                          -- ^ The contract.
             -> State                                             -- ^ The contract's state.
             -> [Input]                                           -- ^ The contract's input,
             -> Either CliError (MarloweInfo PlutusScriptV2 era)  -- ^ The contract and transaction information, or an error message.
buildMarlowe marloweParams era protocolVersion costModel network stake contract state inputs =
  do
    miValidatorInfo <- validatorInfo' (fromV2TypedValidator marloweValidator) Nothing era protocolVersion costModel network stake
    let
      miDatumInfo = buildMarloweDatum marloweParams contract state
      miRedeemerInfo = buildRedeemer inputs
    pure MarloweInfo{..}


-- | Export to a file the comprehensive information about a Marlowe contract and transaction.
exportMarlowe :: forall m era
               . MonadError CliError m
              => MonadIO m
              => MonadReader (CliEnv era) m
              => MarloweParams          -- ^ The Marlowe contract parameters.
              -> ProtocolVersion
              -> CostModelParams        -- ^ The cost model parameters.
              -> NetworkId              -- ^ The network ID.
              -> StakeAddressReference  -- ^ The stake address.
              -> FilePath               -- ^ The JSON file containing the contract.
              -> FilePath               -- ^ The JSON file containing the contract's state.
              -> [FilePath]             -- ^ The JSON files containing the contract's inputs.
              -> Maybe FilePath         -- ^ The output JSON file for Marlowe contract information.
              -> Bool                   -- ^ Whether to print statistics about the contract.
              -> m ()                   -- ^ Action to export the contract and transaction information to a file.
exportMarlowe marloweParams protocolVersion costModel network stake contractFile stateFile inputFiles outputFile printStats =
  do
    contract :: Contract <- decodeFileStrict contractFile
    state    :: State <- decodeFileStrict stateFile
    inputs   :: [Input] <- mapM decodeFileStrict inputFiles
    marloweInfo@MarloweInfo{..} <- liftEither =<< asksEra \era -> buildMarlowe marloweParams era protocolVersion costModel network stake contract state inputs
    let
      ValidatorInfo{..} = miValidatorInfo
      DatumInfo{..}     = miDatumInfo
      RedeemerInfo{..}  = miRedeemerInfo
    doWithShelleyBasedEra $ maybeWriteJson outputFile marloweInfo
    liftIO
      . when printStats
          $ do
            hPutStrLn stderr ""
            hPutStrLn stderr $ "Bare-validator cost: " <> show viCost
            hPutStrLn stderr $ "Validator size: " <> show viSize
            case viTxIn of
              Just txIn -> hPutStrLn stderr $ "Validator script reference: " <> show txIn
              Nothing   -> pure ()
            hPutStrLn stderr $ "Datum size: " <> show diSize
            hPutStrLn stderr $ "Redeemer size: " <> show riSize
            hPutStrLn stderr $ "Total size: " <> show (viSize + diSize + riSize)


-- | Print information about a Marlowe contract and transaction.
printMarlowe :: MonadError CliError m
              => MonadIO m
              => MarloweParams          -- ^ The Marlowe contract parameters.
              -> ScriptDataSupportedInEra era
              -> ProtocolVersion
              -> CostModelParams        -- ^ The cost model parameters.
              -> NetworkId              -- ^ The network ID.
              -> StakeAddressReference  -- ^ The stake address.
              -> Contract               -- ^ The contract.
              -> State                  -- ^ The contract's state.
              -> [Input]                -- ^ The contract's input,
              -> m ()                   -- ^ Action to print the contract and transaction information.
printMarlowe marloweParams era protocolVersion costModel network stake contract state inputs =
  do
    MarloweInfo{..} <- liftEither $ buildMarlowe marloweParams era protocolVersion costModel network stake contract state inputs
    let
      ValidatorInfo{..} = miValidatorInfo
      DatumInfo{..}     = miDatumInfo
      RedeemerInfo{..}  = miRedeemerInfo
    liftIO
      $ do
        putStrLn ""
        putStrLn $ "Contract: " <> show contract
        putStrLn ""
        putStrLn $ "State: " <> show state
        putStrLn ""
        putStrLn $ "Inputs: " <> show inputs
        putStrLn ""
        putStrLn $ "Validator: " <> LBS8.unpack (encode $ C.serialiseToTextEnvelope Nothing viScript)
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
buildAddress :: forall era lang t
             . IsPlutusScriptLanguage lang
             => TypedValidator' lang t    -- ^ The validator.
             -> ScriptDataSupportedInEra era
             -> NetworkId              -- ^ The network ID.
             -> StakeAddressReference  -- ^ The stake address.
             -> AddressInEra era       -- ^ The script address.
buildAddress anyValidator era network stake =
  let
    viScript = PlutusScript (plutusScriptVersion :: PlutusScriptVersion lang) (PlutusScript.fromTypedValidator anyValidator)
  in
    withShelleyBasedEra era $ makeShelleyAddressInEra
      network
      (PaymentCredentialByScript $ hashScript viScript)
      stake


-- | Compute the address of a Marlowe contract.
buildMarloweAddress :: ScriptDataSupportedInEra era
                    -> NetworkId              -- ^ The network ID.
                    -> StakeAddressReference  -- ^ The stake address.
                    -> AddressInEra era       -- ^ The script address.
buildMarloweAddress = buildAddress (TypedValidatorV2 marloweValidator)

-- | Print the address of a validator.
exportAddress :: forall era lang m t
              . MonadIO m
              => MonadReader (CliEnv era) m
              => IsPlutusScriptLanguage lang
              => TypedValidator' lang t    -- ^ The validator.
              -> NetworkId              -- ^ The network ID.
              -> StakeAddressReference  -- ^ The stake address.
              -> m ()                   -- ^ Action to print the script address.
exportAddress validator network stake = do
  era <- askEra
  let address = buildAddress validator era network stake
  doWithShelleyBasedEra $ liftIO $ putStrLn $ T.unpack $ serialiseAddress address


-- | Print the address of a Marlowe contract.
exportMarloweAddress :: (MonadIO m, MonadReader (CliEnv era) m)
                     => NetworkId              -- ^ The network ID.
                     -> StakeAddressReference  -- ^ The stake address.
                     -> m ()                   -- ^ Action to print the script address.
exportMarloweAddress = exportAddress (TypedValidatorV2 marloweValidator)


buildValidatorInfo :: (MonadReader (CliEnv era) m, MonadIO m, MonadError CliError m, IsPlutusScriptLanguage lang)
                   => CS.LocalNodeConnectInfo CS.CardanoMode -> CS.PlutusScript lang -> Maybe C.TxIn -> StakeAddressReference -> m (ValidatorInfo lang era)
buildValidatorInfo connection plutusScript txIn stake = do
  era <- askEra
  protocol <- queryInEra connection C.QueryProtocolParameters
  costModel <- getDefaultCostModel
  let
    networkId = C.localNodeNetworkId connection
    protocolVersion = C.toPlutusProtocolVersion $ CS.protocolParamProtocolVersion protocol
  validatorInfo' plutusScript txIn era protocolVersion costModel networkId stake


-- | Export to a file the validator information.
exportValidatorImpl :: (MonadError CliError m, MonadReader (CliEnv era) m, IsPlutusScriptLanguage lang)
                    => MonadIO m
                    => TypedValidator' lang t
                    -> ProtocolVersion
                    -> CostModelParams        -- ^ The cost model parameters.
                    -> NetworkId              -- ^ The network ID.
                    -> StakeAddressReference  -- ^ The stake address.
                    -> Maybe FilePath         -- ^ The output JSON file for the validator information.
                    -> Bool                   -- ^ Whether to print the validator hash.
                    -> Bool                   -- ^ Whether to print statistics about the validator.
                    -> m ()                   -- ^ Action to export the validator information to a file.
exportValidatorImpl validator protocolVersion costModel network stake outputFile printHash printStats =
  do
    era <- askEra
    let
      plutusScript = fromTypedValidator validator
    ValidatorInfo{..} <- validatorInfo' plutusScript Nothing era protocolVersion costModel network stake
    maybeWriteTextEnvelope outputFile $ PlutusScript.toScript plutusScript
    doWithCardanoEra $ liftIO
      $ do
        hPutStrLn stderr $ T.unpack $ serialiseAddress viAddress
        when printHash
          $ do
            hPutStrLn stderr ""
            hPutStrLn stderr $ "Validator hash: " <> show viHash
            case viTxIn of
              Just txIn -> do
                hPutStrLn stderr ""
                hPutStrLn stderr $ "Validator script reference: " <> show txIn
              Nothing -> pure ()
        when printStats
          $ do
            hPutStrLn stderr ""
            hPutStrLn stderr $ "Validator size: " <> show viSize
            hPutStrLn stderr $ "Bare-validator cost: " <> show viCost


-- | Current Marlowe validator information.
marloweValidatorInfo :: ScriptDataSupportedInEra era         -- ^ The era to build he validator in.
                     -> ProtocolVersion
                     -> CostModelParams                      -- ^ The cost model parameters.
                     -> NetworkId                            -- ^ The network ID.
                     -> StakeAddressReference                -- ^ The stake address.
                     -> Either CliError (ValidatorInfo PlutusScriptV2 era)  -- ^ The validator information, or an error message.
marloweValidatorInfo = validatorInfo' (fromV2TypedValidator marloweValidator) Nothing


-- | Export to a file the validator information about a Marlowe contract.
exportMarloweValidator :: (MonadError CliError m, MonadReader (CliEnv era) m)
                       => MonadIO m
                       => ProtocolVersion
                       -> CostModelParams        -- ^ The cost model parameters.
                       -> NetworkId              -- ^ The network ID.
                       -> StakeAddressReference  -- ^ The stake address.
                       -> Maybe FilePath         -- ^ The output JSON file for the validator information.
                       -> Bool                   -- ^ Whether to print the validator hash.
                       -> Bool                   -- ^ Whether to print statistics about the validator.
                       -> m ()                   -- ^ Action to export the validator information to a file.
exportMarloweValidator = exportValidatorImpl (TypedValidatorV2 marloweValidator)


-- | Build the datum information about a Marlowe transaction.
buildDatumImpl :: BuiltinData  -- ^ The datum.
               -> DatumInfo    -- ^ Information about the transaction datum.
buildDatumImpl datum =
  let
    diDatum = Datum datum
    diBytes = SBS.toShort . LBS.toStrict . serialise $ diDatum
    diJson =
      scriptDataToJson ScriptDataJsonDetailedSchema
        . fromPlutusData
        $ PlutusTx.builtinDataToData datum
    diHash = datumHash diDatum
    diSize = SBS.length diBytes
  in
    DatumInfo{..}


-- | Build the datum information about a Marlowe transaction.
buildMarloweDatum :: MarloweParams
                  -> Contract   -- ^ The contract.
                  -> State      -- ^ The contract's state.
                  -> DatumInfo  -- ^ Information about the transaction datum.
buildMarloweDatum marloweParams marloweContract marloweState =
  let
    marloweData = MarloweData{..}
    marloweDatum = PlutusTx.toBuiltinData marloweData
  in
    buildDatumImpl marloweDatum


-- | Export to a file the datum information about a Marlowe transaction.
exportDatumImpl :: MonadError CliError m
                => MonadIO m
                => BuiltinData     -- ^ The datum
                -> Maybe FilePath  -- ^ The output JSON file for the datum information.
                -> Bool            -- ^ Whether to print statistics about the datum.
                -> m ()            -- ^ Action to export the datum information to a file.
exportDatumImpl datum outputFile printStats =
  do
    let
      DatumInfo{..} = buildDatumImpl datum
    maybeWriteJson outputFile diJson
    liftIO
      $ do
        print diHash
        when printStats
          $ do
            hPutStrLn stderr ""
            hPutStrLn stderr $ "Datum size: " <> show diSize


-- | Export to a file the datum information about a Marlowe transaction.
exportDatum :: MonadError CliError m
            => MonadIO m
            => MarloweParams   -- ^ `MarloweParams` used by the validator.
            -> FilePath        -- ^ The JSON file containing the contract.
            -> FilePath        -- ^ The JSON file containing the contract's state.
            -> Maybe FilePath  -- ^ The output JSON file for the datum information.
            -> Bool            -- ^ Whether to print statistics about the datum.
            -> m ()            -- ^ Action to export the datum information to a file.
exportDatum marloweParams contractFile stateFile outputFile printStats =
  do
    marloweContract <- decodeFileStrict contractFile
    marloweState    <- decodeFileStrict stateFile
    let
      marloweData = MarloweData{..}
      marloweDatum = PlutusTx.toBuiltinData marloweData
    exportDatumImpl marloweDatum outputFile printStats


-- | Build the redeemer information about a Marlowe transaction.
buildRedeemerImpl :: BuiltinData   -- ^ The redeemer.
                  -> RedeemerInfo  -- ^ Information about the transaction redeemer.
buildRedeemerImpl redeemer =
  let
    riRedeemer = Redeemer redeemer
    riBytes = SBS.toShort . LBS.toStrict . serialise $ riRedeemer
    riJson =
      scriptDataToJson ScriptDataJsonDetailedSchema
        . fromPlutusData
        $ PlutusTx.builtinDataToData redeemer
    riSize = SBS.length riBytes
  in
    RedeemerInfo{..}


-- | Build the redeemer information about a Marlowe transaction.
buildRedeemer :: [Input]       -- ^ The contract's input,
              -> RedeemerInfo  -- ^ Information about the transaction redeemer.
buildRedeemer = buildRedeemerImpl . PlutusTx.toBuiltinData . marloweTxInputsFromInputs


-- | Export to a file the redeemer information about a Marlowe transaction.
exportRedeemerImpl :: MonadError CliError m
                   => MonadIO m
                   => BuiltinData     -- ^ The redeemer.
                   -> Maybe FilePath  -- ^ The output JSON file for Marlowe contract information.
                   -> Bool            -- ^ Whether to print statistics on the contract.
                   -> m ()            -- ^ Action to export the redeemer information to a file.
exportRedeemerImpl redeemer outputFile printStats =
  do
    let
      RedeemerInfo{..} = buildRedeemerImpl redeemer
    maybeWriteJson outputFile riJson
    liftIO
      . when printStats
          $ do
            hPutStrLn stderr ""
            hPutStrLn stderr $ "Redeemer size: " <> show riSize


-- | Export to a file the redeemer information about a Marlowe transaction.
exportRedeemer :: MonadError CliError m
               => MonadIO m
               => [FilePath]      -- ^ The files containing the contract's inputs.
               -> Maybe FilePath  -- ^ The output JSON file for Marlowe contract information.
               -> Bool            -- ^ Whether to print statistics on the contract.
               -> m ()            -- ^ Action to export the redeemer information to a file.
exportRedeemer inputFiles outputFile printStats =
  do
    inputs <- mapM decodeFileStrict inputFiles
    exportRedeemerImpl (PlutusTx.toBuiltinData (inputs :: [Input])) outputFile printStats


-- -- | Compute the role address of a Marlowe contract.
buildRoleAddress :: ScriptDataSupportedInEra era
                 -> NetworkId              -- ^ The network ID.
                 -> StakeAddressReference  -- ^ The stake address.
                 -> AddressInEra era       -- ^ The script address.
buildRoleAddress = buildAddress (TypedValidatorV2 rolePayoutValidator)


-- | Print the role address of a Marlowe contract.
exportRoleAddress :: (MonadIO m, MonadReader (CliEnv era) m)
                  => NetworkId              -- ^ The network ID.
                  -> StakeAddressReference  -- ^ The stake address.
                  -> m ()                   -- ^ Action to print the script address.
exportRoleAddress = exportAddress (TypedValidatorV2 rolePayoutValidator)


-- | Current Marlowe validator information.
roleValidatorInfo :: ScriptDataSupportedInEra era                        -- ^ The era to build he validator in.
                  -> ProtocolVersion
                  -> CostModelParams                                     -- ^ The cost model parameters.
                  -> NetworkId                                           -- ^ The network ID.
                  -> StakeAddressReference                               -- ^ The stake address.
                  -> Either CliError (ValidatorInfo PlutusScriptV2 era)  -- ^ The validator information, or an error message.
roleValidatorInfo = validatorInfo' (fromV2TypedValidator rolePayoutValidator) Nothing


-- | Export to a file the role validator information about a Marlowe contract.
exportRoleValidator :: (MonadError CliError m, MonadReader (CliEnv era) m)
                => MonadIO m
                => ProtocolVersion         -- ^ The currency symbol for Marlowe contract roles.
                -> CostModelParams        -- ^ The cost model parameters.
                -> NetworkId              -- ^ The network ID.
                -> StakeAddressReference  -- ^ The stake address.
                -> Maybe FilePath         -- ^ The output JSON file for the validator information.
                -> Bool                   -- ^ Whether to print the validator hash.
                -> Bool                   -- ^ Whether to print statistics about the validator.
                -> m ()                   -- ^ Action to export the validator information to a file.
exportRoleValidator = exportValidatorImpl (TypedValidatorV2 rolePayoutValidator)


-- | Build the role datum information about a Marlowe transaction.
buildRoleDatum :: Token
               -> DatumInfo  -- ^ Information about the transaction datum.
buildRoleDatum (Token currencySymbol tokenName) = buildDatumImpl $ PlutusTx.toBuiltinData (currencySymbol, tokenName)


-- | Export to a file the role datum information about a Marlowe transaction.
exportRoleDatum :: MonadError CliError m
                => MonadIO m
                => Token           -- ^ The role token.
                -> Maybe FilePath  -- ^ The output JSON file for the datum information.
                -> Bool            -- ^ Whether to print statistics about the datum.
                -> m ()            -- ^ Action to export the datum information to a file.
exportRoleDatum = exportDatumImpl . PlutusTx.toBuiltinData


-- | Build the role redeemer information about a Marlowe transaction.
buildRoleRedeemer :: RedeemerInfo  -- ^ Information about the transaction redeemer.
buildRoleRedeemer = buildRedeemerImpl $ PlutusTx.toBuiltinData ()


-- | Export to a file the role redeemer information about a Marlowe transaction.
exportRoleRedeemer :: MonadError CliError m
                   => MonadIO m
                   => Maybe FilePath  -- ^ The output JSON file for Marlowe contract information.
                   -> Bool            -- ^ Whether to print statistics on the contract.
                   -> m ()            -- ^ Action to export the redeemer information to a file.
exportRoleRedeemer = exportRedeemerImpl $ PlutusTx.toBuiltinData ()
