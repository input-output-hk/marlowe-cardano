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


{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Language.Marlowe.CLI.Export (
-- * Contract and Transaction
  exportMarlowe
, printMarlowe
, buildMarlowe
-- * Address
, exportAddress
, buildAddress
-- * Validator
, exportValidator
, buildValidator
-- * Datum
, exportDatum
, buildDatum
-- * Redeemer
, exportRedeemer
, buildRedeemer
-- * Roles Address
, exportRoleAddress
, buildRoleAddress
-- * Role Validator
, exportRoleValidator
, buildRoleValidator
-- * Role Datum
, exportRoleDatum
, buildRoleDatum
-- * Role Redeemer
, exportRoleRedeemer
, buildRoleRedeemer
) where


import Cardano.Api (AddressInEra, NetworkId, PaymentCredential (..), ScriptDataJsonSchema (..),
                    ScriptDataSupportedInEra (..), StakeAddressReference (..), hashScript, makeShelleyAddressInEra,
                    scriptDataToJson, serialiseAddress, serialiseToTextEnvelope)
import Cardano.Api.Shelley (fromPlutusData)
import Control.Monad (when)
import Control.Monad.Except (MonadError, MonadIO, liftEither, liftIO)
import Data.Aeson (encode)
import Language.Marlowe.CLI.IO (decodeFileStrict, maybeWriteJson, maybeWriteTextEnvelope)
import Language.Marlowe.CLI.Types (CliEnv, CliError (..), DatumInfo (..), MarloweInfo (..), RedeemerInfo (..),
                                   ValidatorInfo (..), askEra, asksEra, doWithCardanoEra, doWithShelleyBasedEra,
                                   withCardanoEra, withShelleyBasedEra)
import Language.Marlowe.Core.V1.Semantics (MarloweData (..), MarloweParams)
import Language.Marlowe.Core.V1.Semantics.Types (Contract (..), Input, State (..))
import Language.Marlowe.Scripts (marloweTxInputsFromInputs, rolePayoutScript, smallUntypedValidator)
import Ledger.Typed.Scripts (validatorScript)
import Plutus.Script.Utils.Scripts (datumHash)
import Plutus.Script.Utils.V1.Scripts (validatorHash)
import Plutus.V1.Ledger.Api (BuiltinData, CostModelParams, CurrencySymbol, Datum (..), Redeemer (..), TokenName,
                             Validator, VerboseMode (..), evaluateScriptCounting, getValidator)
import PlutusTx (builtinDataToData, toBuiltinData)
import System.IO (hPutStrLn, stderr)

import qualified Data.ByteString.Lazy as LBS (toStrict)
import qualified Data.ByteString.Lazy.Char8 as LBS8 (unpack)
import qualified Data.Text as T (unpack)

import qualified Cardano.Api as Script
import qualified Cardano.Api.Shelley as Script
import Codec.Serialise (serialise)
import Control.Monad.Reader (MonadReader)
import qualified Data.Bifunctor as Bifunctor
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as SBS
import qualified Plutus.V1.Ledger.Api as PV1
import Plutus.V1.Ledger.EvaluationContext (mkEvaluationContext)

-- | Build comprehensive information about a Marlowe contract and transaction.
buildMarlowe :: MarloweParams                      -- ^ The Marlowe contract parameters.
             -> ScriptDataSupportedInEra era
             -> CostModelParams                    -- ^ The cost model parameters.
             -> NetworkId                          -- ^ The network ID.
             -> StakeAddressReference              -- ^ The stake address.
             -> Contract                           -- ^ The contract.
             -> State                              -- ^ The contract's state.
             -> [Input]                            -- ^ The contract's input,
             -> Either CliError (MarloweInfo era)  -- ^ The contract and transaction information, or an error message.
buildMarlowe marloweParams era costModel network stake contract state inputs =
  do
    validatorInfo <- buildValidator marloweParams era costModel network stake
    let
      datumInfo = buildDatum contract state
      redeemerInfo = buildRedeemer inputs
    pure MarloweInfo{..}

-- | Export to a file the comprehensive information about a Marlowe contract and transaction.
exportMarlowe :: forall m era
               . MonadError CliError m
              => MonadIO m
              => MonadReader (CliEnv era) m
              => MarloweParams          -- ^ The Marlowe contract parameters.
              -> CostModelParams        -- ^ The cost model parameters.
              -> NetworkId              -- ^ The network ID.
              -> StakeAddressReference  -- ^ The stake address.
              -> FilePath               -- ^ The JSON file containing the contract.
              -> FilePath               -- ^ The JSON file containing the contract's state.
              -> [FilePath]             -- ^ The JSON files containing the contract's inputs.
              -> Maybe FilePath         -- ^ The output JSON file for Marlowe contract information.
              -> Bool                   -- ^ Whether to print statistics about the contract.
              -> m ()                   -- ^ Action to export the contract and transaction information to a file.
exportMarlowe marloweParams costModel network stake contractFile stateFile inputFiles outputFile printStats =
  do
    contract :: Contract <- decodeFileStrict contractFile
    state    :: State <- decodeFileStrict stateFile
    inputs   :: [Input] <- mapM decodeFileStrict inputFiles
    marloweInfo@MarloweInfo{..} <- liftEither =<< asksEra \era -> buildMarlowe marloweParams era costModel network stake contract state inputs
    let
      ValidatorInfo{..} = validatorInfo
      DatumInfo{..}     = datumInfo
      RedeemerInfo{..}  = redeemerInfo
    doWithShelleyBasedEra $ maybeWriteJson outputFile marloweInfo
    liftIO
      . when printStats
          $ do
            hPutStrLn stderr ""
            hPutStrLn stderr $ "Bare-validator cost: " <> show viCost
            hPutStrLn stderr $ "Validator size: " <> show viSize
            hPutStrLn stderr $ "Datum size: " <> show diSize
            hPutStrLn stderr $ "Redeemer size: " <> show riSize
            hPutStrLn stderr $ "Total size: " <> show (viSize + diSize + riSize)


-- | Print information about a Marlowe contract and transaction.
printMarlowe :: MonadError CliError m
              => MonadIO m
              => MarloweParams          -- ^ The Marlowe contract parameters.
              -> ScriptDataSupportedInEra era
              -> CostModelParams        -- ^ The cost model parameters.
              -> NetworkId              -- ^ The network ID.
              -> StakeAddressReference  -- ^ The stake address.
              -> Contract               -- ^ The contract.
              -> State                  -- ^ The contract's state.
              -> [Input]                -- ^ The contract's input,
              -> m ()                   -- ^ Action to print the contract and transaction information.
printMarlowe marloweParams era costModel network stake contract state inputs =
  do
    MarloweInfo{..} <- liftEither $ buildMarlowe marloweParams era costModel network stake contract state inputs
    let
      ValidatorInfo{..} = validatorInfo
      DatumInfo{..}     = datumInfo
      RedeemerInfo{..}  = redeemerInfo
    liftIO
      $ do
        putStrLn ""
        putStrLn $ "Contract: " <> show contract
        putStrLn ""
        putStrLn $ "State: " <> show state
        putStrLn ""
        putStrLn $ "Inputs: " <> show inputs
        putStrLn ""
        putStrLn $ "Validator: " <> LBS8.unpack (encode $ serialiseToTextEnvelope Nothing viScript)
        putStrLn ""
        putStrLn $ "Validator address: " <> T.unpack (withCardanoEra era $ serialiseAddress viAddress)
        putStrLn ""
        putStrLn $ "Validator hash: " <> show viHash
        putStrLn ""
        putStrLn $ "Validator size: " <> show viSize
        putStrLn ""
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


-- This function is not exposed by `Ledger.Tx.CardanoAPI` currently.
toCardanoApiScript :: PV1.Script -> Script.Script Script.PlutusScriptV1
toCardanoApiScript =
    Script.PlutusScript Script.PlutusScriptV1
  . Script.PlutusScriptSerialised
  . SBS.toShort
  . BSL.toStrict
  . serialise

-- | Compute the address of a validator.
buildAddressImpl :: Validator             -- ^ The validator.
                 -> ScriptDataSupportedInEra era
                 -> NetworkId              -- ^ The network ID.
                 -> StakeAddressReference  -- ^ The stake address.
                 -> AddressInEra era       -- ^ The script address.
buildAddressImpl viValidator era network stake =
  let
    script = getValidator viValidator
    viScript = toCardanoApiScript script
  in
    withShelleyBasedEra era $ makeShelleyAddressInEra
      network
      (PaymentCredentialByScript $ hashScript viScript)
      stake


-- | Compute the address of a Marlowe contract.
buildAddress :: MarloweParams          -- ^ The Marlowe contract parameters.
             -> ScriptDataSupportedInEra era
             -> NetworkId              -- ^ The network ID.
             -> StakeAddressReference  -- ^ The stake address.
             -> AddressInEra era       -- ^ The script address.
buildAddress = buildAddressImpl . validatorScript . smallUntypedValidator

-- | Print the address of a validator.
exportAddressImpl :: (MonadIO m, MonadReader (CliEnv era0) m)
                  => Validator             -- ^ The validator.
                  -> NetworkId              -- ^ The network ID.
                  -> StakeAddressReference  -- ^ The stake address.
                  -> m ()                   -- ^ Action to print the script address.
exportAddressImpl validator network stake = do
  era <- askEra
  let address = buildAddressImpl validator era network stake
  doWithShelleyBasedEra $ liftIO $ putStrLn $ T.unpack $ serialiseAddress address


-- | Print the address of a Marlowe contract.
exportAddress :: (MonadIO m, MonadReader (CliEnv era00) m)
              => MarloweParams          -- ^ The Marlowe contract parameters.
              -> NetworkId              -- ^ The network ID.
              -> StakeAddressReference  -- ^ The stake address.
              -> m ()                   -- ^ Action to print the script address.
exportAddress = exportAddressImpl . validatorScript . smallUntypedValidator

-- | Build validator info.
buildValidatorImpl :: Validator                            -- ^ The validator.
                   -> ScriptDataSupportedInEra era         -- ^ The era to build the validator in.
                   -> CostModelParams                      -- ^ The cost model parameters.
                   -> NetworkId                            -- ^ The network ID.
                   -> StakeAddressReference                -- ^ The stake address.
                   -> Either CliError (ValidatorInfo era)  -- ^ The validator information, or an error message.
buildValidatorImpl viValidator era costModel network stake =
  let
    script = getValidator viValidator
    viScript = toCardanoApiScript script
    viBytes = SBS.toShort . LBS.toStrict . serialise $ script
    viHash = validatorHash viValidator
    paymentCredential = PaymentCredentialByScript $ hashScript viScript
    viAddress = withShelleyBasedEra era $ makeShelleyAddressInEra network paymentCredential stake
    viSize = SBS.length viBytes
  in do
    evaluationContext <- Bifunctor.first (CliError . show) $ mkEvaluationContext costModel
    case evaluateScriptCounting (PV1.ProtocolVersion 5 0) Verbose evaluationContext viBytes [] of
      (_, Right viCost) -> Right ValidatorInfo{..}
      _                 -> Left $ CliError "Failed to evaluate cost of validator script."


-- | Build the validator information about a Marlowe contract.
buildValidator :: MarloweParams                        -- ^ The Marlowe contract parameters.
               -> ScriptDataSupportedInEra era         -- ^ The era to build he validator in.
               -> CostModelParams                      -- ^ The cost model parameters.
               -> NetworkId                            -- ^ The network ID.
               -> StakeAddressReference                -- ^ The stake address.
               -> Either CliError (ValidatorInfo era)  -- ^ The validator information, or an error message.
buildValidator = buildValidatorImpl . validatorScript . smallUntypedValidator


-- | Export to a file the validator information.
exportValidatorImpl :: (MonadError CliError m, MonadReader (CliEnv era0) m)
                => MonadIO m
                => Validator              -- ^ The validator.
                -> CostModelParams        -- ^ The cost model parameters.
                -> NetworkId              -- ^ The network ID.
                -> StakeAddressReference  -- ^ The stake address.
                -> Maybe FilePath         -- ^ The output JSON file for the validator information.
                -> Bool                   -- ^ Whether to print the validator hash.
                -> Bool                   -- ^ Whether to print statistics about the validator.
                -> m ()                   -- ^ Action to export the validator information to a file.
exportValidatorImpl validator costModel network stake outputFile printHash printStats =
  do
    ValidatorInfo{..} <- liftEither =<< asksEra \era -> buildValidatorImpl validator era costModel network stake
    maybeWriteTextEnvelope outputFile viScript
    doWithCardanoEra $ liftIO
      $ do
        hPutStrLn stderr $ T.unpack $ serialiseAddress viAddress
        when printHash
          $ do
            hPutStrLn stderr ""
            hPutStrLn stderr $ "Validator hash: " <> show viHash
        when printStats
          $ do
            hPutStrLn stderr ""
            hPutStrLn stderr $ "Validator size: " <> show viSize
            hPutStrLn stderr $ "Bare-validator cost: " <> show viCost


-- | Export to a file the validator information about a Marlowe contract.
exportValidator :: (MonadError CliError m, MonadReader (CliEnv era00) m)
                => MonadIO m
                => MarloweParams          -- ^ The Marlowe contract parameters.
                -> CostModelParams        -- ^ The cost model parameters.
                -> NetworkId              -- ^ The network ID.
                -> StakeAddressReference  -- ^ The stake address.
                -> Maybe FilePath         -- ^ The output JSON file for the validator information.
                -> Bool                   -- ^ Whether to print the validator hash.
                -> Bool                   -- ^ Whether to print statistics about the validator.
                -> m ()                   -- ^ Action to export the validator information to a file.
exportValidator = exportValidatorImpl . validatorScript . smallUntypedValidator


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
buildDatum :: Contract   -- ^ The contract.
           -> State      -- ^ The contract's state.
           -> DatumInfo  -- ^ Information about the transaction datum.
buildDatum marloweContract marloweState =
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
            => FilePath        -- ^ The JSON file containing the contract.
            -> FilePath        -- ^ The JSON file containing the contract's state.
            -> Maybe FilePath  -- ^ The output JSON file for the datum information.
            -> Bool            -- ^ Whether to print statistics about the datum.
            -> m ()            -- ^ Action to export the datum information to a file.
exportDatum contractFile stateFile outputFile printStats =
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

rolePayoutScript' :: CurrencySymbol -> Validator
rolePayoutScript' = validatorScript . rolePayoutScript

-- | Compute the role address of a Marlowe contract.
buildRoleAddress :: CurrencySymbol         -- ^ The currency symbol for Marlowe contract roles.
                 -> ScriptDataSupportedInEra era
                 -> NetworkId              -- ^ The network ID.
                 -> StakeAddressReference  -- ^ The stake address.
                 -> AddressInEra era       -- ^ The script address.
buildRoleAddress currencySymbol = buildAddressImpl (rolePayoutScript' currencySymbol)


-- | Print the role address of a Marlowe contract.
exportRoleAddress :: (MonadIO m, MonadReader (CliEnv era00) m)
                  => CurrencySymbol         -- ^ The currency symbol for Marlowe contract roles.
                  -> NetworkId              -- ^ The network ID.
                  -> StakeAddressReference  -- ^ The stake address.
                  -> m ()                   -- ^ Action to print the script address.
exportRoleAddress = exportAddressImpl . rolePayoutScript'


-- | Build the role validator for a Marlowe contract.
buildRoleValidator :: CurrencySymbol                       -- ^ The currency symbol for Marlowe contract roles.
                   -> ScriptDataSupportedInEra era         -- ^ The era to build the role validator in
                   -> CostModelParams                      -- ^ The cost model parameters.
                   -> NetworkId                            -- ^ The network ID.
                   -> StakeAddressReference                -- ^ The stake address.
                   -> Either CliError (ValidatorInfo era)  -- ^ The validator information, or an error message.
buildRoleValidator currencySymbol = buildValidatorImpl (rolePayoutScript' currencySymbol)


-- | Export to a file the role validator information about a Marlowe contract.
exportRoleValidator :: (MonadError CliError m, MonadReader (CliEnv era00) m)
                => MonadIO m
                => CurrencySymbol         -- ^ The currency symbol for Marlowe contract roles.
                -> CostModelParams        -- ^ The cost model parameters.
                -> NetworkId              -- ^ The network ID.
                -> StakeAddressReference  -- ^ The stake address.
                -> Maybe FilePath         -- ^ The output JSON file for the validator information.
                -> Bool                   -- ^ Whether to print the validator hash.
                -> Bool                   -- ^ Whether to print statistics about the validator.
                -> m ()                   -- ^ Action to export the validator information to a file.
exportRoleValidator = exportValidatorImpl . rolePayoutScript'


-- | Build the role datum information about a Marlowe transaction.
buildRoleDatum :: TokenName  -- ^ The role name.
               -> DatumInfo  -- ^ Information about the transaction datum.
buildRoleDatum = buildDatumImpl . PlutusTx.toBuiltinData


-- | Export to a file the role datum information about a Marlowe transaction.
exportRoleDatum :: MonadError CliError m
                => MonadIO m
                => TokenName       -- ^ The role name.
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
