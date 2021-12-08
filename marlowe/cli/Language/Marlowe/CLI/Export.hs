-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Export information for Marlowe contracts and transactions.
--
-----------------------------------------------------------------------------


{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


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
) where


import           Cardano.Api                     (AddressInEra, AlonzoEra, IsShelleyBasedEra, NetworkId,
                                                  PaymentCredential (..), ScriptDataJsonSchema (..),
                                                  StakeAddressReference (..), hashScript, makeShelleyAddressInEra,
                                                  scriptDataToJson, serialiseAddress, serialiseToTextEnvelope,
                                                  writeFileTextEnvelope)
import           Cardano.Api.Shelley             (fromPlutusData)
import           Codec.Serialise                 (serialise)
import           Control.Monad                   (void, when)
import           Control.Monad.Except            (MonadError, MonadIO, liftEither, liftIO)
import           Data.Aeson                      (encode)
import           Data.Aeson.Encode.Pretty        (encodePretty)
import           Language.Marlowe.CLI.IO         (decodeFileStrict)
import           Language.Marlowe.CLI.Types      (CliError (..), DatumInfo (..), MarloweInfo (..), RedeemerInfo (..),
                                                  ValidatorInfo (..))
import           Language.Marlowe.Scripts        (smallTypedValidator)
import           Language.Marlowe.Semantics      (MarloweData (..), MarloweParams)
import           Language.Marlowe.SemanticsTypes (Contract (..), Input, State (..))
import           Ledger.Scripts                  (datumHash, toCardanoApiScript)
import           Ledger.Typed.Scripts            (validatorHash, validatorScript)
import           Plutus.V1.Ledger.Api            (CostModelParams, Datum (..), Redeemer (..), VerboseMode (..),
                                                  evaluateScriptCounting, getValidator)
import           PlutusTx                        (builtinDataToData, toBuiltinData)
import           System.IO                       (hPutStrLn, stderr)

import qualified Data.ByteString.Lazy            as LBS (toStrict, writeFile)
import qualified Data.ByteString.Lazy.Char8      as LBS8 (unpack)
import qualified Data.ByteString.Short           as SBS (length, toShort)
import qualified Data.Text                       as T (unpack)


-- | Build comprehensive information about a Marlowe contract and transaction.
buildMarlowe :: IsShelleyBasedEra era
             => MarloweParams                      -- ^ The Marlowe contract parameters.
             -> CostModelParams                    -- ^ The cost model parameters.
             -> NetworkId                          -- ^ The network ID.
             -> StakeAddressReference              -- ^ The stake address.
             -> Contract                           -- ^ The contract.
             -> State                              -- ^ The contract's state.
             -> [Input]                            -- ^ The contract's input,
             -> Either CliError (MarloweInfo era)  -- ^ The contract and transaction information, or an error message.
buildMarlowe marloweParams costModel network stake contract state inputs =
  do
    validatorInfo <- buildValidator marloweParams costModel network stake
    let
      datumInfo = buildDatum contract state
      redeemerInfo = buildRedeemer inputs
    pure MarloweInfo{..}


-- | Export to a file the comprehensive information about a Marlowe contract and transaction.
exportMarlowe :: MonadError CliError m
              => MonadIO m
              => MarloweParams          -- ^ The Marlowe contract parameters.
              -> CostModelParams        -- ^ The cost model parameters.
              -> NetworkId              -- ^ The network ID.
              -> StakeAddressReference  -- ^ The stake address.
              -> FilePath               -- ^ The JSON file containing the contract.
              -> FilePath               -- ^ The JSON file containing the contract's state.
              -> [FilePath]             -- ^ The JSON files containing the contract's inputs.
              -> FilePath               -- ^ The output JSON file for Marlowe contract information.
              -> Bool                   -- ^ Whether to print statistics about the contract.
              -> m ()                   -- ^ Action to export the contract and transaction information to a file.
exportMarlowe marloweParams costModel network stake contractFile stateFile inputFiles outputFile printStats =
  do
    contract <- decodeFileStrict contractFile
    state    <- decodeFileStrict stateFile
    inputs   <- mapM decodeFileStrict inputFiles
    marloweInfo@MarloweInfo{..} <-
      liftEither
        $ buildMarlowe
            marloweParams costModel network stake
            contract state
            inputs
    let
      ValidatorInfo{..} = validatorInfo
      DatumInfo{..}     = datumInfo
      RedeemerInfo{..}  = redeemerInfo
    liftIO
      $ do
        LBS.writeFile outputFile
          $ encodePretty (marloweInfo :: MarloweInfo AlonzoEra) -- FIXME: Generalize eras.
        when printStats
          $ do
            hPutStrLn stderr ""
            hPutStrLn stderr $ "Bare-validator cost: " ++ show viCost
            hPutStrLn stderr $ "Validator size: " ++ show viSize
            hPutStrLn stderr $ "Datum size: " ++ show diSize
            hPutStrLn stderr $ "Redeemer size: " ++ show riSize
            hPutStrLn stderr $ "Total size: " ++ show (viSize + diSize + riSize)


-- | Print information about a Marlowe contract and transaction.
printMarlowe :: MonadError CliError m
              => MonadIO m
              => MarloweParams          -- ^ The Marlowe contract parameters.
              -> CostModelParams        -- ^ The cost model parameters.
              -> NetworkId              -- ^ The network ID.
              -> StakeAddressReference  -- ^ The stake address.
              -> Contract               -- ^ The contract.
              -> State                  -- ^ The contract's state.
              -> [Input]                -- ^ The contract's input,
              -> m ()                   -- ^ Action to print the contract and transaction information.
printMarlowe marloweParams costModel network stake contract state inputs =
  do
    MarloweInfo{..} <-
      liftEither
        $ buildMarlowe
            marloweParams costModel network stake
            contract state
            inputs
    let
      ValidatorInfo{..} = validatorInfo
      DatumInfo{..}     = datumInfo
      RedeemerInfo{..}  = redeemerInfo
    liftIO
      $ do
        putStrLn ""
        putStrLn $ "Contract: " ++ show contract
        putStrLn ""
        putStrLn $ "State: " ++ show state
        putStrLn ""
        putStrLn $ "Inputs: " ++ show inputs
        putStrLn ""
        putStrLn $ "Validator: " ++ LBS8.unpack (encode $ serialiseToTextEnvelope Nothing viScript)
        putStrLn ""
        putStrLn $ "Validator address: " ++ (T.unpack $ serialiseAddress (viAddress :: AddressInEra AlonzoEra)) -- FIXME: Generalize eras.
        putStrLn ""
        putStrLn $ "Validator hash: " ++ show viHash
        putStrLn ""
        putStrLn $ "Validator size: " ++ show viSize
        putStrLn ""
        putStrLn $ "Validator cost: " ++ show viCost
        putStrLn ""
        putStrLn $ "Datum:" ++ LBS8.unpack (encode diJson)
        putStrLn ""
        putStrLn $ "Datum hash: " ++ show diHash
        putStrLn ""
        putStrLn $ "Datum size: " ++ show diSize
        putStrLn ""
        putStrLn $ "Redeemer: " ++ LBS8.unpack (encode riJson)
        putStrLn ""
        putStrLn $ "Redeemer size: " ++ show riSize
        putStrLn ""
        putStrLn $ "Total size: " ++ show (viSize + diSize + riSize)


-- | Compute the address of a Marlowe contract.
buildAddress :: IsShelleyBasedEra era
             => MarloweParams          -- ^ The Marlowe contract parameters.
             -> NetworkId              -- ^ The network ID.
             -> StakeAddressReference  -- ^ The stake address.
             -> AddressInEra era       -- ^ The script address.
buildAddress marloweParams network stake =
  let
    viValidator = smallTypedValidator marloweParams
    script = getValidator . validatorScript $ viValidator
    viScript = toCardanoApiScript script
  in
    makeShelleyAddressInEra
      network
      (PaymentCredentialByScript $ hashScript viScript)
      stake


-- | Print the address of a Marlowe contract.
exportAddress :: MonadIO m
              => MarloweParams          -- ^ The Marlowe contract parameters.
              -> NetworkId              -- ^ The network ID.
              -> StakeAddressReference  -- ^ The stake address.
              -> m ()                   -- ^ Action to print the script address.
exportAddress marloweParams network stake =
  let
    address = buildAddress marloweParams network stake
  in
    liftIO
      . putStrLn
      . T.unpack
      $ serialiseAddress (address :: AddressInEra AlonzoEra) -- FIXME: Generalize eras.



-- | Build the validator information about a Marlowe contract.
buildValidator :: IsShelleyBasedEra era
               => MarloweParams                        -- ^ The Marlowe contract parameters.
               -> CostModelParams                      -- ^ The cost model parameters.
               -> NetworkId                            -- ^ The network ID.
               -> StakeAddressReference                -- ^ The stake address.
               -> Either CliError (ValidatorInfo era)  -- ^ The validator information, or an error message.
buildValidator marloweParams costModel network stake =
  let
    viValidator = smallTypedValidator marloweParams
    script = getValidator . validatorScript $ viValidator
    viScript = toCardanoApiScript script
    viBytes = SBS.toShort . LBS.toStrict . serialise $ script
    viHash = validatorHash viValidator
    viAddress =
      makeShelleyAddressInEra
        network
        (PaymentCredentialByScript $ hashScript viScript)
        stake
    viSize = SBS.length viBytes
  in
    case evaluateScriptCounting Verbose costModel viBytes [] of
      (_, Right viCost) -> Right ValidatorInfo{..}
      _                 -> Left $ CliError "Failed to evaluate cost of validator script."


-- | Export to a file the validator information about a Marlowe contract.
exportValidator :: MonadError CliError m
                => MonadIO m
                => MarloweParams          -- ^ The Marlowe contract parameters.
                -> CostModelParams        -- ^ The cost model parameters.
                -> NetworkId              -- ^ The network ID.
                -> StakeAddressReference  -- ^ The stake address.
                -> FilePath               -- ^ The output JSON file for the validator information.
                -> Bool                   -- ^ Whether to print the validator hash.
                -> Bool                   -- ^ Whether to print statistics about the validator.
                -> m ()                   -- ^ Action to export the validator information to a file.
exportValidator marloweParams costModel network stake outputFile printHash printStats =
  do
    ValidatorInfo{..} <-
      liftEither
        $ buildValidator marloweParams costModel network stake
    liftIO
      $ do
        void
          $ writeFileTextEnvelope outputFile Nothing viScript
        putStrLn . T.unpack $ serialiseAddress (viAddress :: AddressInEra AlonzoEra) -- FIXME: Generalize eras.
        when printHash
          $ do
            hPutStrLn stderr ""
            hPutStrLn stderr $ "Validator hash: " ++ show viHash
        when printStats
          $ do
            hPutStrLn stderr ""
            hPutStrLn stderr $ "Validator size: " ++ show viSize
            hPutStrLn stderr $ "Validator cost: " ++ show viCost


-- | Build the datum information about a Marlowe transaction.
buildDatum :: Contract   -- ^ The contract.
           -> State      -- ^ The contract's state.
           -> DatumInfo  -- ^ Information about the transaction datum.
buildDatum marloweContract marloweState =
  let
    marloweData = MarloweData{..}
    marloweDatum = PlutusTx.toBuiltinData marloweData
    diDatum = Datum marloweDatum
    diBytes = SBS.toShort . LBS.toStrict . serialise $ diDatum
    diJson =
      scriptDataToJson ScriptDataJsonDetailedSchema
        . fromPlutusData
        $ PlutusTx.builtinDataToData marloweDatum
    diHash = datumHash diDatum
    diSize = SBS.length diBytes
  in
    DatumInfo{..}


-- | Export to a file the datum information about a Marlowe transaction.
exportDatum :: MonadError CliError m
            => MonadIO m
            => FilePath  -- ^ The JSON file containing the contract.
            -> FilePath  -- ^ The JSON file containing the contract's state.
            -> FilePath  -- ^ The output JSON file for the datum information.
            -> Bool      -- ^ Whether to print statistics about the datum.
            -> m ()      -- ^ Action to export the datum information to a file.
exportDatum contractFile stateFile outputFile printStats =
  do
    contract <- decodeFileStrict contractFile
    state    <- decodeFileStrict stateFile
    let
      DatumInfo{..} = buildDatum contract state
    liftIO
      $ do
        LBS.writeFile outputFile
          $ encodePretty diJson
        print diHash
        when printStats
          $ do
            hPutStrLn stderr ""
            hPutStrLn stderr $ "Datum size: " ++ show diSize


-- | Build the redeemer information about a Marlowe transaction.
buildRedeemer :: [Input]       -- ^ The contract's input,
              -> RedeemerInfo  -- ^ Information about the transaction redeemer.
buildRedeemer inputs =
  let
    marloweRedeemer = PlutusTx.toBuiltinData inputs
    riRedeemer = Redeemer marloweRedeemer
    riBytes = SBS.toShort . LBS.toStrict . serialise $ riRedeemer
    riJson =
      scriptDataToJson ScriptDataJsonDetailedSchema
        . fromPlutusData
        $ PlutusTx.builtinDataToData marloweRedeemer
    riSize = SBS.length riBytes
  in
    RedeemerInfo{..}


-- | Export to a file the redeemer information about a Marlowe transaction.
exportRedeemer :: MonadError CliError m
               => MonadIO m
               => [FilePath]  -- ^ The files containing the contract's inputs.
               -> FilePath    -- ^ The output JSON file for Marlowe contract information.
               -> Bool        -- ^ Whether to print statistics on the contract.
               -> m ()        -- ^ Action to export the redeemer information to a file.
exportRedeemer inputFiles outputFile printStats =
  do
    inputs <- mapM decodeFileStrict inputFiles
    let
      RedeemerInfo{..} = buildRedeemer inputs
    liftIO
      $ do
        LBS.writeFile outputFile
          $ encodePretty riJson
        when printStats
          $ do
            hPutStrLn stderr ""
            hPutStrLn stderr $ "Redeemer size: " ++ show riSize
