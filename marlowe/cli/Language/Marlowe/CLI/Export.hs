
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Language.Marlowe.CLI.Export (
  exportMarlowe
, exportAddress
, exportValidator
, exportDatum
, exportRedeemer
, buildMarlowe
, buildAddress
, buildValidator
, buildDatum
, buildRedeemer
) where


import           Cardano.Api                     (AddressInEra, AlonzoEra, IsShelleyBasedEra, NetworkId,
                                                  PaymentCredential (..), ScriptDataJsonSchema (..), SlotNo (..),
                                                  StakeAddressReference (..), hashScript, makeShelleyAddressInEra,
                                                  scriptDataToJson, serialiseAddress, writeFileTextEnvelope)
import           Cardano.Api.Shelley             (fromPlutusData)
import           Codec.Serialise                 (serialise)
import           Control.Monad                   (void, when)
import           Control.Monad.Except            (MonadError, MonadIO, liftEither, liftIO)
import           Data.Aeson                      (FromJSON, eitherDecodeFileStrict)
import           Data.Aeson.Encode.Pretty        (encodePretty)
import           Data.Bifunctor                  (first)
import           Language.Marlowe.CLI.Types      (CliError (..), DatumInfo (..), MarloweInfo (..), RedeemerInfo (..),
                                                  ValidatorInfo (..))
import           Language.Marlowe.Scripts        (MarloweInput, typedValidator1)
import           Language.Marlowe.Semantics      (MarloweData (..), MarloweParams)
import           Language.Marlowe.SemanticsTypes (Contract (..), Input, State (..))
import           Ledger.Scripts                  (datumHash, toCardanoApiScript)
import           Ledger.Typed.Scripts            (validatorHash, validatorScript)
import           Plutus.V1.Ledger.Api            (CostModelParams, Datum (..), Redeemer (..), VerboseMode (..),
                                                  evaluateScriptCounting, getValidator)
import           PlutusTx                        (builtinDataToData, toBuiltinData)
import           System.IO                       (hPutStrLn, stderr)

import qualified Data.ByteString.Lazy            as LBS (toStrict, writeFile)
import qualified Data.ByteString.Short           as SBS (length, toShort)
import qualified Data.Text                       as T (unpack)


buildMarlowe :: IsShelleyBasedEra era
             => MarloweParams
             -> CostModelParams
             -> NetworkId
             -> StakeAddressReference
             -> Contract
             -> State
             -> [Input]
             -> SlotNo
             -> SlotNo
             -> Either CliError (MarloweInfo era)
buildMarlowe marloweParams costModel network stake contract state inputs minimumSlot maximumSlot =
  do
    validatorInfo <- buildValidator marloweParams costModel network stake
    let
      datumInfo     = buildDatum contract state
      redeemerInfo  = buildRedeemer inputs minimumSlot maximumSlot
    pure MarloweInfo{..}


exportMarlowe :: MonadError CliError m
              => MonadIO m
              => MarloweParams
              -> CostModelParams
              -> NetworkId
              -> StakeAddressReference
              -> FilePath
              -> FilePath
              -> Maybe FilePath
              -> SlotNo
              -> SlotNo
              -> FilePath
              -> Bool
              -> m ()
exportMarlowe marloweParams costModel network stake contractFile stateFile inputsFile minimumSlot maximumSlot outputFile printStats =
  do
    contract <- decodeFileStrict contractFile
    state    <- decodeFileStrict stateFile
    inputs   <- maybe (pure []) decodeFileStrict inputsFile
    marloweInfo@MarloweInfo{..} <-
      liftEither
        $ buildMarlowe
            marloweParams costModel network stake
            contract state
            inputs minimumSlot maximumSlot
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
            hPutStrLn stderr $ "Validator cost: " ++ show viCost
            hPutStrLn stderr $ "Validator size: " ++ show viSize
            hPutStrLn stderr $ "Datum size: " ++ show diSize
            hPutStrLn stderr $ "Redeemer size: " ++ show riSize
            hPutStrLn stderr $ "Total size: " ++ show (viSize + diSize + riSize)


buildAddress :: IsShelleyBasedEra era
             => MarloweParams
             -> NetworkId
             -> StakeAddressReference
             -> AddressInEra era
buildAddress marloweParams network stake =
  let
    viValidator = typedValidator1 marloweParams
    script = getValidator . validatorScript $ viValidator
    viScript = toCardanoApiScript script
  in
    makeShelleyAddressInEra
      network
      (PaymentCredentialByScript $ hashScript viScript)
      stake


exportAddress :: MonadIO m
              => MarloweParams
              -> NetworkId
              -> StakeAddressReference
              -> m ()
exportAddress marloweParams network stake =
  let
    address = buildAddress marloweParams network stake
  in
    liftIO
      . putStrLn
      . T.unpack
      $ serialiseAddress (address :: AddressInEra AlonzoEra) -- FIXME: Generalize eras.


buildValidator :: IsShelleyBasedEra era
               => MarloweParams
               -> CostModelParams
               -> NetworkId
               -> StakeAddressReference
               -> Either CliError (ValidatorInfo era)
buildValidator marloweParams costModel network stake =
  let
    viValidator = typedValidator1 marloweParams
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


exportValidator :: MonadError CliError m
                => MonadIO m
                => MarloweParams
                -> CostModelParams
                -> NetworkId
                -> StakeAddressReference
                -> FilePath
                -> Bool
                -> Bool
                -> m ()
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


buildDatum :: Contract
           -> State
           -> DatumInfo
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


exportDatum :: MonadError CliError m
            => MonadIO m
            => FilePath
            -> FilePath
            -> FilePath
            -> Bool
            -> m ()
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


buildRedeemer :: [Input]
              -> SlotNo
              -> SlotNo
              -> RedeemerInfo
buildRedeemer inputs (SlotNo minimumSlot) (SlotNo maximumSlot) =
  let
    input = ((fromIntegral minimumSlot, fromIntegral maximumSlot), inputs)
    marloweRedeemer = PlutusTx.toBuiltinData (input :: MarloweInput)
    riRedeemer = Redeemer marloweRedeemer
    riBytes = SBS.toShort . LBS.toStrict . serialise $ riRedeemer
    riJson =
      scriptDataToJson ScriptDataJsonDetailedSchema
        . fromPlutusData
        $ PlutusTx.builtinDataToData marloweRedeemer
    riSize = SBS.length riBytes
  in
    RedeemerInfo{..}


exportRedeemer :: MonadError CliError m
               => MonadIO m
               => Maybe FilePath
               -> SlotNo
               -> SlotNo
               -> FilePath
               -> Bool
               -> m ()
exportRedeemer inputsFile minimumSlot maximumSlot outputFile printStats =
  do
    inputs <- maybe (pure []) decodeFileStrict inputsFile
    let
      RedeemerInfo{..} = buildRedeemer inputs minimumSlot maximumSlot
    liftIO
      $ do
        LBS.writeFile outputFile
          $ encodePretty riJson
        when printStats
          $ do
            hPutStrLn stderr ""
            hPutStrLn stderr $ "Redeemer size: " ++ show riSize


decodeFileStrict :: MonadError CliError m
                 => MonadIO m
                 => FromJSON a
                 => FilePath
                 -> m a
decodeFileStrict filePath =
  do
    result <- liftIO $ eitherDecodeFileStrict filePath
    liftEither $ first CliError result
