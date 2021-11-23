
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns  #-} -- FIXME: Remove this after error handling is implemented.


module Language.Marlowe.CLI.Export (
  exportMarlowe
, exportAddress
, exportValidator
, exportDatum
, exportRedeemer
, buildMarlowe
, buildValidator
, buildDatum
, buildRedeemer
) where


import           Cardano.Api                     (AlonzoEra, IsShelleyBasedEra, NetworkId, PaymentCredential (..),
                                                  ScriptDataJsonSchema (..), SlotNo (..), StakeAddressReference (..),
                                                  hashScript, makeShelleyAddressInEra, scriptDataToJson,
                                                  serialiseAddress, writeFileTextEnvelope)
import           Cardano.Api.Shelley             (fromPlutusData)
import           Codec.Serialise                 (serialise)
import           Control.Monad                   (void, when)
import           Data.Aeson                      (eitherDecodeFileStrict)
import           Data.Aeson.Encode.Pretty        (encodePretty)
import           Language.Marlowe.CLI.Types      (DatumInfo (..), MarloweInfo (..), RedeemerInfo (..),
                                                  ValidatorInfo (..))
import           Language.Marlowe.Scripts        (MarloweInput, marloweValidator2, typedValidator1)
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
             -> MarloweInfo era
buildMarlowe marloweParams costModel network stake contract state inputs minimumSlot maximumSlot =
  let
    validatorInfo = buildValidator marloweParams costModel network stake
    datumInfo     = buildDatum contract state
    redeemerInfo  = buildRedeemer inputs minimumSlot maximumSlot
  in
    MarloweInfo{..}


exportMarlowe :: MarloweParams
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
              -> IO ()
exportMarlowe marloweParams costModel network stake contractFile stateFile inputsFile minimumSlot maximumSlot outputFile printStats =
  do
    Right contract <- eitherDecodeFileStrict contractFile
    Right state    <- eitherDecodeFileStrict stateFile
    Right inputs   <- maybe (pure $ Right []) eitherDecodeFileStrict inputsFile
    let
      marloweInfo@MarloweInfo{..} =
        buildMarlowe
          marloweParams costModel network stake
          contract state
          inputs minimumSlot maximumSlot
      ValidatorInfo{..} = validatorInfo
      DatumInfo{..}     = datumInfo
      RedeemerInfo{..}  = redeemerInfo
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


buildValidator :: IsShelleyBasedEra era
               => MarloweParams
               -> CostModelParams
               -> NetworkId
               -> StakeAddressReference
               -> ValidatorInfo era
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
    (_, Right viCost) = evaluateScriptCounting Verbose costModel viBytes [] -- FIXME: Implement error handling.
  in
    ValidatorInfo{..}


exportAddress :: MarloweParams
              -> NetworkId
              -> StakeAddressReference
              -> IO ()
exportAddress marloweParams network stake =
  do
    let
      ValidatorInfo{..} = buildValidator marloweParams undefined network stake :: ValidatorInfo AlonzoEra -- FIXME: Generalize eras.
    putStrLn . T.unpack . serialiseAddress $ viAddress


exportValidator :: MarloweParams
                -> CostModelParams
                -> NetworkId
                -> StakeAddressReference
                -> FilePath
                -> Bool
                -> Bool
                -> IO ()
exportValidator marloweParams costModel network stake outputFile printHash printStats =
  do
    let
      ValidatorInfo{..} = buildValidator marloweParams costModel network stake :: ValidatorInfo AlonzoEra -- FIXME: Generalize eras.
    void
      $ writeFileTextEnvelope outputFile Nothing viScript
    putStrLn . T.unpack . serialiseAddress $ viAddress
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


exportDatum :: FilePath
            -> FilePath
            -> FilePath
            -> Bool
            -> IO ()
exportDatum contractFile stateFile outputFile printStats =
  do
    Right contract <- eitherDecodeFileStrict contractFile
    Right state <- eitherDecodeFileStrict stateFile
    let
      DatumInfo{..} = buildDatum contract state
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
    marloweRedeemer = PlutusTx.toBuiltinData $ inputs
    riRedeemer = Redeemer marloweRedeemer
    riBytes = SBS.toShort . LBS.toStrict . serialise $ riRedeemer
    riJson =
      scriptDataToJson ScriptDataJsonDetailedSchema
        . fromPlutusData
        $ PlutusTx.builtinDataToData marloweRedeemer
    riSize = SBS.length riBytes
  in
    RedeemerInfo{..}


exportRedeemer :: Maybe FilePath
               -> SlotNo
               -> SlotNo
               -> FilePath
               -> Bool
               -> IO ()
exportRedeemer inputsFile minimumSlot maximumSlot outputFile printStats =
  do
    Right inputs <- maybe (pure $ Right []) eitherDecodeFileStrict inputsFile
    let
      RedeemerInfo{..} = buildRedeemer inputs minimumSlot maximumSlot
    LBS.writeFile outputFile
      $ encodePretty riJson
    when printStats
      $ do
        hPutStrLn stderr ""
        hPutStrLn stderr $ "Redeemer size: " ++ show riSize
