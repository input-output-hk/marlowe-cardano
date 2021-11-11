
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


import           Cardano.Api                     (AlonzoEra, IsShelleyBasedEra, Lovelace, NetworkId,
                                                  PaymentCredential (..), Quantity (..), ScriptDataJsonSchema (..),
                                                  SlotNo (..), StakeAddressReference (..), hashScript,
                                                  lovelaceToQuantity, makeShelleyAddressInEra, scriptDataToJson,
                                                  serialiseAddress, writeFileTextEnvelope)
import           Cardano.Api.Shelley             (fromPlutusData)
import           Codec.Serialise                 (serialise)
import           Control.Monad                   (void, when)
import           Data.Aeson.Encode.Pretty        (encodePretty)
import           Language.Marlowe.CLI.Types      (DatumInfo (..), MarloweInfo (..), RedeemerInfo (..),
                                                  ValidatorInfo (..))
import           Language.Marlowe.Client         (defaultMarloweParams)
import           Language.Marlowe.Scripts        (typedValidator1)
import           Language.Marlowe.Semantics      (MarloweData (..))
import           Language.Marlowe.SemanticsTypes (Contract (..), Input, Party (..), State (..), Token (..))
import           Ledger.Scripts                  (datumHash, toCardanoApiScript)
import           Ledger.Typed.Scripts            (validatorHash, validatorScript)
import           Plutus.V1.Ledger.Api            (CostModelParams, Datum (..), PubKeyHash, Redeemer (..),
                                                  VerboseMode (..), adaSymbol, adaToken, evaluateScriptCounting,
                                                  getValidator)
import           Plutus.V1.Ledger.Slot           (Slot (..))
import           PlutusTx                        (builtinDataToData, toBuiltinData)
import           System.IO                       (hPutStrLn, stderr)

import qualified Data.ByteString.Base16          as Base16 (encode)
import qualified Data.ByteString.Lazy            as LBS (toStrict, writeFile)
import qualified Data.ByteString.Short           as SBS (fromShort, length, toShort)
import qualified Data.Text                       as T (unpack)
import qualified PlutusTx.AssocMap               as AM (empty, singleton)


buildMarlowe :: IsShelleyBasedEra era
             => CostModelParams
             -> NetworkId
             -> StakeAddressReference
             -> Contract
             -> PubKeyHash
             -> Lovelace
             -> SlotNo
             -> SlotNo
             -> SlotNo
             -> MarloweInfo era
buildMarlowe costModel network stake contract accountHash accountLovelace minimumSlot' minimumSlot maximumSlot =
  let
    validatorInfo = buildValidator costModel network stake
    datumInfo     = buildDatum contract accountHash accountLovelace minimumSlot'
    redeemerInfo  = buildRedeemer minimumSlot maximumSlot
  in
    MarloweInfo{..}


exportMarlowe :: CostModelParams
              -> NetworkId
              -> StakeAddressReference
              -> Contract
              -> PubKeyHash
              -> Lovelace
              -> SlotNo
              -> SlotNo
              -> SlotNo
              -> FilePath
              -> Bool
              -> IO ()
exportMarlowe costModel network stake contract accountHash accountLovelace minimumSlot' minimumSlot maximumSlot outputFile printStats =
  do
    let
      marloweInfo@MarloweInfo{..} =
        buildMarlowe
          costModel network stake
          contract accountHash accountLovelace minimumSlot'
          minimumSlot maximumSlot
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
               => CostModelParams
               -> NetworkId
               -> StakeAddressReference
               -> ValidatorInfo era
buildValidator costModel network stake =
  let
    viValidator = typedValidator1 defaultMarloweParams
    viScript = getValidator . validatorScript $ viValidator
    viBytes = SBS.toShort . LBS.toStrict . serialise $ viScript
    viHash = validatorHash viValidator
    viAddress =
      makeShelleyAddressInEra
        network
        (PaymentCredentialByScript . hashScript $ toCardanoApiScript viScript)
        stake
    viSize = SBS.length viBytes
    (_, Right viCost) = evaluateScriptCounting Verbose costModel viBytes [] -- FIXME: Implement error handling.
  in
    ValidatorInfo{..}


exportAddress :: NetworkId
              -> StakeAddressReference
              -> IO ()
exportAddress network stake =
  do
    let
      ValidatorInfo{..} = buildValidator undefined network stake :: ValidatorInfo AlonzoEra -- FIXME: Generalize eras.
    hPutStrLn stderr . T.unpack . serialiseAddress $ viAddress


exportValidator :: CostModelParams
                -> NetworkId
                -> StakeAddressReference
                -> FilePath
                -> Bool
                -> Bool
                -> IO ()
exportValidator costModel network stake outputFile printHash printStats =
  do
    let
      ValidatorInfo{..} = buildValidator costModel network stake :: ValidatorInfo AlonzoEra -- FIXME: Generalize eras.
    void
      . writeFileTextEnvelope outputFile Nothing
      $ toCardanoApiScript viScript
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
           -> PubKeyHash
           -> Lovelace
           -> SlotNo
           -> DatumInfo
buildDatum contract accountHash accountLovelace (SlotNo minimumSlot) =
  let
    Quantity lovelace = lovelaceToQuantity accountLovelace
    marloweData =
      MarloweData
      {
        marloweState    = State
                          {
                            accounts    = AM.singleton
                                            (PK accountHash, Token adaSymbol adaToken)
                                            lovelace
                          , choices     = AM.empty
                          , boundValues = AM.empty
                          , minSlot     = Slot . toInteger $ minimumSlot
                          }
      , marloweContract = contract
      }
    diDatum = Datum . PlutusTx.toBuiltinData $ marloweData
    diBytes = SBS.toShort . LBS.toStrict . serialise $ diDatum
    diJson =
      scriptDataToJson ScriptDataJsonDetailedSchema
        . fromPlutusData
        . PlutusTx.builtinDataToData
        $ PlutusTx.toBuiltinData marloweData
    diHash = datumHash diDatum
    diSize = SBS.length diBytes
  in
    DatumInfo{..}


exportDatum :: Contract
            -> PubKeyHash
            -> Lovelace
            -> SlotNo
            -> FilePath
            -> Bool
            -> IO ()
exportDatum contract accountHash accountLovelace minimumSlot outputFile printStats =
  do
    let
      DatumInfo{..} = buildDatum contract accountHash accountLovelace minimumSlot
    LBS.writeFile outputFile
      $ encodePretty diJson
    print diHash
    when printStats
      $ do
        hPutStrLn stderr ""
        hPutStrLn stderr $ "Datum size: " ++ show diSize


buildRedeemer :: SlotNo
              -> SlotNo
              -> RedeemerInfo
buildRedeemer (SlotNo minimumSlot) (SlotNo maximumSlot) =
  let
    inputs :: ((Integer, Integer), [Input])
    inputs = ((fromIntegral minimumSlot, fromIntegral maximumSlot), [])
    riRedeemer = Redeemer . PlutusTx.toBuiltinData $ inputs
    riBytes = SBS.toShort . LBS.toStrict . serialise $ riRedeemer
    riJson =
      scriptDataToJson ScriptDataJsonDetailedSchema
        . fromPlutusData
        . PlutusTx.builtinDataToData
        $ PlutusTx.toBuiltinData inputs
    riSize = SBS.length riBytes
  in
    RedeemerInfo{..}


exportRedeemer :: SlotNo
               -> SlotNo
               -> FilePath
               -> Bool
               -> IO ()
exportRedeemer minimumSlot maximumSlot outputFile printStats =
  do
    let
      RedeemerInfo{..} = buildRedeemer minimumSlot maximumSlot
    LBS.writeFile outputFile
      $ encodePretty riJson
    print $ Base16.encode $ SBS.fromShort riBytes
    when printStats
      $ do
        hPutStrLn stderr ""
        hPutStrLn stderr $ "Redeemer size: " ++ show riSize
