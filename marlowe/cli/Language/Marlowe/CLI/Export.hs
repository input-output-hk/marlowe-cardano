
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns  #-} -- FIXME: Remove this after error handling is implemented.


module Language.Marlowe.CLI.Export (
  exportValidator
, exportDatum
, exportRedeemer
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
import           Language.Marlowe.CLI.Types      (DatumInfo (..), RedeemerInfo (..), ValidatorInfo (..))
import           Language.Marlowe.Client         (defaultMarloweParams)
import           Language.Marlowe.Scripts        (typedValidator1)
import           Language.Marlowe.Semantics      (MarloweData (..))
import           Language.Marlowe.SemanticsTypes (Contract (..), Input, Party (..), State (..), Token (..))
import           Ledger.Scripts                  (datumHash, toCardanoApiScript)
import           Ledger.Typed.Scripts            (validatorHash, validatorScript)
import           Plutus.V1.Ledger.Api            (CostModelParams, Datum (..), PubKeyHash, Redeemer (..),
                                                  VerboseMode (..), adaSymbol, adaToken, defaultCostModelParams,
                                                  evaluateScriptCounting, getValidator)
import           Plutus.V1.Ledger.Slot           (Slot (..))
import           PlutusTx                        (builtinDataToData, toBuiltinData)

import qualified Data.ByteString.Lazy            as LBS (toStrict, writeFile)
import qualified Data.ByteString.Short           as SBS (length, toShort)
import qualified Data.Text                       as T (unpack)
import qualified PlutusTx.AssocMap               as AM (empty, singleton)


buildValidator :: IsShelleyBasedEra era
               => CostModelParams
               -> NetworkId
               -> StakeAddressReference
               -> ValidatorInfo era
buildValidator costModel network stake =
  let
    viValidator = typedValidator1 defaultMarloweParams
    viScript = getValidator . validatorScript $ viValidator
    bytes = SBS.toShort . LBS.toStrict . serialise $ viScript
    viHash = validatorHash viValidator
    viAddress =
      makeShelleyAddressInEra
        network
        (PaymentCredentialByScript . hashScript $ toCardanoApiScript viScript)
        stake
    viSize = SBS.length bytes
    (_, Right viCost) = evaluateScriptCounting Verbose costModel bytes [] -- FIXME: Implement error handling.
  in
    ValidatorInfo{..}


exportValidator :: NetworkId
                -> StakeAddressReference
                -> FilePath
                -> Bool
                -> Bool
                -> Bool
                -> IO ()
exportValidator network stake validatorFile printAddress printHash printStats =
  do
    let
      Just costModel = defaultCostModelParams -- FIXME: Implement error handling.
      ValidatorInfo{..} = buildValidator costModel network stake :: ValidatorInfo AlonzoEra
    void
      . writeFileTextEnvelope validatorFile Nothing
      $ toCardanoApiScript viScript
    when printHash
      $ do
        putStrLn ""
        putStrLn $ "Validator hash: " ++ show viHash
    when printAddress
      $ do
        putStrLn ""
        putStrLn $ "Validator address: " ++ T.unpack (serialiseAddress viAddress)
    when printStats
      $ do
        putStrLn ""
        putStrLn $ "Validator size: " ++ show viSize
        putStrLn $ "Validator cost: " ++ show viCost


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
    bytes = SBS.toShort . LBS.toStrict . serialise $ diDatum
    diJson =
      scriptDataToJson ScriptDataJsonDetailedSchema
        . fromPlutusData
        . PlutusTx.builtinDataToData
        $ PlutusTx.toBuiltinData marloweData
    diHash = datumHash diDatum
    diSize = SBS.length bytes
  in
    DatumInfo{..}


exportDatum :: PubKeyHash
            -> Lovelace
            -> SlotNo
            -> FilePath
            -> Bool
            -> Bool
            -> IO ()
exportDatum accountHash accountLovelace minimumSlot datumFile printHash printStats =
  do
    let
      DatumInfo{..} = buildDatum Close accountHash accountLovelace minimumSlot
    LBS.writeFile datumFile
      $ encodePretty diJson
    when printHash
      $ do
        putStrLn ""
        putStrLn $ "Datum hash: " ++ show diHash
    when printStats
      $ do
        putStrLn ""
        putStrLn $ "Datum size: " ++ show diSize


buildRedeemer :: SlotNo
              -> SlotNo
              -> RedeemerInfo
buildRedeemer (SlotNo minimumSlot) (SlotNo maximumSlot) =
  let
    inputs :: ((Integer, Integer), [Input])
    inputs = ((fromIntegral minimumSlot, fromIntegral maximumSlot), [])
    riRedeemer = Redeemer . PlutusTx.toBuiltinData $ inputs
    bytes = SBS.toShort . LBS.toStrict . serialise $ riRedeemer
    riJson =
      scriptDataToJson ScriptDataJsonDetailedSchema
        . fromPlutusData
        . PlutusTx.builtinDataToData
        $ PlutusTx.toBuiltinData inputs
    riSize = SBS.length bytes
  in
    RedeemerInfo{..}


exportRedeemer :: SlotNo
               -> SlotNo
               -> FilePath
               -> Bool
               -> IO ()
exportRedeemer minimumSlot maximumSlot redeemerFile printStats =
  do
    let
      RedeemerInfo{..} = buildRedeemer minimumSlot maximumSlot
    LBS.writeFile redeemerFile
      $ encodePretty riJson
    when printStats
      $ do
        putStrLn ""
        putStrLn $ "Redeemer size: " ++ show riSize


{-

exportMarlowe = do
    let marloweValidator = typedValidator1 defaultMarloweParams
    let marloweScript = getValidator . Scripts.validatorScript $ marloweValidator
    let marloweScriptSBS = (SBS.toShort . LBS.toStrict . serialise) marloweScript
    let Right eeee = Base16.decode "d7604c51452bf9c135d63c686ba306d268fcae8494c877e12c44c657"
    let ownPubKey = PubKeyHash (toBuiltin eeee)
    let contract = Close
    let md = MarloweData {
            marloweState = State
                { accounts = AM.singleton (PK ownPubKey, Token adaSymbol adaToken) 3000000
                , choices  = AM.empty
                , boundValues = AM.empty
                , minSlot = 10 },
            marloweContract = contract
            }
    let datum = Datum $ PlutusTx.toBuiltinData md
    let slotRange = (1000, 43000000)
    let inputs = (slotRange, []) :: ((Integer, Integer), [Input])
    let redeemer = Redeemer $ PlutusTx.toBuiltinData inputs
    -- putStrLn $ "Redeemer hash: " <> show (redeemerHash redeemer)
    let aa = deserialiseAddress (AsAddress AsShelleyAddr) "addr_test1qrtkqnz3g54lnsf46c7xs6arqmfx3l9wsj2vsalp93zvv4c037fu3vhtk8t6gluhuq8kfdyzswxr0g83fqlqgv79mrpqe4rge9"
    let asdf :: AddressInEra (AlonzoEra)
        asdf = makeShelleyAddressInEra
                       (Testnet (NetworkMagic 1097911063))
                       (PaymentCredentialByScript $ hashScript (toCardanoApiScript marloweScript))
                       NoStakeAddress
    putStrLn "Marlowe Validator CBOX Hex:"
    print (Base16.encode (serialiseToCBOR $ toCardanoApiScript marloweScript))
    putStrLn $ "Default Marlowe validator hash: " <> show (Scripts.validatorHash marloweValidator)
    putStrLn $ "Default Marlowe validator size: " <> show (SBS.length marloweScriptSBS)
    putStrLn $ "Datum hash: " <> show (datumHash datum)
    print aa
    print (serialiseAddress asdf)
    putStrLn "==== Data ===="
    let aaaa = encode $ scriptDataToJson ScriptDataJsonDetailedSchema (fromPlutusData (PlutusTx.builtinDataToData (PlutusTx.toBuiltinData md)))
    putStrLn (BSC.unpack aaaa)
    putStrLn "==== Redeemer ===="
    let redeemerJson = encode $ scriptDataToJson ScriptDataJsonDetailedSchema (fromPlutusData (PlutusTx.builtinDataToData (PlutusTx.toBuiltinData inputs)))
    putStrLn (BSC.unpack redeemerJson)
    -- print (Base16.encode (serialiseToCBOR aaaa))
    let Just dcmp = defaultCostModelParams
    let result = evaluateScriptCounting Verbose dcmp marloweScriptSBS []
    print result

-}
