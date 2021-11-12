module Main (
  main
) where


import           Language.Marlowe.CLI (mainCLI)
import           Paths_marlowe        (version)

{-
let marloweScript = getValidator . Scripts.validatorScript $ marloweValidator
    let marloweScriptSBS = (SBS.toShort . BSL.toStrict . serialise) marloweScript
    let Right eeee = Base16.decode "d7604c51452bf9c135d63c686ba306d268fcae8494c877e12c44c657"
    let ownPubKey = PubKeyHash (toBuiltin eeee)
    let contract = Close
    let party = PK ownPubKey
    let adatoken = Token adaSymbol adaToken
    let md = MarloweData {
            marloweState = State
                { accounts = AssocMap.singleton (PK ownPubKey, Token adaSymbol adaToken) 3000000
                , choices  = AssocMap.empty
                , boundValues = AssocMap.empty
                , minSlot = 10 },
            marloweContract = contract
            }
    let contract2 = When [Case (Deposit party party ada (Constant 12000000)) Close] 42294000 Close
    let md2 = MarloweData {
            marloweState = State
                { accounts = AssocMap.singleton (PK ownPubKey, Token adaSymbol adaToken) 3000000
                , choices  = AssocMap.empty
                , boundValues = AssocMap.empty
                , minSlot = 42293000 },
            marloweContract = contract2
            }
    let datum = Datum $ PlutusTx.toBuiltinData md2
    let slotRange = (1000, 43000000)
    let inputs = (slotRange, []) :: ((Integer, Integer), [Input])
    let inputs1 = [IDeposit party party adatoken 12000000] :: [Input]
    let redeemer = Redeemer $ PlutusTx.toBuiltinData inputs
    -- putStrLn $ "Redeemer hash: " <> show (redeemerHash redeemer)
    -- let aa = deserialiseAddress (AsAddress AsShelleyAddr) "addr_test1qrtkqnz3g54lnsf46c7xs6arqmfx3l9wsj2vsalp93zvv4c037fu3vhtk8t6gluhuq8kfdyzswxr0g83fqlqgv79mrpqe4rge9"
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
    -- print aa
    print (serialiseAddress asdf)
    putStrLn "==== Data ===="
    let aaaa = encode $ scriptDataToJson ScriptDataJsonDetailedSchema (fromPlutusData (PlutusTx.builtinDataToData (PlutusTx.toBuiltinData md2)))
    putStrLn (BSC.unpack aaaa)
    putStrLn "==== Redeemer ===="
    let redeemerJson = encode $ scriptDataToJson ScriptDataJsonDetailedSchema (fromPlutusData (PlutusTx.builtinDataToData (PlutusTx.toBuiltinData inputs1)))
    putStrLn (BSC.unpack redeemerJson)
    -- print (Base16.encode (serialiseToCBOR aaaa))
    let Just dcmp = defaultCostModelParams
    let result = evaluateScriptCounting Verbose dcmp marloweScriptSBS []
    print result
     -}

main :: IO ()
main = mainCLI version
