{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module Language.Marlowe.CLI(mainCLI) where

import           Codec.Serialise                       (serialise)
import           Data.Aeson                            (decode, encode)
import qualified Data.ByteString.Lazy                  as BSL
import qualified Data.ByteString.Short                 as SBS
import           Data.Version                          (Version, showVersion)
import           Language.Marlowe.Analysis.FSSemantics
import           Language.Marlowe.Client
import           Language.Marlowe.Deserialisation      (byteStringToInt, byteStringToList)
import           Language.Marlowe.Scripts

import           Language.Marlowe.Semantics
import           Language.Marlowe.SemanticsTypes
import           Language.Marlowe.Serialisation        (intToByteString, listToByteString)
import           Language.Marlowe.Util
import           Ledger                                (Datum (..), PubKeyHash (..), Redeemer (..), Slot (..),
                                                        getValidator, pubKeyHash, validatorHash)
import           Ledger.Ada                            (adaSymbol, adaToken, lovelaceValueOf)
import           Ledger.Constraints.TxConstraints      (TxConstraints)
import           Ledger.Scripts                        (dataHash, datumHash, redeemerHash, toCardanoApiScript)
import qualified Ledger.Typed.Scripts                  as Scripts
import qualified Ledger.Value                          as Val
import qualified Plutus.Contract.StateMachine          as SM
import           Plutus.Contract.Test                  hiding ((.&&.))
import qualified Plutus.Contract.Test                  as T
import           Plutus.Contract.Types                 (_observableState)
import qualified Plutus.Trace.Emulator                 as Trace
import           Plutus.Trace.Emulator.Types           (instContractState)
import           Plutus.V1.Ledger.Api
import qualified PlutusTx
import qualified PlutusTx.AssocMap                     as AssocMap
import           PlutusTx.Builtins                     (emptyByteString)
import           PlutusTx.Lattice
import qualified PlutusTx.Prelude                      as P

import           Cardano.Api
import           Cardano.Api.Shelley
import qualified Data.ByteString.Base16                as Base16

import qualified Cardano.Ledger.BaseTypes              as Shelley
import           Control.Monad.Fail                    (fail)
import           Data.Time.Clock                       (UTCTime)
import           Data.Time.Format                      (defaultTimeLocale, iso8601DateFormat, parseTimeOrError)
import           Prettyprinter                         (line, pretty)


import qualified Data.ByteString.Lazy.Char8            as BSC
import qualified Data.List.NonEmpty                    as NE
import qualified Data.Set                              as Set
import qualified Data.Text                             as Text
import qualified Data.Text.Encoding                    as Text

import qualified Data.Aeson                            as Aeson
import qualified Data.Aeson.Parser                     as Aeson.Parser

mainCLI :: Version
        -> IO ()
mainCLI version = do
    putStrLn $ "Marlowe Command Line" ++ showVersion version

    let marloweValidator = typedValidator1 defaultMarloweParams
    let marloweScript = getValidator . Scripts.validatorScript $ marloweValidator
    let marloweScriptSBS = (SBS.toShort . BSL.toStrict . serialise) marloweScript
    let Right eeee = Base16.decode "d7604c51452bf9c135d63c686ba306d268fcae8494c877e12c44c657"
    let ownPubKey = PubKeyHash (toBuiltin eeee)
    let contract = Close
    let md = MarloweData {
            marloweState = State
                { accounts = AssocMap.singleton (PK ownPubKey, Token adaSymbol adaToken) 3000000
                , choices  = AssocMap.empty
                , boundValues = AssocMap.empty
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



  where
