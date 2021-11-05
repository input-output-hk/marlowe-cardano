{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
module Main(main) where

import           Data.Aeson                            (decode, encode)
import           Language.Marlowe.Analysis.FSSemantics
import           Language.Marlowe.Client
import           Language.Marlowe.Deserialisation      (byteStringToInt, byteStringToList)
import           Language.Marlowe.Scripts              (MarloweInput, mkMarloweStateMachineTransition, rolePayoutScript,
                                                        typedValidator)
import           Language.Marlowe.Semantics
import           Language.Marlowe.SemanticsTypes
import           Language.Marlowe.Serialisation        (intToByteString, listToByteString)
import           Language.Marlowe.Util
import           Ledger                                (Datum (..), Redeemer (..), Slot (..), pubKeyHash, validatorHash)
import           Ledger.Ada                            (lovelaceValueOf)
import           Ledger.Constraints.TxConstraints      (TxConstraints)
import           Ledger.Scripts                        (dataHash, datumHash, redeemerHash)
import qualified Ledger.Typed.Scripts                  as Scripts
import qualified Ledger.Value                          as Val
import qualified Plutus.Contract.StateMachine          as SM
import           Plutus.Contract.Test                  hiding ((.&&.))
import qualified Plutus.Contract.Test                  as T
import           Plutus.Contract.Types                 (_observableState)
import qualified Plutus.Trace.Emulator                 as Trace
import           Plutus.Trace.Emulator.Types           (instContractState)
import qualified PlutusTx
import qualified PlutusTx.AssocMap                     as AssocMap
import           PlutusTx.Builtins                     (emptyByteString)
import           PlutusTx.Lattice
import qualified PlutusTx.Prelude                      as P


main :: IO ()
main = do
    putStrLn "Marlowe Command Line v0.1"
    putStrLn $ "Default Marlowe validator hash: " <> show (Scripts.validatorHash $ typedValidator defaultMarloweParams)
    let contract = Close
    let md = MarloweData {
            marloweState = State
                { accounts = AssocMap.empty
                , choices  = AssocMap.empty
                , boundValues = AssocMap.empty
                , minSlot = 10 },
            marloweContract = contract
            }
    let datum = Datum $ PlutusTx.toBuiltinData md
    let redeemer = Redeemer $ PlutusTx.toBuiltinData ([] :: [Input])
    putStrLn $ "Datum hash: " <> show (datumHash datum)
    putStrLn $ "Redeemer hash: " <> show (redeemerHash redeemer)
