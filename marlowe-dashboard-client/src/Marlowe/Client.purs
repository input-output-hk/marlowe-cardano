{-
NOTE: This module duplicates the ContractHistory Haskell type from Marlowe.Client. It would be
preferred to have a generated type by purescript-bridge but, the type relies on Data.Semigroup.First
which doesn't have an Encode/DecodeJson
-}
module Marlowe.Client where

import Prologue

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Aeson as D
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Aeson ((>$<))
import Data.Argonaut.Encode.Aeson as E
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype, unwrap)
import Marlowe.Semantics (MarloweData, MarloweParams, TransactionInput)
import Plutus.V1.Ledger.Address (Address)
import Prelude as Data
import Type.Proxy (Proxy(..))

-- This is the state of the follower contract. Its purpose is to provide us with an up-to-date
-- transaction history for a Marlowe contract running on the blockchain.
newtype ContractHistory
  = ContractHistory
  { chParams :: Tuple MarloweParams MarloweData
  , chHistory :: Array TransactionInput
  , chAddress :: Address
  }

derive instance newtypeContractHistory :: Newtype ContractHistory _

derive instance eqContractHistory :: Eq ContractHistory

derive instance genericContractHistory :: Generic ContractHistory _

instance encodeJsonContractHistory :: EncodeJson ContractHistory where
  encodeJson =
    E.encode $ unwrap
      >$< E.record
        { chParams: E.value :: _ (Tuple MarloweParams MarloweData)
        , chHistory: E.value :: _ (Array TransactionInput)
        , chAddress: E.value :: _ Address
        }

instance decodeJsonContractHistory :: DecodeJson ContractHistory where
  decodeJson =
    D.decode $ ContractHistory
      <$> D.record "ContractHistory"
        { chParams: D.value :: _ (Tuple MarloweParams MarloweData)
        , chHistory: D.value :: _ (Array TransactionInput)
        , chAddress: D.value :: _ Address
        }

_chParams :: Lens' ContractHistory (Tuple MarloweParams MarloweData)
_chParams = _Newtype <<< prop (Proxy :: _ "chParams")

_chHistory :: Lens' ContractHistory (Array TransactionInput)
_chHistory = _Newtype <<< prop (Proxy :: _ "chHistory")

_chAddress :: Lens' ContractHistory Address
_chAddress = _Newtype <<< prop (Proxy :: _ "chAddress")
