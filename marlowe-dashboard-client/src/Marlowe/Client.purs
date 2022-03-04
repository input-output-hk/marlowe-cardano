{-
NOTE: This module duplicates the ContractHistory Haskell type from Marlowe.Client. It would be
preferred to have a generated type by purescript-bridge but, the type relies on Data.Semigroup.First
which doesn't have an Encode/DecodeJson
-}
module Marlowe.Client where

import Prologue

import Data.Lens (Lens', _2, view)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Language.Marlowe.Client (ContractHistory)
import Marlowe.Semantics
  ( Contract
  , MarloweData
  , MarloweParams
  , TransactionInput
  , _marloweContract
  )
import Plutus.V1.Ledger.Address (Address)
import Type.Proxy (Proxy(..))

_chParams :: Lens' ContractHistory (Tuple MarloweParams MarloweData)
_chParams = _Newtype <<< prop (Proxy :: _ "chParams")

_chHistory :: Lens' ContractHistory (Array TransactionInput)
_chHistory = _Newtype <<< prop (Proxy :: _ "chHistory")

_chAddress :: Lens' ContractHistory Address
_chAddress = _Newtype <<< prop (Proxy :: _ "chAddress")

getContract :: ContractHistory -> Contract
getContract = view $ _chParams <<< _2 <<< _marloweContract
