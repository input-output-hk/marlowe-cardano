{-
NOTE: This module duplicates the ContractHistory Haskell type from Marlowe.Client. It would be
preferred to have a generated type by purescript-bridge but, the type relies on Data.Semigroup.First
which doesn't have an Encode/DecodeJson
-}
module Marlowe.Client
  ( _chAddress
  , _chHistory
  , _chInitialData
  , _chParams
  , getContract
  , getInitialData
  , getMarloweParams
  , getTransactionInputs
  ) where

import Prologue

import Data.Lens (Lens', view)
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

_chParams :: Lens' ContractHistory MarloweParams
_chParams = _Newtype <<< prop (Proxy :: _ "chParams")

_chInitialData :: Lens' ContractHistory MarloweData
_chInitialData = _Newtype <<< prop (Proxy :: _ "chInitialData")

_chHistory :: Lens' ContractHistory (Array TransactionInput)
_chHistory = _Newtype <<< prop (Proxy :: _ "chHistory")

_chAddress :: Lens' ContractHistory Address
_chAddress = _Newtype <<< prop (Proxy :: _ "chAddress")

getContract :: ContractHistory -> Contract
getContract = view $ _chInitialData <<< _marloweContract

getTransactionInputs :: ContractHistory -> (Array TransactionInput)
getTransactionInputs = view _chHistory

getInitialData :: ContractHistory -> MarloweData
getInitialData = view _chInitialData

getMarloweParams :: ContractHistory -> MarloweParams
getMarloweParams = view _chParams
