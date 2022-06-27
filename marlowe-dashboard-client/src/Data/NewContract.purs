module Data.NewContract
  ( NewContract(..)
  , getContractNickname
  , getContract
  , _newContractError
  ) where

import Prologue

import Data.ContractNickname (ContractNickname)
import Data.Lens (Lens', lens)
import Data.UUID.Argonaut (UUID)
import Language.Marlowe.Client (MarloweError)
import Language.Marlowe.Core.V1.Semantics.Types (Contract)
import Language.Marlowe.Extended.V1.Metadata (MetaData)

-- This data type contains the information needed to display a new contract
data NewContract = NewContract
  UUID
  ContractNickname
  MetaData
  (Maybe MarloweError)
  Contract

derive instance Eq NewContract

getContractNickname :: NewContract -> ContractNickname
getContractNickname (NewContract _ contractNickname _ _ _) = contractNickname

getContract :: NewContract -> Contract
getContract (NewContract _ _ _ _ contract) = contract

_newContractError :: Lens' NewContract (Maybe MarloweError)
_newContractError = lens get set
  where
  get (NewContract _ _ _ e _) = e
  set (NewContract reqId nickname metadata _ contract) e =
    NewContract reqId nickname metadata e contract
