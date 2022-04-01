module Data.NewContract
  ( NewContract(..)
  , getContractNickname
  , _newContractError
  ) where

import Prologue

import Data.ContractNickname (ContractNickname)
import Data.Lens (Lens', lens)
import Data.UUID.Argonaut (UUID)
import Language.Marlowe.Client (MarloweError)
import Marlowe.Extended.Metadata (MetaData)

-- This data type contains the information needed to display a new contract
data NewContract = NewContract UUID ContractNickname MetaData
  (Maybe MarloweError)

derive instance Eq NewContract

getContractNickname :: NewContract -> ContractNickname
getContractNickname (NewContract _ contractNickname _ _) = contractNickname

_newContractError :: Lens' NewContract (Maybe MarloweError)
_newContractError = lens get set
  where
  get (NewContract _ _ _ e) = e
  set (NewContract reqId nickname metadata _) e =
    NewContract reqId nickname metadata e
