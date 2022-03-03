module Data.NewContract
  ( NewContract(..)
  , getContractNickname
  ) where

import Prologue

import Data.ContractNickname (ContractNickname)
import Data.UUID.Argonaut (UUID)
import Marlowe.Extended.Metadata (MetaData)

-- This data type contains the information needed to display a new contract
data NewContract
  = NewContract UUID ContractNickname MetaData

derive instance Eq NewContract

getContractNickname :: NewContract -> ContractNickname
getContractNickname (NewContract _ contractNickname _) = contractNickname
