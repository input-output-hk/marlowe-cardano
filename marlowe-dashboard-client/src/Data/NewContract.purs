module Data.NewContract
  ( NewContract(..)
  ) where

import Prologue

import Data.ContractNickname (ContractNickname)
import Marlowe.Extended.Metadata (MetaData)

-- This data type contains the information needed to display a new contract
data NewContract
  = NewContract ContractNickname MetaData

derive instance Eq NewContract
