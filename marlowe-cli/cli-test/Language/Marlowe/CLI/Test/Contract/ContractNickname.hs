{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Marlowe.CLI.Test.Contract.ContractNickname where

import Data.Aeson (FromJSON, ToJSON)
import Data.String (IsString)
import GHC.Exts (IsString (fromString))
import GHC.Generics (Generic)

newtype ContractNickname = ContractNickname {unContractNickname :: String}
  deriving stock (Eq, Ord, Generic, Show)
  deriving newtype (FromJSON, ToJSON)
instance IsString ContractNickname where fromString = ContractNickname
