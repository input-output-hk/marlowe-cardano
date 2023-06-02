{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
module Language.Marlowe.CLI.Test.Contract.ContractNickname
  where

import Data.Aeson (FromJSON, ToJSON)
import Data.String (IsString)
import GHC.Exts (IsString(fromString))
import GHC.Generics (Generic)

newtype ContractNickname = ContractNickname String
    deriving stock (Eq, Ord, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)
instance IsString ContractNickname where fromString = ContractNickname
