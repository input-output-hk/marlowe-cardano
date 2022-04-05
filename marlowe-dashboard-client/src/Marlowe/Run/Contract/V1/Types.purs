module Marlowe.Run.Contract.V1.Types where

import Prelude

import Data.Address (Address)
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Marlowe.Semantics (CurrencySymbol, TokenName)

newtype RoleToken = RoleToken
  { currencySymbol :: CurrencySymbol
  , tokenName :: TokenName
  , utxoAddress :: Address
  }

derive newtype instance Eq RoleToken
derive newtype instance Ord RoleToken
derive newtype instance Show RoleToken
derive newtype instance DecodeJson RoleToken
derive newtype instance EncodeJson RoleToken
