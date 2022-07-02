module Marlowe.Run.Contract.V1.Types where

import Prelude

import Data.Address (Address)
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Language.Marlowe.Core.V1.Semantics.Types
  ( CurrencySymbol
  , Token(..)
  , TokenName
  )
import Type.Proxy (Proxy(..))

newtype RoleToken = RoleToken
  { currencySymbol :: CurrencySymbol
  , tokenName :: TokenName
  , utxoAddress :: Address
  }

derive instance Generic RoleToken _
derive instance Newtype RoleToken _
derive newtype instance Eq RoleToken
derive newtype instance Ord RoleToken
derive newtype instance Show RoleToken
derive newtype instance DecodeJson RoleToken
derive newtype instance EncodeJson RoleToken

type RoleTokenLens a = Lens' RoleToken a

_currencySymbol :: RoleTokenLens CurrencySymbol
_currencySymbol = _Newtype <<< prop (Proxy :: _ "currencySymbol")

_tokenName :: RoleTokenLens TokenName
_tokenName = _Newtype <<< prop (Proxy :: _ "tokenName")

_utxoAddress :: RoleTokenLens Address
_utxoAddress = _Newtype <<< prop (Proxy :: _ "utxoAddress")

_token :: RoleTokenLens Token
_token = lens' case _ of
  RoleToken rt -> Tuple (Token rt.currencySymbol rt.tokenName) case _ of
    Token cs tn -> RoleToken rt { currencySymbol = cs, tokenName = tn }
