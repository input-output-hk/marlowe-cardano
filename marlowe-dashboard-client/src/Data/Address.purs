module Data.Address
  ( Address(..)
  , AddressError(..)
  , fromString
  , toString
  ) where

import Prologue

import Data.Address.Bech32 (Bech32Address)
import Data.Address.Bech32 as Bech32Address
import Data.Argonaut
  ( class DecodeJson
  , class EncodeJson
  , JsonDecodeError(..)
  , decodeJson
  , encodeJson
  )
import Data.Bifunctor (lmap)
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Either (note)
import Data.Enum (class BoundedEnum, class Enum)
import Data.Enum.Generic
  ( genericCardinality
  , genericFromEnum
  , genericPred
  , genericSucc
  , genericToEnum
  )
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String as String

data AddressError
  = Empty
  | Invalid

derive instance Generic AddressError _
derive instance Eq AddressError
derive instance Ord AddressError

instance Bounded AddressError where
  bottom = genericBottom
  top = genericTop

instance Enum AddressError where
  succ = genericSucc
  pred = genericPred

instance BoundedEnum AddressError where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

instance Show AddressError where
  show = genericShow

newtype Address = Bech32 Bech32Address

derive instance Eq Address
derive instance Ord Address
derive newtype instance Show Address

instance EncodeJson Address where
  encodeJson = encodeJson <<< toString

instance DecodeJson Address where
  decodeJson =
    lmap (const $ TypeMismatch "Address") <<< fromString
      <=< decodeJson

fromString :: String -> Either AddressError Address
fromString s
  | String.null s = Left Empty
  | otherwise = note Invalid $ Bech32 <$> Bech32Address.fromString s

toString :: Address -> String
toString (Bech32 addr) = Bech32Address.toString addr
