module Data.Address
  ( Address
  , AddressError(..)
  , empty
  , fromString
  , fromPubKeyHash
  , toPubKeyHash
  , toString
  ) where

import Prologue

import Data.Argonaut
  ( class DecodeJson
  , class EncodeJson
  , JsonDecodeError(..)
  , decodeJson
  )
import Data.Bifunctor (lmap)
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum (class BoundedEnum, class Enum)
import Data.Enum.Generic
  ( genericCardinality
  , genericFromEnum
  , genericPred
  , genericSucc
  , genericToEnum
  )
import Data.Generic.Rep (class Generic)
import Data.PubKeyHash (PubKeyHash)
import Data.PubKeyHash as PKH
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

-- TODO use bech-32 addresses (SCP-3145)
newtype Address = Address String

derive instance Eq Address
derive instance Ord Address
derive newtype instance Show Address
derive newtype instance EncodeJson Address

instance DecodeJson Address where
  decodeJson =
    lmap (const $ TypeMismatch "Address") <<< fromString
      <=< decodeJson

empty :: Address
empty = Address "00000000000000000000000000000000000000000000000000000000"

-- TODO right now Addresses and pub key hashes are the same... this is
-- obviously not what we want. This should bech32 encode it and somehow combine
-- a staking key hash and payment key hash...
fromPubKeyHash :: PubKeyHash -> Address
fromPubKeyHash = Address <<< PKH.toString

-- TODO right now Addresses and pub key hashes are the same... this is
-- obviously not what we want. This should bech32 encode it and somehow combine
-- a staking key hash and payment key hash...
toPubKeyHash :: Address -> PubKeyHash
toPubKeyHash = PKH.fromString <<< toString

fromString :: String -> Either AddressError Address
fromString s
  | String.null s = Left Empty
  | String.length s == 56 = Right $ Address s
  | otherwise = Left Invalid

toString :: Address -> String
toString (Address s) = s
