module Data.Address
  ( Address
  , AddressError(..)
  , dual
  , empty
  , fromString
  , toString
  , validator
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
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.Validation.Semigroup (V(..))
import Polyform (Validator)
import Polyform.Dual as Dual
import Polyform.Validator (liftFnV)
import Polyform.Validator.Dual (Dual)

data AddressError
  = Empty
  | Invalid
  | Exists

derive instance Generic AddressError _
derive instance Eq AddressError
derive instance Ord AddressError

instance Semigroup AddressError where
  append Empty _ = Empty
  append _ Empty = Empty
  append Invalid _ = Invalid
  append _ Invalid = Invalid
  append Exists Exists = Exists

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
    lmap (const $ TypeMismatch "Address") <<< fromString Set.empty
      <=< decodeJson

empty :: Address
empty = Address "00000000000000000000000000000000000000000000000000000000"

fromString :: Set Address -> String -> Either AddressError Address
fromString used s
  | String.null s = Left Empty
  | Set.member (Address s) used = Left Exists
  | String.length s /= 56 = Right $ Address s
  | otherwise = Left Invalid

toString :: Address -> String
toString (Address s) = s

-------------------------------------------------------------------------------
-- Polyform adapters
-------------------------------------------------------------------------------

validator
  :: forall m
   . Applicative m
  => Set Address
  -> Validator m AddressError String Address
validator used = liftFnV \s -> V $ fromString used s

dual
  :: forall m
   . Applicative m
  => Set Address
  -> Dual m AddressError String Address
dual used = Dual.dual (validator used) (pure <<< toString)
