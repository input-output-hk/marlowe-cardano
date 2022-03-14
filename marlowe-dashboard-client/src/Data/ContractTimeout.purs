module Data.ContractTimeout
  ( ContractTimeout
  , ContractTimeoutError(..)
  , fromDuration
  , fromString
  , toString
  , toDuration
  ) where

import Prologue

import Data.Argonaut.Core as Json
import Data.Argonaut.Encode.Class (class EncodeJson)
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
import Data.Int (decimal, round)
import Data.Int as Int
import Data.Newtype (un)
import Data.Show.Generic (genericShow)
import Data.String (null)
import Data.Time.Duration
  ( class Duration
  , Milliseconds(..)
  , Minutes(..)
  , convertDuration
  )

data ContractTimeoutError
  = Empty
  | Past
  | Invalid

derive instance genericContractTimeoutError :: Generic ContractTimeoutError _
derive instance eqContractTimeoutError :: Eq ContractTimeoutError
derive instance ordContractTimeoutError :: Ord ContractTimeoutError

instance boundedContractTimeoutError :: Bounded ContractTimeoutError where
  bottom = genericBottom
  top = genericTop

instance enumContractTimeoutError :: Enum ContractTimeoutError where
  succ = genericSucc
  pred = genericPred

instance boundedEnumContractTimeoutError :: BoundedEnum ContractTimeoutError where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

instance showContractTimeoutError :: Show ContractTimeoutError where
  show = genericShow

newtype ContractTimeout = ContractTimeout Milliseconds

derive instance Eq ContractTimeout
derive instance Ord ContractTimeout
derive newtype instance Show ContractTimeout

instance EncodeJson ContractTimeout where
  encodeJson (ContractTimeout (Milliseconds ms)) = Json.fromNumber ms

fromString :: String -> Either ContractTimeoutError ContractTimeout
fromString s
  | null s = Left Empty
  | otherwise = case Int.fromStringAs decimal s of
      Nothing -> Left Invalid
      Just i -> fromDuration $ Minutes $ Int.toNumber i

fromDuration
  :: forall d. Duration d => d -> Either ContractTimeoutError ContractTimeout
fromDuration ms
  | convertDuration ms < Milliseconds zero = Left Past
  | otherwise = Right $ ContractTimeout $ convertDuration ms

toString :: ContractTimeout -> String
toString = Int.toStringAs decimal <<< round <<< un Minutes <<< toDuration

toDuration :: forall d. Duration d => ContractTimeout -> d
toDuration (ContractTimeout min) = convertDuration min
