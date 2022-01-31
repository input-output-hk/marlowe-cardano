module Data.ContractTimeout
  ( ContractTimeout
  , ContractTimeoutError(..)
  , dual
  , fromBigInt
  , fromString
  , validator
  , toString
  , toInt
  ) where

import Prologue

import Data.Argonaut
  ( class DecodeJson
  , class EncodeJson
  , JsonDecodeError(..)
  , decodeJson
  )
import Data.Bifunctor (lmap)
import Data.BigInt.Argonaut (BigInt)
import Data.BigInt.Argonaut as BigInt
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
import Data.Show.Generic (genericShow)
import Data.String (null)
import Data.Validation.Semigroup (V(..))
import Polyform (Validator)
import Polyform.Dual as Dual
import Polyform.Validator (liftFnV)
import Polyform.Validator.Dual (Dual)

data ContractTimeoutError
  = Empty
  | Past
  | Invalid

derive instance genericContractTimeoutError :: Generic ContractTimeoutError _
derive instance eqContractTimeoutError :: Eq ContractTimeoutError
derive instance ordContractTimeoutError :: Ord ContractTimeoutError

instance semigroupContractTimeoutError :: Semigroup ContractTimeoutError where
  append Empty _ = Empty
  append _ Empty = Empty
  append Past _ = Past
  append _ Past = Past
  append Invalid Invalid = Invalid

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

newtype ContractTimeout = ContractTimeout BigInt

derive instance Eq ContractTimeout
derive instance Ord ContractTimeout
derive newtype instance Show ContractTimeout
derive newtype instance EncodeJson ContractTimeout

instance DecodeJson ContractTimeout where
  decodeJson =
    lmap (const $ TypeMismatch "ContractTimeout") <<< fromString
      <=< decodeJson

fromString :: String -> Either ContractTimeoutError ContractTimeout
fromString s
  | null s = Left Empty
  | otherwise = case BigInt.fromString s of
      Nothing -> Left Invalid
      Just i -> fromBigInt i

fromBigInt :: BigInt -> Either ContractTimeoutError ContractTimeout
fromBigInt i
  | i < zero = Left Past
  | otherwise = Right $ ContractTimeout i

toString :: ContractTimeout -> String
toString = BigInt.toString <<< toInt

toInt :: ContractTimeout -> BigInt
toInt (ContractTimeout i) = i

-------------------------------------------------------------------------------
-- Polyform adapters
-------------------------------------------------------------------------------

validator
  :: forall m
   . Applicative m
  => Validator m ContractTimeoutError String ContractTimeout
validator = liftFnV \s -> V $ fromString s

dual
  :: forall m
   . Applicative m
  => Dual m ContractTimeoutError String ContractTimeout
dual = Dual.dual validator (pure <<< toString)
