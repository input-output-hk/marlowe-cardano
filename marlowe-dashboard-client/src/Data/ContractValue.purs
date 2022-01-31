module Data.ContractValue
  ( ContractValue
  , ContractValueError(..)
  , dual
  , fromBigInt
  , fromString
  , validator
  , toString
  , toBigInt
  ) where

import Prologue

import Data.Argonaut
  ( class DecodeJson
  , class EncodeJson
  , JsonDecodeError(..)
  , decodeJson
  )
import Data.Array (replicate)
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
import Data.String (Pattern(..), null, split)
import Data.String as String
import Data.String.CodeUnits (dropRight, fromCharArray)
import Data.Validation.Semigroup (V(..))
import Polyform (Validator)
import Polyform.Dual as Dual
import Polyform.Validator (liftFnV)
import Polyform.Validator.Dual (Dual)

data ContractValueError
  = Empty
  | Negative
  | Invalid

derive instance genericContractValueError :: Generic ContractValueError _
derive instance eqContractValueError :: Eq ContractValueError
derive instance ordContractValueError :: Ord ContractValueError

instance semigroupContractValueError :: Semigroup ContractValueError where
  append Empty _ = Empty
  append _ Empty = Empty
  append Negative _ = Negative
  append _ Negative = Negative
  append Invalid Invalid = Invalid

instance boundedContractValueError :: Bounded ContractValueError where
  bottom = genericBottom
  top = genericTop

instance enumContractValueError :: Enum ContractValueError where
  succ = genericSucc
  pred = genericPred

instance boundedEnumContractValueError :: BoundedEnum ContractValueError where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

instance showContractValueError :: Show ContractValueError where
  show = genericShow

newtype ContractValue = ContractValue BigInt

derive instance Eq ContractValue
derive instance Ord ContractValue
derive newtype instance Show ContractValue
derive newtype instance EncodeJson ContractValue

instance DecodeJson ContractValue where
  decodeJson =
    lmap (const $ TypeMismatch "ContractValue") <<< fromString
      <=< decodeJson

fromString :: String -> Either ContractValueError ContractValue
fromString s
  | null s = Left Empty
  | otherwise = case split (Pattern ".") s of
      [ s' ] -> case BigInt.fromString s' of
        Nothing -> Left Invalid
        Just i -> fromBigInt $ i * BigInt.fromInt 1000000
      [ whole, fractional ] ->
        case BigInt.fromString whole of
          Nothing -> Left Invalid
          Just whole' -> fromFractional whole' fractional
      _ -> Left Invalid
  | otherwise = Left Invalid

fromFractional :: BigInt -> String -> Either ContractValueError ContractValue
fromFractional whole fractional = case BigInt.fromString normalized of
  Nothing -> Left Invalid
  Just fractional' -> Right $ ContractValue
    (whole * BigInt.fromInt 1000000 + fractional')
  where
  normalized = case compare (String.length fractional) 6 of
    GT -> dropRight (6 - String.length fractional) fractional
    LT -> pad (String.length fractional - 6) fractional
    EQ -> fractional
  pad num value = value <> fromCharArray (replicate num '0')

fromBigInt :: BigInt -> Either ContractValueError ContractValue
fromBigInt i
  | i < zero = Left Negative
  | otherwise = Right $ ContractValue i

toString :: ContractValue -> String
toString (ContractValue i) =
  BigInt.toString whole <> "." <> normalized (BigInt.toString fractional)
  where
  whole = i / BigInt.fromInt 1000000
  fractional = i `mod` BigInt.fromInt 1000000
  normalized s = fromCharArray (replicate (String.length s - 6) '0') <> s

toBigInt :: ContractValue -> BigInt
toBigInt (ContractValue i) = i

-------------------------------------------------------------------------------
-- Polyform adapters
-------------------------------------------------------------------------------

validator
  :: forall m
   . Applicative m
  => Validator m ContractValueError String ContractValue
validator = liftFnV \s -> V $ fromString s

dual
  :: forall m
   . Applicative m
  => Dual m ContractValueError String ContractValue
dual = Dual.dual validator (pure <<< toString)
