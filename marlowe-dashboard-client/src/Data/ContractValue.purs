module Data.ContractValue
  ( ContractValue
  , ContractValueError(..)
  , fromBigInt
  , fromString
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
import Data.Bifunctor (lmap)
import Data.BigInt.Argonaut (BigInt)
import Data.BigInt.Argonaut as BigInt
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
import Data.Int as Int
import Data.Ord as Ring
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..))
import Data.String as String
import Data.String.Extra (leftPadTo, rightPadTo)
import Marlowe.Extended.Metadata (NumberFormat(..))

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
    lmap (const $ TypeMismatch "ContractValue") <<< fromString DefaultFormat
      <=< decodeJson

fromString :: NumberFormat -> String -> Either ContractValueError ContractValue
fromString format s
  | String.null s = Right $ ContractValue zero
  | otherwise = ContractValue <$> case format of
      DefaultFormat -> fromStringDefault s
      DecimalFormat decimals _ -> fromStringDecimal decimals s
      TimeFormat -> fromStringTime s

fromStringDefault :: String -> Either ContractValueError BigInt
fromStringDefault = note Invalid <<< BigInt.fromString

fromStringTime :: String -> Either ContractValueError BigInt
fromStringTime =
  note Invalid <<< map (mul (BigInt.fromInt 60)) <<< BigInt.fromString

fromStringDecimal :: Int -> String -> Either ContractValueError BigInt
fromStringDecimal decimals value = do
  let
    { signum, absoluteValue } =
      if String.take 1 value == "-" then
        { signum: negate one, absoluteValue: String.drop 1 value }
      else
        { signum: one, absoluteValue: value }
  Tuple wholePart fracPart <- case String.split (Pattern ".") absoluteValue of
    [ s ] -> Right $ Tuple s "0"
    [ s, t ] -> Right $ Tuple s t
    _ -> Left Invalid

  -- if zeros have been deleted from the end of the string, the fractional part will be wrong
  let normalizedFrac = String.take decimals $ rightPadTo decimals "0" fracPart
  let multiplier = BigInt.fromInt $ Int.pow 10 decimals
  whole <- note Invalid $ BigInt.fromString wholePart
  frac <-
    note Invalid $ BigInt.fromString $ String.take decimals $ normalizedFrac
  pure $ signum * multiplier * whole + frac

fromBigInt :: BigInt -> Either ContractValueError ContractValue
fromBigInt i
  | i < zero = Left Negative
  | otherwise = Right $ ContractValue i

toString :: NumberFormat -> ContractValue -> String
toString DefaultFormat (ContractValue value) = BigInt.toString value
toString TimeFormat (ContractValue value) = BigInt.toString $ value /
  BigInt.fromInt 60
toString (DecimalFormat decimals _) (ContractValue value) =
  let
    signum = Ring.signum value
    absoluteValue = Ring.abs value
    string = leftPadTo decimals "0" $ BigInt.toString absoluteValue
    len = String.length string

    { before, after: fracPart } = String.splitAt (len - decimals) string

    wholePart = case before of
      "" -> "0"
      _ -> before
    prefix = if signum < zero then "-" else ""
    suffix = if decimals > zero then "." <> fracPart else ""
  in
    prefix <> wholePart <> suffix

toBigInt :: ContractValue -> BigInt
toBigInt (ContractValue i) = i
