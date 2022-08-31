module Data.ContractValue
  ( ContractValue(..)
  , ContractValueError(..)
  , currencyFromString
  , fromString
  , toString
  , _value
  , _decimals
  , _currencySymbol
  ) where

import Prologue

import Data.Argonaut (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
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
import Data.Lens (Lens', lens)
import Data.Lens.AffineTraversal (AffineTraversal', affineTraversal)
import Data.Ord as Ring
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..))
import Data.String as String
import Data.String.Extra (leftPadTo, rightPadTo)
import Language.Marlowe.Core.V1.Semantics.Types (CurrencySymbol)

data ContractValueError = Empty | Invalid

derive instance genericContractValueError :: Generic ContractValueError _
derive instance eqContractValueError :: Eq ContractValueError
derive instance ordContractValueError :: Ord ContractValueError

instance semigroupContractValueError :: Semigroup ContractValueError where
  append Empty _ = Empty
  append _ Empty = Empty
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

data ContractValue
  = Currency CurrencySymbol Int BigInt
  | Normal BigInt

derive instance Generic ContractValue _
derive instance Eq ContractValue
derive instance Ord ContractValue
instance Show ContractValue where
  show = genericShow

instance EncodeJson ContractValue where
  encodeJson a = genericEncodeJson a

fromString'
  :: (String -> Either ContractValueError BigInt)
  -> (BigInt -> ContractValue)
  -> String
  -> Either ContractValueError ContractValue
fromString' parse wrap s
  | String.null s = Right $ wrap zero
  | otherwise = wrap <$> parse s

currencyFromString
  :: CurrencySymbol -> Int -> String -> Either ContractValueError ContractValue
currencyFromString cs d = fromString' (fromStringDecimal d) $ Currency cs d

fromString :: String -> Either ContractValueError ContractValue
fromString = fromString' fromStringDefault Normal

fromStringDefault :: String -> Either ContractValueError BigInt
fromStringDefault = note Invalid <<< BigInt.fromString

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

_value :: Lens' ContractValue BigInt
_value = lens get set
  where
  get (Currency _ _ value) = value
  get (Normal value) = value
  set (Currency cs d _) value = Currency cs d value
  set (Normal _) value = Normal value

_decimals :: AffineTraversal' ContractValue Int
_decimals = affineTraversal set pre
  where
  set (Currency cs _ value) d = Currency cs d value
  set (Normal value) _ = Normal value
  pre (Currency _ d _) = Right d
  pre (Normal value) = Left $ Normal value

_currencySymbol :: AffineTraversal' ContractValue CurrencySymbol
_currencySymbol = affineTraversal set pre
  where
  set (Currency _ d value) cs = Currency cs d value
  set (Normal value) _ = Normal value
  pre (Currency cs _ _) = Right cs
  pre (Normal value) = Left $ Normal value

toString :: ContractValue -> String
toString (Normal value) = BigInt.toString value
toString (Currency _ decimals value) =
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
