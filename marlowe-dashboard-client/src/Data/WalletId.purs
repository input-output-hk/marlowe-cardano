module Data.WalletId
  ( WalletId
  , WalletIdError(..)
  , fromString
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
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as SNE
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (ignoreCase)
import Data.String.Regex.Unsafe (unsafeRegex)
import Servant.PureScript (class ToPathSegment, toPathSegment)

data WalletIdError
  = Empty
  | Invalid

derive instance genericWalletIdError :: Generic WalletIdError _
derive instance eqWalletIdError :: Eq WalletIdError
derive instance ordWalletIdError :: Ord WalletIdError

instance semigroupWalletIdError :: Semigroup WalletIdError where
  append Empty _ = Empty
  append _ Empty = Empty
  append Invalid Invalid = Invalid

instance boundedWalletIdError :: Bounded WalletIdError where
  bottom = genericBottom
  top = genericTop

instance enumWalletIdError :: Enum WalletIdError where
  succ = genericSucc
  pred = genericPred

instance boundedEnumWalletIdError :: BoundedEnum WalletIdError where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

instance showWalletIdError :: Show WalletIdError where
  show = genericShow

newtype WalletId = WalletId NonEmptyString

derive instance Eq WalletId
derive instance Ord WalletId
derive newtype instance Show WalletId
derive newtype instance EncodeJson WalletId

instance ToPathSegment WalletId where
  toPathSegment = toPathSegment <<< toString

instance DecodeJson WalletId where
  decodeJson =
    lmap (const $ TypeMismatch "WalletId") <<< fromString <=< decodeJson

-- WalletIDs are hex-encoded blake2b 160 hashes. They are 40-characters long.
hexRegex :: Regex
hexRegex = unsafeRegex "^[a-f0-9]{40}$" ignoreCase

fromString :: String -> Either WalletIdError WalletId
fromString s = do
  nes <- note Empty $ SNE.fromString s
  if Regex.test hexRegex s then
    Right $ WalletId nes
  else
    Left Invalid

toString :: WalletId -> String
toString (WalletId s) = SNE.toString s
