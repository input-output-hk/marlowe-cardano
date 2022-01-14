module Data.WalletNickname
  ( WalletNickname
  , WalletNicknameError(..)
  , dual
  , fromFoldable
  , fromString
  , new
  , validator
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
import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.String (null)
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (ignoreCase)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Validation.Semigroup (V(..))
import Polyform (Validator)
import Polyform.Dual as Dual
import Polyform.Validator (liftFnV)
import Polyform.Validator.Dual (Dual)

data WalletNicknameError
  = Empty
  | ContainsNonAlphaNumeric
  | Exists

derive instance genericWalletNicknameError :: Generic WalletNicknameError _
derive instance eqWalletNicknameError :: Eq WalletNicknameError
derive instance ordWalletNicknameError :: Ord WalletNicknameError

instance semigroupWalletNicknameError :: Semigroup WalletNicknameError where
  append Empty _ = Empty
  append _ Empty = Empty
  append ContainsNonAlphaNumeric _ = ContainsNonAlphaNumeric
  append _ ContainsNonAlphaNumeric = ContainsNonAlphaNumeric
  append Exists Exists = Exists

instance boundedWalletNicknameError :: Bounded WalletNicknameError where
  bottom = genericBottom
  top = genericTop

instance enumWalletNicknameError :: Enum WalletNicknameError where
  succ = genericSucc
  pred = genericPred

instance boundedEnumWalletNicknameError :: BoundedEnum WalletNicknameError where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

instance showWalletNicknameError :: Show WalletNicknameError where
  show = genericShow

newtype WalletNickname = WalletNickname String

derive instance Eq WalletNickname
derive instance Ord WalletNickname
derive newtype instance Show WalletNickname
derive newtype instance EncodeJson WalletNickname

instance DecodeJson WalletNickname where
  decodeJson =
    lmap (const $ TypeMismatch "WalletId") <<< fromString Set.empty
      <=< decodeJson

nicknameRegex :: Regex
nicknameRegex = unsafeRegex "^[a-z0-9]+$" ignoreCase

new :: WalletNickname
new = WalletNickname "newWallet"

fromFoldable :: forall f. Foldable f => f String -> Set WalletNickname
fromFoldable = Set.map WalletNickname <<< Set.fromFoldable

fromString
  :: Set WalletNickname -> String -> Either WalletNicknameError WalletNickname
fromString used s
  | null s = Left Empty
  | Set.member (WalletNickname s) used = Left Exists
  | Regex.test nicknameRegex s = Right $ WalletNickname s
  | otherwise = Left ContainsNonAlphaNumeric

toString :: WalletNickname -> String
toString (WalletNickname s) = s

-------------------------------------------------------------------------------
-- Polyform adapters
-------------------------------------------------------------------------------

validator
  :: forall m
   . Applicative m
  => Set WalletNickname
  -> Validator m WalletNicknameError String WalletNickname
validator used = liftFnV \s -> V $ fromString used s

dual
  :: forall m
   . Applicative m
  => Set WalletNickname
  -> Dual m WalletNicknameError String WalletNickname
dual used = Dual.dual (validator used) (pure <<< toString)
