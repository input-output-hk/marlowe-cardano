module Data.ContractNickname
  ( ContractNickname
  , ContractNicknameError(..)
  , fromFoldable
  , fromString
  , toString
  , unknown
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

data ContractNicknameError = Empty

derive instance genericContractNicknameError :: Generic ContractNicknameError _
derive instance eqContractNicknameError :: Eq ContractNicknameError
derive instance ordContractNicknameError :: Ord ContractNicknameError

instance semigroupContractNicknameError :: Semigroup ContractNicknameError where
  append _ _ = Empty

instance boundedContractNicknameError :: Bounded ContractNicknameError where
  bottom = genericBottom
  top = genericTop

instance enumContractNicknameError :: Enum ContractNicknameError where
  succ = genericSucc
  pred = genericPred

instance boundedEnumContractNicknameError :: BoundedEnum ContractNicknameError where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

instance showContractNicknameError :: Show ContractNicknameError where
  show = genericShow

newtype ContractNickname = ContractNickname String

derive instance Eq ContractNickname
derive instance Ord ContractNickname
derive newtype instance Show ContractNickname
derive newtype instance EncodeJson ContractNickname

instance DecodeJson ContractNickname where
  decodeJson =
    lmap (const $ TypeMismatch "ContractNickname") <<< fromString
      <=< decodeJson

fromFoldable :: forall f. Foldable f => f String -> Set ContractNickname
fromFoldable = Set.map ContractNickname <<< Set.fromFoldable

fromString :: String -> Either ContractNicknameError ContractNickname
fromString s
  | null s = Left Empty
  | otherwise = Right $ ContractNickname s

toString :: ContractNickname -> String
toString (ContractNickname s) = s

-------------------------------------------------------------------------------
-- Fallback constructor
-------------------------------------------------------------------------------

unknown :: ContractNickname
unknown = ContractNickname "Unknown"
