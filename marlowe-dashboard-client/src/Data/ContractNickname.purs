module Data.ContractNickname
  ( ContractNickname
  , ContractNicknameError(..)
  , dual
  , fromFoldable
  , fromString
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
import Data.Validation.Semigroup (V(..))
import Polyform (Validator)
import Polyform.Dual as Dual
import Polyform.Validator (liftFnV)
import Polyform.Validator.Dual (Dual)

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
-- Polyform adapters
-------------------------------------------------------------------------------

validator
  :: forall m
   . Applicative m
  => Validator m ContractNicknameError String ContractNickname
validator = liftFnV \s -> V $ fromString s

dual
  :: forall m
   . Applicative m
  => Dual m ContractNicknameError String ContractNickname
dual = Dual.dual validator (pure <<< toString)
