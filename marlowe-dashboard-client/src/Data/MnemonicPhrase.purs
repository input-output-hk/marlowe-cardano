module Data.MnemonicPhrase
  ( MnemonicPhrase
  , MnemonicPhraseError(..)
  , Word
  , checkMnemonic
  , class CheckMnemonic
  , dual
  , fromString
  , fromWords
  , toString
  , toWords
  , validator
  , wordFromString
  , wordToString
  ) where

import Prologue

import Control.Monad.Except (ExceptT, except, runExceptT, throwError)
import Control.Monad.Trans.Class (lift)
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
import Data.Filterable (filter)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), contains, joinWith, split, trim)
import Data.Traversable (traverse)
import Data.Validation.Semigroup (V(..))
import Polyform (Validator)
import Polyform.Dual as Dual
import Polyform.Validator (liftFnMV)
import Polyform.Validator.Dual (Dual)

data MnemonicPhraseError
  = Empty
  | WrongWordCount
  | ContainsInvalidWords

derive instance genericMnemonicPhraseError :: Generic MnemonicPhraseError _
derive instance eqMnemonicPhraseError :: Eq MnemonicPhraseError
derive instance ordMnemonicPhraseError :: Ord MnemonicPhraseError

instance semigroupMnemonicPhraseError :: Semigroup MnemonicPhraseError where
  append Empty _ = Empty
  append _ Empty = Empty
  append WrongWordCount _ = WrongWordCount
  append _ WrongWordCount = WrongWordCount
  append ContainsInvalidWords ContainsInvalidWords = ContainsInvalidWords

instance boundedMnemonicPhraseError :: Bounded MnemonicPhraseError where
  bottom = genericBottom
  top = genericTop

instance enumMnemonicPhraseError :: Enum MnemonicPhraseError where
  succ = genericSucc
  pred = genericPred

instance boundedEnumMnemonicPhraseError :: BoundedEnum MnemonicPhraseError where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

instance showMnemonicPhraseError :: Show MnemonicPhraseError where
  show = genericShow

newtype Word = Word String

derive instance eqWord :: Eq Word
derive instance ordWord :: Ord Word
derive newtype instance showWord :: Show Word

data MnemonicPhrase =
  MnemonicPhrase
    Word
    Word
    Word
    Word
    Word
    Word
    Word
    Word
    Word
    Word
    Word
    Word
    Word
    Word
    Word
    Word
    Word
    Word
    Word
    Word
    Word
    Word
    Word
    Word

derive instance eqMnemonicPhrase :: Eq MnemonicPhrase
derive instance ordMnemonicPhrase :: Ord MnemonicPhrase
instance showMnemonicPhrase :: Show MnemonicPhrase where
  show = show <<< toWords

class Monad m <= CheckMnemonic m where
  checkMnemonic :: MnemonicPhrase -> m Boolean

wordFromString :: String -> Maybe Word
wordFromString s =
  case trim s of
    "" -> Nothing
    _
      | contains (Pattern " ") s -> Nothing
    _ -> Just $ Word s

fromString
  :: forall m
   . CheckMnemonic m
  => String
  -> ExceptT MnemonicPhraseError m MnemonicPhrase
fromString =
  fromWords
    <=< except
      <<< note ContainsInvalidWords
      <<< traverse wordFromString
      <<< filter (notEq "")
      <<< split (Pattern " ")

fromWords
  :: forall m
   . CheckMnemonic m
  => Array Word
  -> ExceptT MnemonicPhraseError m MnemonicPhrase
fromWords = case _ of
  [] -> throwError Empty
  [ w1
  , w2
  , w3
  , w4
  , w5
  , w6
  , w7
  , w8
  , w9
  , w10
  , w11
  , w12
  , w13
  , w14
  , w15
  , w16
  , w17
  , w18
  , w19
  , w20
  , w21
  , w22
  , w23
  , w24
  ] -> do
    let
      m = MnemonicPhrase
        w1
        w2
        w3
        w4
        w5
        w6
        w7
        w8
        w9
        w10
        w11
        w12
        w13
        w14
        w15
        w16
        w17
        w18
        w19
        w20
        w21
        w22
        w23
        w24
    valid <- lift $ checkMnemonic m
    if valid then
      pure m
    else
      throwError ContainsInvalidWords
  _ -> throwError WrongWordCount

wordToString :: Word -> String
wordToString (Word s) = s

toString :: MnemonicPhrase -> String
toString = joinWith " " <<< map wordToString <<< toWords

toWords :: MnemonicPhrase -> Array Word
toWords
  ( MnemonicPhrase
      w1
      w2
      w3
      w4
      w5
      w6
      w7
      w8
      w9
      w10
      w11
      w12
      w13
      w14
      w15
      w16
      w17
      w18
      w19
      w20
      w21
      w22
      w23
      w24
  ) =
  [ w1
  , w2
  , w3
  , w4
  , w5
  , w6
  , w7
  , w8
  , w9
  , w10
  , w11
  , w12
  , w13
  , w14
  , w15
  , w16
  , w17
  , w18
  , w19
  , w20
  , w21
  , w22
  , w23
  , w24
  ]

-------------------------------------------------------------------------------
-- Polyform adapters
-------------------------------------------------------------------------------

validator
  :: forall m
   . CheckMnemonic m
  => Validator m MnemonicPhraseError String MnemonicPhrase
validator = liftFnMV $ map V <<< runExceptT <<< fromString

dual
  :: forall m
   . CheckMnemonic m
  => Dual m MnemonicPhraseError String MnemonicPhrase
dual = Dual.dual validator (pure <<< toString)
