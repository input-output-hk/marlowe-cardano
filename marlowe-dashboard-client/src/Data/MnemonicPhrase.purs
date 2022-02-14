module Data.MnemonicPhrase
  ( MnemonicPhrase
  , MnemonicPhraseError(..)
  , MnenonicPhraseErrorRow
  , fromString
  , fromStrings
  , fromWords
  , toString
  , toWords
  , injErr
  ) where

import Prologue

import Control.Monad.Except (throwError)
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
import Data.MnemonicPhrase.Word (Word)
import Data.MnemonicPhrase.Word (fromString, toString) as Word
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), joinWith, split)
import Data.Traversable (traverse)
import Data.Variant (Variant)
import Data.Variant (inj) as Variant
import Type.Proxy (Proxy(..))

data MnemonicPhraseError
  = Empty
  | WrongWordCount
  | ContainsInvalidWords

derive instance genericMnemonicPhraseError :: Generic MnemonicPhraseError _
derive instance eqMnemonicPhraseError :: Eq MnemonicPhraseError
derive instance ordMnemonicPhraseError :: Ord MnemonicPhraseError

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

fromString
  :: String
  -> Either MnemonicPhraseError MnemonicPhrase
fromString =
  fromStrings
    <<< filter (notEq "")
    <<< split (Pattern " ")

fromStrings
  :: Array String
  -> Either MnemonicPhraseError MnemonicPhrase
fromStrings =
  fromWords
    <=< note ContainsInvalidWords
      <<< traverse Word.fromString

fromWords
  :: Array Word
  -> Either MnemonicPhraseError MnemonicPhrase
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
  ] -> pure $ MnemonicPhrase
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
  _ -> throwError WrongWordCount

type MnenonicPhraseErrorRow r = (mnemonicPharseError :: MnemonicPhraseError | r)

injErr
  :: forall r
   . MnemonicPhraseError
  -> Variant (MnenonicPhraseErrorRow r)
injErr = Variant.inj (Proxy :: Proxy "mnemonicPharseError")

toString :: MnemonicPhrase -> String
toString = joinWith " " <<< map Word.toString <<< toWords

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
