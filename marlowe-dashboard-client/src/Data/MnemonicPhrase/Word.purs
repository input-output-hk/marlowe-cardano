module Data.MnemonicPhrase.Word
  ( Word
  , fromString
  , toString
  ) where

import Prologue

import Crypto.Encoding.BIP39.English as English
import Data.Argonaut
  ( class DecodeJson
  , class EncodeJson
  , JsonDecodeError(..)
  , decodeJson
  , encodeJson
  )
import Data.Array (elem) as Array
import Data.Either (note)

newtype Word = Word String

derive instance eqWord :: Eq Word
derive instance ordWord :: Ord Word
derive newtype instance showWord :: Show Word

instance DecodeJson Word where
  decodeJson =
    note (TypeMismatch "MnemonicPhrase.Word") <<< fromString <=< decodeJson

instance EncodeJson Word where
  encodeJson = encodeJson <<< toString

fromString :: String -> Maybe Word
fromString s =
  if s `Array.elem` English.dictionary then Just $ Word s
  else Nothing

toString :: Word -> String
toString (Word s) = s
