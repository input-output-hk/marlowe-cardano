module Data.MnemonicPhrase.Word
  ( Word
  , fromString
  , toString
  ) where

import Prologue

import Crypto.Encoding.BIP39.English as English
import Data.Array (elem) as Array

newtype Word = Word String

derive instance eqWord :: Eq Word
derive instance ordWord :: Ord Word
derive newtype instance showWord :: Show Word

fromString :: String -> Maybe Word
fromString s =
  if s `Array.elem` English.words then Just $ Word s
  else Nothing

toString :: Word -> String
toString (Word s) = s

