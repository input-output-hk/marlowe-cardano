module Data.Address.Bech32.DataPart
  ( Bech32DataPart
  , fromCodePoints
  , fromString
  , isValid
  , toCodePoints
  , toString
  ) where

import Prologue

import Data.Address.Bech32.DataPart.CodePoint
  ( DataPartCodePoint
  , fromCodePoint
  , toCodePoint
  )
import Data.Filterable (filterMap)
import Data.Maybe (isJust)
import Data.String.NonEmpty
  ( NonEmptyString
  , fromCodePointArray
  , toCodePointArray
  )
import Data.String.NonEmpty as SNE
import Data.Traversable (traverse)

newtype Bech32DataPart = Bech32DataPart NonEmptyString

derive newtype instance Eq Bech32DataPart
derive newtype instance Ord Bech32DataPart

instance Show Bech32DataPart where
  show (Bech32DataPart cp) = "(Bech32DataPart " <> show cp <> ")"

toCodePoints :: Bech32DataPart -> Array DataPartCodePoint
toCodePoints (Bech32DataPart s) = filterMap fromCodePoint $ toCodePointArray s

fromCodePoints :: Array DataPartCodePoint -> Maybe Bech32DataPart
fromCodePoints = map Bech32DataPart <<< fromCodePointArray <<< map toCodePoint

toString :: Bech32DataPart -> String
toString (Bech32DataPart s) = SNE.toString s

fromString :: String -> Maybe Bech32DataPart
fromString = fromCodePoints
  <=< traverse fromCodePoint <<< toCodePointArray
  <=< SNE.fromString

isValid :: Bech32DataPart -> Boolean
isValid (Bech32DataPart cp) = isJust $ fromString $ SNE.toString cp
