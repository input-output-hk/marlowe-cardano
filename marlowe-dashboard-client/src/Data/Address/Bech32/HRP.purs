module Data.Address.Bech32.HRP
  ( Bech32HRP
  , fromCodePoints
  , fromString
  , isValid
  , toCodePoints
  , toString
  ) where

import Prologue

import Data.Address.Bech32.HRP.CodePoint
  ( HRPCodePoint
  , fromCodePoint
  , toCodePoint
  )
import Data.Array (length)
import Data.Filterable (filterMap)
import Data.Maybe (isJust)
import Data.String.NonEmpty
  ( NonEmptyString
  , fromCodePointArray
  , toCodePointArray
  )
import Data.String.NonEmpty as SNE
import Data.Traversable (traverse)

newtype Bech32HRP = Bech32HRP NonEmptyString

derive newtype instance Eq Bech32HRP
derive newtype instance Ord Bech32HRP

instance Show Bech32HRP where
  show (Bech32HRP cp) = "(Bech32HRP " <> show cp <> ")"

toCodePoints :: Bech32HRP -> Array HRPCodePoint
toCodePoints (Bech32HRP s) = filterMap fromCodePoint $ toCodePointArray s

fromCodePoints :: Array HRPCodePoint -> Maybe Bech32HRP
fromCodePoints cps
  | length cps > 83 = Nothing
  | otherwise = Bech32HRP <$> fromCodePointArray (toCodePoint <$> cps)

toString :: Bech32HRP -> String
toString (Bech32HRP s) = SNE.toString s

fromString :: String -> Maybe Bech32HRP
fromString = fromCodePoints
  <=< traverse fromCodePoint <<< toCodePointArray
  <=< SNE.fromString

isValid :: Bech32HRP -> Boolean
isValid (Bech32HRP cp) = isJust $ fromString $ SNE.toString cp
