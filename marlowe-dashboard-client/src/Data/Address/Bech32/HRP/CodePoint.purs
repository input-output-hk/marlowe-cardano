module Data.Address.Bech32.HRP.CodePoint
  ( HRPCodePoint
  , fromCodePoint
  , toCodePoint
  , isValid
  ) where

import Prologue

import Data.Enum
  ( class BoundedEnum
  , class Enum
  , Cardinality(..)
  , defaultPred
  , defaultSucc
  , fromEnum
  , toEnum
  )
import Data.Filterable (filter)
import Data.String (CodePoint)
import Unsafe.Coerce (unsafeCoerce)

newtype HRPCodePoint = HRPCodePoint CodePoint

derive newtype instance Eq HRPCodePoint
derive newtype instance Ord HRPCodePoint

instance Show HRPCodePoint where
  show (HRPCodePoint cp) = "(HRPCodePoint " <> show cp <> ")"

instance Bounded HRPCodePoint where
  bottom = HRPCodePoint $ unsafeCoerce 33
  top = HRPCodePoint $ unsafeCoerce 126

instance Enum HRPCodePoint where
  succ = defaultSucc toEnum fromEnum
  pred = defaultPred toEnum fromEnum

instance BoundedEnum HRPCodePoint where
  cardinality = Cardinality (126 - 33 + 1)
  fromEnum (HRPCodePoint cp) = fromEnum cp - 33
  toEnum = fromCodePoint <=< toEnum <<< (_ + 33)

fromCodePoint :: CodePoint -> Maybe HRPCodePoint
fromCodePoint = filter isValid <<< pure <<< HRPCodePoint

toCodePoint :: HRPCodePoint -> CodePoint
toCodePoint (HRPCodePoint cp) = cp

isValid :: HRPCodePoint -> Boolean
isValid (HRPCodePoint cp) = intVal >= 33 && intVal <= 126
  where
  intVal = fromEnum cp
