module Data.Slot
  ( Slot
  , fromPlutusSlot
  ) where

import Prologue

import Data.BigInt.Argonaut as BigInt
import Data.Enum
  ( class BoundedEnum
  , class Enum
  , Cardinality(..)
  , fromEnum
  , toEnum
  )
import Data.Int as Int
import Plutus.V1.Ledger.Slot as Plutus

-- FIXME: This is loosing precision, consider using a BigInt
newtype Slot = Slot Int

derive newtype instance Show Slot
derive instance Eq Slot
derive instance Ord Slot
instance Bounded Slot where
  bottom = Slot 0
  top = Slot $ top - 1

instance Enum Slot where
  pred = toEnum <<< (_ - 1) <<< fromEnum
  succ = toEnum <<< (_ + 1) <<< fromEnum

instance BoundedEnum Slot where
  cardinality = Cardinality top
  fromEnum (Slot i) = i
  toEnum i
    | i < 0 || i >= top = Nothing
    | otherwise = Just $ Slot i

fromPlutusSlot :: Plutus.Slot -> Slot
fromPlutusSlot (Plutus.Slot { getSlot }) = Slot $ Int.floor $ BigInt.toNumber
  getSlot
