module Data.Slot
  ( Slot
  ) where

import Prologue

import Data.Enum
  ( class BoundedEnum
  , class Enum
  , Cardinality(..)
  , fromEnum
  , toEnum
  )

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

instance Semigroup Slot where
  append = max

instance Monoid Slot where
  mempty = bottom
