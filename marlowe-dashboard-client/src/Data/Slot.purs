module Data.Slot
  ( Slot
  , asFormattedPercentage
  , asPercentage
  , format
  , fromPlutusSlot
  , zeroSlot
  ) where

import Prologue

import Data.BigInt.Argonaut (BigInt)
import Data.BigInt.Argonaut as BigInt
import Data.Formatter.Number (Formatter(..), format) as Number
import Plutus.V1.Ledger.Slot as Plutus

newtype Slot = Slot BigInt

derive instance Eq Slot
derive instance Ord Slot
instance Show Slot where
  show slot = "(Slot " <> format slot <> ")"

zeroSlot :: Slot
zeroSlot = Slot zero

-- Gives a string representation that is human readable (using a comma every thousand)
format :: Slot -> String
format (Slot slot) = Number.format slotFormatter $ BigInt.toNumber slot
  where
  slotFormatter =
    Number.Formatter
      { sign: false
      , before: 0
      , comma: true
      , after: 0
      , abbreviations: false
      }

asPercentage :: Slot -> Slot -> Number
asPercentage (Slot min) (Slot max) = BigInt.toNumber min / BigInt.toNumber max *
  100.0

asFormattedPercentage :: Slot -> Slot -> String
asFormattedPercentage min max =
  let
    percentage = asPercentage min max
  in
    Number.format percentageFormatter percentage <> "%"

fromPlutusSlot :: Plutus.Slot -> Slot
fromPlutusSlot (Plutus.Slot { getSlot }) = Slot getSlot

percentageFormatter :: Number.Formatter
percentageFormatter =
  Number.Formatter
    { sign: false
    , before: 0
    , comma: true
    , after: 2
    , abbreviations: false
    }
