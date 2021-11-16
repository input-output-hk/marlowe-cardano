module Data.Map.Row.Gen where

import Prelude

import Data.Map.Row (RowMap, class RowMapRecord, fromRecord)
import Prim.RowList (class RowToList)
import Test.QuickCheck.Gen (Gen)

genRowMap
  :: forall rl r rec
   . RowMapRecord rl r rec
  => RowToList r rl
  => Gen (Record rec)
  -> Gen (RowMap r)
genRowMap = map fromRecord
