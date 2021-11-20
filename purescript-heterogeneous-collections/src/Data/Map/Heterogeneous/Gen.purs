module Data.Map.Heterogeneous.Gen where

import Prelude

import Data.Map.Heterogeneous (HMap, class RowMapRecord, fromRecord)
import Prim.RowList (class RowToList)
import Test.QuickCheck.Gen (Gen)

genHMap
  :: forall rl r rec
   . RowMapRecord rl r rec
  => RowToList r rl
  => Gen (Record rec)
  -> Gen (HMap r)
genHMap = map fromRecord
