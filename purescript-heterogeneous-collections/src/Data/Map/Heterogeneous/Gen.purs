module Data.Map.Heterogeneous.Gen where

import Prelude

import Data.Map.Heterogeneous (HMap, class HMapRecord, fromRecord)
import Prim.RowList (class RowToList)
import Test.QuickCheck.Gen (Gen)

genHMap
  :: forall rl r rec
   . HMapRecord rl r rec
  => RowToList r rl
  => Gen (Record rec)
  -> Gen (HMap r)
genHMap = map fromRecord
