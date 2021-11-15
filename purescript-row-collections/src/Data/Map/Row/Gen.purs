module Data.Map.Row.Gen where

import Prelude

import Data.Map.Row (class RowListMaybe, RowMap, fromRecord)
import Prim.RowList (class RowToList)
import Test.QuickCheck.Gen (Gen)

genRowMap
  :: forall rl r r'
   . RowToList r rl
  => RowListMaybe rl r'
  => Gen (Record r')
  -> Gen (RowMap r)
genRowMap = map fromRecord
