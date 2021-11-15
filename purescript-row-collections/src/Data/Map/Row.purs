module Data.Map.Row
  ( RowMap
  , class RowListMaybe
  , fromRecord
  , toRecord
  , get
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Symbol (class IsSymbol)
import Prim.Row (class Cons) as R
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Record as Record
import Unsafe.Coerce (unsafeCoerce)

-- | A map whose keys and values are backed by a row or types. Within the map,
-- | the types of values is determined by the value of the keys. Values may or
-- | may not be present in the map. This makes it conceptually, and currently
-- | literally, analogous to a record with all optional fields.
-- |
-- | Note: because of the close analogy with Records, and because of the
-- | intended use at the time of writing, the API more closely resembles that
-- | of Data.Record than Data.Map (e.g. it has a `get` method rather than a
-- | `lookup` method). Some of the API of `Data.Map` is provided in the cases
-- | where a matching method doesn't exist for `Data.Record` however (such as
-- | `member`).
newtype RowMap :: Row Type -> Type
newtype RowMap r
  = RowMap
  ( forall a
     . ( forall rl r'. RowToList r rl => RowListMaybe rl r' => Record r' -> a
       )
    -> a
  )

class RowListMaybe (rl :: RowList Type) (r :: Row Type) | rl -> r

instance rowListMaybeNill :: RowListMaybe Nil ()

instance rowMaybeCons ::
  ( RowListMaybe rl r'
  , R.Cons l (Maybe a) r' r
  , IsSymbol l
  ) =>
  RowListMaybe (Cons l a rl) r

fromRecord
  :: forall rl r r'
   . RowToList r rl
  => RowListMaybe rl r'
  => Record r'
  -> RowMap r
fromRecord r = RowMap \k -> k r

toRecord
  :: forall rl r r'
   . RowToList r rl
  => RowListMaybe rl r'
  => RowMap r
  -> Record r'
toRecord (RowMap k) = k unsafeCoerce

get
  :: forall proxy rl r mr mr' l a
   . IsSymbol l
  => RowToList r rl
  => RowListMaybe rl mr
  => R.Cons l (Maybe a) mr' mr
  => proxy l
  -> RowMap r
  -> Maybe a
get p = Record.get p <<< toRecord
