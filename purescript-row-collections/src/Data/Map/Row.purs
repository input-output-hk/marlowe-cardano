module Data.Map.Row
  ( RowMap
  , class RowListMaybe
  , fromRecord
  , toRecord
  , mapRecord
  , get
  , set
  ) where

import Prelude

import Data.Eq (class EqRecord, eqRecord)
import Data.Maybe (Maybe(..))
import Data.Ord (class OrdRecord, compareRecord)
import Data.Show (class ShowRecordFields, showRecordFields)
import Data.String (joinWith)
import Data.Symbol (class IsSymbol)
import Prim.Row (class Cons) as R
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Record as Record
import Type.Proxy (Proxy(..))
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

instance eqRowMap ::
  ( RowToList r rl
  , RowToList mr mrl
  , RowListMaybe rl mr
  , EqRecord mrl mr
  ) =>
  Eq (RowMap r) where
  eq a b = eqRecord (Proxy :: _ mrl) (toRecord a) (toRecord b)

instance ordRowMap ::
  ( RowToList r rl
  , RowToList mr mrl
  , RowListMaybe rl mr
  , OrdRecord mrl mr
  ) =>
  Ord (RowMap r) where
  compare a b = compareRecord (Proxy :: _ mrl) (toRecord a) (toRecord b)

instance showRowMap ::
  ( RowToList r rl
  , RowToList mr mrl
  , RowListMaybe rl mr
  , ShowRecordFields mrl mr
  ) =>
  Show (RowMap r) where
  show a = case showRecordFields (Proxy :: _ mrl) (toRecord a) of
    [] -> "RowMap {}"
    fields -> joinWith " " [ "RowMap {", joinWith ", " fields, "}" ]

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

mapRecord
  :: forall rl1 r1 mr1 rl2 r2 mr2
   . RowToList r1 rl1
  => RowToList r2 rl2
  => RowListMaybe rl1 mr1
  => RowListMaybe rl2 mr2
  => (Record mr1 -> Record mr2)
  -> RowMap r1
  -> RowMap r2
mapRecord f = fromRecord <<< f <<< toRecord

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

set
  :: forall proxy rl1 rl2 r1 r2 mr1 mr2 mr l a b
   . IsSymbol l
  => RowToList r1 rl1
  => RowToList r2 rl2
  => RowListMaybe rl1 mr1
  => RowListMaybe rl2 mr2
  => R.Cons l (Maybe a) mr mr1
  => R.Cons l (Maybe b) mr mr2
  => proxy l
  -> b
  -> RowMap r1
  -> RowMap r2
set p = mapRecord <<< Record.set p <<< Just
