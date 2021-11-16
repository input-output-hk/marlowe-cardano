module Data.Map.Row
  ( RowMap
  , class EqRowMapFields
  , class RowMapRecord
  , class ShowRowMapFields
  , clear
  , delete
  , empty
  , eqRowMapFields
  , fromRecord
  , get
  , insert
  , modify
  , rename
  , rowMapFromRecord
  , rowMapToRecord
  , set
  , showRowMapFields
  , toRecord
  ) where

import Prelude

import Data.Array as Array
import Data.List (List)
import Data.List as List
import Data.Map.Row.Unsafe
  ( UnsafeRowMap
  , unsafeDelete
  , unsafeEmpty
  , unsafeGet
  , unsafeSet
  )
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Row as R
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Record as Record
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- | A map whose keys and values are backed by a row or types. Within the map,
-- | the types of values is determined by the value of the keys. Values may or
-- | may not be present in the map. This makes it conceptually analogous to a
-- | record with all optional fields.
-- |
-- | In the taxonomy of Row-backed collections, RowMap is the most general. For
-- | a row of length N, it can contain between zero and N members, making it a
-- | superset of all row-backed collections.
-- |
-- | Note: because of the close analogy with Records, and because of the
-- | intended use at the time of writing, the API more closely resembles that
-- | of Record than Data.Map (e.g. it has a `get` method rather than a
-- | `lookup` method). Some of the API of `Data.Map` is provided in the cases
-- | where a matching method doesn't exist for `Record` however (such as
-- | `member`).
newtype RowMap r = RowMap (UnsafeRowMap r)

-- | A class used to define structural equality on RowMaps
class EqRowMapFields (rl :: RowList Type) (r :: Row Type) | rl -> r where
  eqRowMapFields :: Proxy rl -> RowMap r -> RowMap r -> Boolean

instance eqRowMapFieldsNil :: EqRowMapFields Nil () where
  eqRowMapFields _ _ _ = true

instance eqRowMapFieldsCons ::
  ( EqRowMapFields rl r'
  , R.Lacks l r
  , R.Cons l a r' r
  , IsSymbol l
  , Eq a
  ) =>
  EqRowMapFields (Cons l a rl) r where
  eqRowMapFields _ a b =
    get (Proxy :: _ l) a == get (Proxy :: _ l) b
      && eqRowMapFields (Proxy :: _ rl) (unsafeCoerce a) (unsafeCoerce b)

instance eqRowMap :: (RowToList r rl, EqRowMapFields rl r) => Eq (RowMap r) where
  eq a b = eqRowMapFields (Proxy :: _ rl) a b

-- | A class used to define structural stringification on RowMaps
class ShowRowMapFields (rl :: RowList Type) (r :: Row Type) | rl -> r where
  showRowMapFields :: Proxy rl -> RowMap r -> List String

instance showRowMapNil :: ShowRowMapFields Nil () where
  showRowMapFields _ _ = List.Nil

instance showRowMapCons ::
  ( ShowRowMapFields rl r'
  , R.Lacks l r
  , R.Cons l a r' r
  , IsSymbol l
  , Show a
  ) =>
  ShowRowMapFields (Cons l a rl) r where
  showRowMapFields _ rowMap =
    List.fromFoldable (showAtLabel <$> get (Proxy :: _ l) rowMap)
      <> showRowMapFields (Proxy :: _ rl) (unsafeCoerce rowMap)
    where
    showAtLabel a = joinWith ": " [ reflectSymbol (Proxy :: _ l), show a ]

instance showRowMap ::
  ( RowToList r rl
  , ShowRowMapFields rl r
  ) =>
  Show (RowMap r) where
  show a = case Array.fromFoldable $ showRowMapFields (Proxy :: _ rl) a of
    [] -> "(RowMap {})"
    fields -> joinWith " " [ "(RowMap {", joinWith ", " fields, "})" ]

-- | A class used to witness the isomorphism between RowMaps and Records.
class
  RowMapRecord (rl :: RowList Type) (r :: Row Type) (rec :: Row Type)
  | rl -> r rec where
  -- | Convert a RowMap to a Record with the same labels with the values
  -- | wrapped in a Maybe.
  rowMapToRecord :: Proxy rl -> RowMap r -> Record rec
  -- | Convert a Record with Maybe fields to a RowMap with the same labels
  -- | and unwrapped values.
  rowMapFromRecord :: Proxy rl -> Record rec -> RowMap r

instance rowMapRecordNil :: RowMapRecord Nil () () where
  rowMapToRecord _ _ = {}
  rowMapFromRecord _ _ = empty

instance rowMapRecordCons ::
  ( RowMapRecord rl r' rec'
  , R.Lacks l rec'
  , R.Lacks l r'
  , R.Cons l a r' r
  , R.Cons l (Maybe a) rec' rec
  , IsSymbol l
  ) =>
  RowMapRecord (Cons l a rl) r rec where
  rowMapToRecord _ r = Record.insert label (get label r) $ rowMapToRecord rl $
    unsafeCoerce r
    where
    label :: Proxy l
    label = Proxy

    rl :: Proxy rl
    rl = Proxy
  rowMapFromRecord _ r =
    case Record.get label r of
      Nothing -> unsafeCoerce $ rowMapFromRecord rl $ unsafeCoerce r
      Just a -> insert label a $ rowMapFromRecord rl $ unsafeCoerce r
    where
    label :: Proxy l
    label = Proxy

    rl :: Proxy rl
    rl = Proxy

-- | Get a value from a RowMap. Returns nothing if the value is not found.
get
  :: forall r' r l a
   . IsSymbol l
  => R.Cons l a r' r
  => Proxy l
  -> RowMap r
  -> Maybe a
get p (RowMap m) = unsafeGet Just Nothing (reflectSymbol p) m

-- | Add a value to a RowMap without affecting the row type.
set
  :: forall r1 r2 r l a b
   . IsSymbol l
  => R.Cons l a r r1
  => R.Cons l b r r2
  => Proxy l
  -> b
  -> RowMap r1
  -> RowMap r2
set p b (RowMap m) = RowMap $ unsafeSet (reflectSymbol p) b m

-- | Remove a value from a RowMap without affecting the row type.
clear
  :: forall r1 r2 r l a b
   . IsSymbol l
  => R.Cons l a r r1
  => R.Cons l b r r2
  => Proxy l
  -> RowMap r1
  -> RowMap r2
clear p (RowMap m) = RowMap $ unsafeDelete (reflectSymbol p) m

-- | Change a value within a row map with an update function.
modify
  :: forall r1 r2 r l a b
   . IsSymbol l
  => R.Cons l a r r1
  => R.Cons l b r r2
  => Proxy l
  -> (a -> b)
  -> RowMap r1
  -> RowMap r2
modify p f m = case get p m of
  Nothing -> unsafeCoerce m
  Just a -> set p (f a) m

-- | Add a value to a RowMap, adding a new label to the row.
insert
  :: forall r1 r2 l a
   . IsSymbol l
  => R.Lacks l r1
  => R.Cons l a r1 r2
  => Proxy l
  -> a
  -> RowMap r1
  -> RowMap r2
insert p a (RowMap m) = RowMap $ unsafeSet (reflectSymbol p) a m

-- | Add a new label to the row without adding a value.
addName
  :: forall r1 r2 l a
   . IsSymbol l
  => R.Lacks l r1
  => R.Cons l a r1 r2
  => Proxy l
  -> RowMap r1
  -> RowMap r2
addName _ = unsafeCoerce

-- | Remove a value from a RowMap, removing the label from the row.
delete
  :: forall r1 r2 l a
   . IsSymbol l
  => R.Cons l a r2 r1
  => R.Lacks l r2
  => Proxy l
  -> RowMap r1
  -> RowMap r2
delete p (RowMap m) = RowMap $ unsafeDelete (reflectSymbol p) m

-- | Change one of the labels in a row.
rename
  :: forall prev next a input inter output
   . IsSymbol prev
  => IsSymbol next
  => R.Cons prev a inter input
  => R.Lacks prev inter
  => R.Cons next a inter output
  => R.Lacks next inter
  => Proxy prev
  -> Proxy next
  -> RowMap input
  -> RowMap output
rename prev next r =
  case get prev r of
    Nothing -> addName next (delete prev r :: RowMap inter)
    Just a -> insert next a (delete prev r :: RowMap inter)

-- | Create an empty Row Map
empty :: forall r. RowMap r
empty = RowMap $ unsafeEmpty

-- | Convert a RowMap to a Record of Maybes
toRecord
  :: forall rl r rec
   . RowMapRecord rl r rec
  => RowToList r rl
  => RowMap r
  -> Record rec
toRecord = rowMapToRecord (Proxy :: _ rl)

-- | Convert a Record of Maybes to a RowMap.
fromRecord
  :: forall rl r rec
   . RowMapRecord rl r rec
  => RowToList r rl
  => Record rec
  -> RowMap r
fromRecord = rowMapFromRecord (Proxy :: _ rl)
