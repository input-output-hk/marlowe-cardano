module Data.Map.Heterogeneous
  ( HMap
  , class EqHMapFields
  , class HMapRecord
  , class ShowHMapFields
  , clear
  , delete
  , empty
  , eqHMapFields
  , fromRecord
  , get
  , insert
  , modify
  , rename
  , hmapFromRecord
  , hmapToRecord
  , set
  , setWith
  , showHMapFields
  , toRecord
  ) where

import Prelude

import Data.Array as Array
import Data.List (List)
import Data.List as List
import Data.Map.Heterogeneous.Unsafe
  ( UnsafeHMap
  , unsafeDelete
  , unsafeEmpty
  , unsafeGet
  , unsafeSet
  )
import Data.Maybe (Maybe(..), maybe)
import Data.String (joinWith)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Row as R
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Record as Record
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- TODO rename modify to update
-- TODO rename delete to pop
-- TODO rename clear to delete
-- TODO rename get to lookup
-- TODO rename insert to push
-- TODO rename set to insert
-- TODO rename setWith to insertWith
-- TODO add unionWith :: Record duplicateHandlers -> HMap r1 -> HMap r2 -> HMap r3
-- TODO add union :: HMap r1 -> HMap r2 -> HMap r3
-- TODO add unions :: f (HMap r) -> HMap r
-- TODO add toUnfoldable :: HMap r -> f (Variant r)
-- TODO add unsafeToUnfoldable :: HMap r -> f (Variant r)      (unrdered)
-- TODO add size :: HMap r -> Int
-- TODO add singleton :: Variant r -> HMap r
-- TODO add member :: Proxy l -> HMap r -> Boolean
-- TODO add isSubmap :: HMap r1 -> HMap r2 -> Boolean
-- TODO add isEmpty :: HMap r -> Boolean
-- TODO add intersectionWith :: Record duplicateHandlers -> HMap r1 -> HMap r2 -> HMap duplicates
-- TODO add intersection :: HMap r1 -> HMap r2 -> HMap r3
-- TODO add intersections :: f (HMap r) -> HMap r
-- TODO add fromFoldable :: f (Variant r) -> HMap r
-- TODO add filterKeys :: MaskFields r mask r' (Const Unit) => Record mask -> HMap r -> HMap r'
-- TODO add filter :: MaskFields r mask r' Predicate => Record mask -> HMap r -> HMap r'
-- TODO add difference :: HMap r1 -> HMap r2 -> HMap r3
-- TODO add compact :: HMap rMaybes -> HMap r
-- TODO add compactMap :: Record maybeMappers -> HMap r1 -> HMap r2
-- TODO add map :: Record mappers -> HMap r1 -> HMap r2
-- TODO add alter :: (Maybe a -> Maybe b) -> Proxy k -> HMap r1 -> HMap r2

-- | A map whose keys and values are backed by a row or types. Within the map,
-- | the types of values is determined by the value of the keys. Values may or
-- | may not be present in the map. This makes it conceptually analogous to a
-- | record with all optional fields.
-- |
-- | In the taxonomy of Row-backed collections, HMap is the most general. For
-- | a row of length N, it can contain between zero and N members, making it a
-- | superset of all row-backed collections.
-- |
-- | Note: because of the close analogy with Records, and because of the
-- | intended use at the time of writing, the API more closely resembles that
-- | of Record than Data.Map (e.g. it has a `get` method rather than a
-- | `lookup` method). Some of the API of `Data.Map` is provided in the cases
-- | where a matching method doesn't exist for `Record` however (such as
-- | `member`).
newtype HMap :: forall k. Row k -> Type
newtype HMap r = HMap (UnsafeHMap r)

-- | A class used to define structural equality on HMaps
class EqHMapFields (rl :: RowList Type) (r :: Row Type) | rl -> r where
  eqHMapFields :: Proxy rl -> HMap r -> HMap r -> Boolean

instance eqHMapFieldsNil :: EqHMapFields Nil () where
  eqHMapFields _ _ _ = true

instance eqHMapFieldsCons ::
  ( EqHMapFields rl r'
  , R.Lacks l r
  , R.Cons l a r' r
  , IsSymbol l
  , Eq a
  ) =>
  EqHMapFields (Cons l a rl) r where
  eqHMapFields _ a b =
    get (Proxy :: _ l) a == get (Proxy :: _ l) b
      && eqHMapFields (Proxy :: _ rl) (unsafeCoerce a) (unsafeCoerce b)

instance eqHMap :: (RowToList r rl, EqHMapFields rl r) => Eq (HMap r) where
  eq a b = eqHMapFields (Proxy :: _ rl) a b

-- | A class used to define structural stringification on HMaps
class ShowHMapFields (rl :: RowList Type) (r :: Row Type) | rl -> r where
  showHMapFields :: Proxy rl -> HMap r -> List String

instance showHMapNil :: ShowHMapFields Nil () where
  showHMapFields _ _ = List.Nil

instance showHMapCons ::
  ( ShowHMapFields rl r'
  , R.Lacks l r
  , R.Cons l a r' r
  , IsSymbol l
  , Show a
  ) =>
  ShowHMapFields (Cons l a rl) r where
  showHMapFields _ hmap =
    List.fromFoldable (showAtLabel <$> get (Proxy :: _ l) hmap)
      <> showHMapFields (Proxy :: _ rl) (unsafeCoerce hmap)
    where
    showAtLabel a = joinWith ": " [ reflectSymbol (Proxy :: _ l), show a ]

instance showHMap ::
  ( RowToList r rl
  , ShowHMapFields rl r
  ) =>
  Show (HMap r) where
  show a = case Array.fromFoldable $ showHMapFields (Proxy :: _ rl) a of
    [] -> "(HMap {})"
    fields -> joinWith " " [ "(HMap {", joinWith ", " fields, "})" ]

-- | A class used to witness the isomorphism between HMaps and Records.
class
  HMapRecord (rl :: RowList Type) (r :: Row Type) (rec :: Row Type)
  | rl -> r rec where
  -- | Convert a HMap to a Record with the same labels with the values
  -- | wrapped in a Maybe.
  hmapToRecord :: Proxy rl -> HMap r -> Record rec
  -- | Convert a Record with Maybe fields to a HMap with the same labels
  -- | and unwrapped values.
  hmapFromRecord :: Proxy rl -> Record rec -> HMap r

instance hmapRecordNil :: HMapRecord Nil () () where
  hmapToRecord _ _ = {}
  hmapFromRecord _ _ = empty

instance hmapRecordCons ::
  ( HMapRecord rl r' rec'
  , R.Lacks l rec'
  , R.Lacks l r'
  , R.Cons l a r' r
  , R.Cons l (Maybe a) rec' rec
  , IsSymbol l
  ) =>
  HMapRecord (Cons l a rl) r rec where
  hmapToRecord _ r = Record.insert label (get label r) $ hmapToRecord rl $
    unsafeCoerce r
    where
    label :: Proxy l
    label = Proxy

    rl :: Proxy rl
    rl = Proxy
  hmapFromRecord _ r =
    case Record.get label r of
      Nothing -> unsafeCoerce $ hmapFromRecord rl $ unsafeCoerce r
      Just a -> insert label a $ hmapFromRecord rl $ unsafeCoerce r
    where
    label :: Proxy l
    label = Proxy

    rl :: Proxy rl
    rl = Proxy

-- | Get a value from a HMap. Returns nothing if the value is not found.
get
  :: forall r' r l a
   . IsSymbol l
  => R.Cons l a r' r
  => Proxy l
  -> HMap r
  -> Maybe a
get p (HMap m) = unsafeGet Just Nothing (reflectSymbol p) m

-- | Add a value to a HMap without affecting the row type.
set
  :: forall r1 r2 r l a b
   . IsSymbol l
  => R.Cons l a r r1
  => R.Cons l b r r2
  => Proxy l
  -> b
  -> HMap r1
  -> HMap r2
set p b (HMap m) = HMap $ unsafeSet (reflectSymbol p) b m

-- | Add a value to a HMap without affecting the row type. The provided
-- | function is used to combine the new value with the existing value, if
-- | there is one.
setWith
  :: forall r1 r2 r l a b
   . IsSymbol l
  => R.Cons l a r r1
  => R.Cons l b r r2
  => (b -> a -> b)
  -> Proxy l
  -> b
  -> HMap r1
  -> HMap r2
setWith f k b m = flip (set k) m $ maybe b (f b) $ get k m

-- | Remove a value from a HMap without affecting the row type.
clear
  :: forall r1 r2 r l a b
   . IsSymbol l
  => R.Cons l a r r1
  => R.Cons l b r r2
  => Proxy l
  -> HMap r1
  -> HMap r2
clear p (HMap m) = HMap $ unsafeDelete (reflectSymbol p) m

-- | Change a value within a row map with an update function.
modify
  :: forall r1 r2 r l a b
   . IsSymbol l
  => R.Cons l a r r1
  => R.Cons l b r r2
  => Proxy l
  -> (a -> b)
  -> HMap r1
  -> HMap r2
modify p f m = case get p m of
  Nothing -> unsafeCoerce m
  Just a -> set p (f a) m

-- | Add a value to a HMap, adding a new label to the row.
insert
  :: forall r1 r2 l a
   . IsSymbol l
  => R.Lacks l r1
  => R.Cons l a r1 r2
  => Proxy l
  -> a
  -> HMap r1
  -> HMap r2
insert p a (HMap m) = HMap $ unsafeSet (reflectSymbol p) a m

-- | Add a new label to the row without adding a value.
addName
  :: forall r1 r2 l a
   . IsSymbol l
  => R.Lacks l r1
  => R.Cons l a r1 r2
  => Proxy l
  -> HMap r1
  -> HMap r2
addName _ = unsafeCoerce

-- | Remove a value from a HMap, removing the label from the row.
delete
  :: forall r1 r2 l a
   . IsSymbol l
  => R.Cons l a r2 r1
  => R.Lacks l r2
  => Proxy l
  -> HMap r1
  -> HMap r2
delete p (HMap m) = HMap $ unsafeDelete (reflectSymbol p) m

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
  -> HMap input
  -> HMap output
rename prev next r =
  case get prev r of
    Nothing -> addName next (delete prev r :: HMap inter)
    Just a -> insert next a (delete prev r :: HMap inter)

-- | Create an empty Row Map
empty :: forall r. HMap r
empty = HMap $ unsafeEmpty

-- | Convert a HMap to a Record of Maybes
toRecord
  :: forall rl r rec
   . HMapRecord rl r rec
  => RowToList r rl
  => HMap r
  -> Record rec
toRecord = hmapToRecord (Proxy :: _ rl)

-- | Convert a Record of Maybes to a HMap.
fromRecord
  :: forall rl r rec
   . HMapRecord rl r rec
  => RowToList r rl
  => Record rec
  -> HMap r
fromRecord = hmapFromRecord (Proxy :: _ rl)
