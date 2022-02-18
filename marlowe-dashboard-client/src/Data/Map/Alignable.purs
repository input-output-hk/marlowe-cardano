module Data.Map.Alignable where

import Prologue

import Data.Align (class Align, class Alignable)
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (class Newtype)
import Data.Set as Set
import Data.These (maybeThese)

newtype AlignableMap k v = AlignableMap (Map k v)

derive instance Newtype (AlignableMap k v) _

derive newtype instance Functor (AlignableMap k)

instance Ord k => Align (AlignableMap k) where
  align f (AlignableMap map1) (AlignableMap map2) =
    AlignableMap $ Map.mapMaybeWithKey align' unionKeys
    where
    unionKeys = Set.toMap $ Set.union (Map.keys map1) (Map.keys map2)
    align' k _ = f <$> maybeThese (Map.lookup k map1) (Map.lookup k map2)

instance Ord k => Alignable (AlignableMap k) where
  nil = AlignableMap Map.empty
