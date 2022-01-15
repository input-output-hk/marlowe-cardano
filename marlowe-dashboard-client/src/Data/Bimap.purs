module Data.Bimap
  ( Bimap
  , associated
  , deleteL
  , deleteR
  , fromFoldable
  , empty
  , insert
  , keysL
  , keysR
  , lookupL
  , lookupR
  , memberL
  , memberR
  , null
  , singleton
  , size
  , swap
  , swapped
  , toUnfoldable
  , tryInsert
  ) where

import Prologue

import Data.Argonaut
  ( class DecodeJson
  , class EncodeJson
  , decodeJson
  , encodeJson
  )
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.FoldableWithIndex
  ( class FoldableWithIndex
  , foldMapWithIndex
  , foldlWithIndex
  , foldrWithIndex
  )
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (maybe)
import Data.Set (Set)
import Data.Tuple (uncurry)
import Data.Unfoldable (class Unfoldable)

data Bimap k v = UnsafeBimap (Map k v) (Map v k)

derive instance (Eq k, Eq v) => Eq (Bimap k v)
derive instance (Ord k, Ord v) => Ord (Bimap k v)

instance (Show k, Show v) => Show (Bimap k v) where
  show (UnsafeBimap left _) = show left

instance (DecodeJson k, DecodeJson v, Ord k, Ord v) => DecodeJson (Bimap k v) where
  decodeJson = map fromMap <<< decodeJson
    where
    fromMap =
      (fromFoldable :: Array (Tuple k v) -> Bimap k v) <<< M.toUnfoldable

instance (EncodeJson k, EncodeJson v, Ord k, Ord v) => EncodeJson (Bimap k v) where
  encodeJson (UnsafeBimap left _) = encodeJson left

instance Foldable (Bimap k) where
  foldMap f (UnsafeBimap left _) = foldMap f left
  foldl f b (UnsafeBimap left _) = foldl f b left
  foldr f b (UnsafeBimap left _) = foldr f b left

instance FoldableWithIndex k (Bimap k) where
  foldMapWithIndex f (UnsafeBimap left _) = foldMapWithIndex f left
  foldlWithIndex f b (UnsafeBimap left _) = foldlWithIndex f b left
  foldrWithIndex f b (UnsafeBimap left _) = foldrWithIndex f b left

empty :: forall k v. Bimap k v
empty = UnsafeBimap M.empty M.empty

singleton :: forall k v. k -> v -> Bimap k v
singleton k v = UnsafeBimap (M.singleton k v) (M.singleton v k)

null :: forall k v. Bimap k v -> Boolean
null (UnsafeBimap left _) = M.isEmpty left

size :: forall k v. Bimap k v -> Int
size (UnsafeBimap left _) = M.size left

insert :: forall k v. Ord k => Ord v => k -> v -> Bimap k v -> Bimap k v
insert k v = unsafeInsert k v <<< deleteL k <<< deleteR v

tryInsert :: forall k v. Ord k => Ord v => k -> v -> Bimap k v -> Bimap k v
tryInsert k v bm
  | memberL k bm || memberR v bm = bm
  | otherwise = unsafeInsert k v bm

associated :: forall k v. Ord k => Ord v => Tuple k v -> Bimap k v -> Boolean
associated (Tuple k v) (UnsafeBimap left _) =
  maybe false (v == _) $ M.lookup k left

keysL :: forall k v. Bimap k v -> Set k
keysL (UnsafeBimap left _) = M.keys left

keysR :: forall k v. Bimap k v -> Set v
keysR (UnsafeBimap _ right) = M.keys right

memberL :: forall k v. Ord k => k -> Bimap k v -> Boolean
memberL k (UnsafeBimap left _) = M.member k left

memberR :: forall k v. Ord v => v -> Bimap k v -> Boolean
memberR v (UnsafeBimap _ right) = M.member v right

deleteL :: forall k v. Ord k => Ord v => k -> Bimap k v -> Bimap k v
deleteL k bm@(UnsafeBimap left right) =
  case M.pop k left of
    Nothing -> bm
    Just (Tuple v left') -> UnsafeBimap left' $ M.delete v right

deleteR :: forall k v. Ord k => Ord v => v -> Bimap k v -> Bimap k v
deleteR v bm@(UnsafeBimap left right) =
  case M.pop v right of
    Nothing -> bm
    Just (Tuple k right') -> UnsafeBimap (M.delete k left) right'

unsafeInsert :: forall k v. Ord k => Ord v => k -> v -> Bimap k v -> Bimap k v
unsafeInsert k v (UnsafeBimap left right) =
  UnsafeBimap (M.insert k v left) (M.insert v k right)

lookupL :: forall k v. Ord k => Ord v => k -> Bimap k v -> Maybe v
lookupL k (UnsafeBimap left _) = M.lookup k left

lookupR :: forall k v. Ord k => Ord v => v -> Bimap k v -> Maybe k
lookupR v (UnsafeBimap _ right) = M.lookup v right

fromFoldable
  :: forall t k v. Foldable t => Ord k => Ord v => t (Tuple k v) -> Bimap k v
fromFoldable = foldl (flip <<< uncurry $ insert) empty

toUnfoldable
  :: forall t k v. Unfoldable t => Bimap k v -> t (Tuple k v)
toUnfoldable (UnsafeBimap left _) = M.toUnfoldable left

swap :: forall k v. Bimap k v -> Bimap v k
swap (UnsafeBimap left right) = UnsafeBimap right left

swapped :: forall k v. (Bimap k v -> Bimap k v) -> Bimap v k -> Bimap v k
swapped f = swap <<< f <<< swap
