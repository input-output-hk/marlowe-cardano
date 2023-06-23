module Contrib.Data.Foldable where

import Data.Foldable (foldlM)
import Data.Foldable.WithIndex (FoldableWithIndex, ifoldlM)

-- Useful aliases for infix usage.

foldMapFlipped :: (Monoid m) => (Foldable f) => f a -> (a -> m) -> m
foldMapFlipped = flip foldMap

anyFlipped :: [a] -> (a -> Bool) -> Bool
anyFlipped = flip any

foldMapM
  :: (Monad m, Monoid w, Foldable t)
  => (a -> m w)
  -> t a
  -> m w
foldMapM f =
  foldlM
    ( \acc a -> do
        w <- f a
        pure $! acc <> w
    )
    mempty

foldMapMFlipped :: (Monoid b) => (Monad m) => (Foldable f) => f a -> (a -> m b) -> m b
foldMapMFlipped = flip foldMapM

ifoldMapM
  :: (Monad m, Monoid w, FoldableWithIndex i t)
  => (i -> a -> m w)
  -> t a
  -> m w
ifoldMapM f =
  ifoldlM
    ( \i acc a -> do
        w <- f i a
        pure $! acc <> w
    )
    mempty

ifoldMapMFlipped :: (Monad m, Monoid w, FoldableWithIndex i t) => t a -> (i -> a -> m w) -> m w
ifoldMapMFlipped = flip ifoldMapM
