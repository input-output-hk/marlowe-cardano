module Contrib.Data.Foldable
  where

import Data.Foldable (foldlM)

-- Useful aliases for infix usage.

foldMapFlipped :: Monoid m => Foldable f => f a -> (a -> m) -> m
foldMapFlipped = flip foldMap

foldMapMFlipped :: Monoid b => Monad m => Foldable f => f a -> (a -> m b) -> m b
foldMapMFlipped = flip foldMapM

anyFlipped :: [a] -> (a -> Bool) -> Bool
anyFlipped = flip any

foldMapM
  :: (Monad m, Monoid w, Foldable t)
  => (a -> m w)
  -> t a
  -> m w
foldMapM f = foldlM
  (\acc a -> do
    w <- f a
    pure $! acc <> w)
  mempty

