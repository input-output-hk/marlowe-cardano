module Contrib.Data.Foldable
  where

-- Useful alias for infix.
import Data.Foldable (foldlM)
foldMapFlipped :: Monoid m => Foldable f => f a -> (a -> m) -> m
foldMapFlipped = flip foldMap

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

foldMapMFlipped :: Monoid b => Monad m => Foldable f => f a -> (a -> m b) -> m b
foldMapMFlipped = flip foldMapM

anyFlipped :: [a] -> (a -> Bool) -> Bool
anyFlipped = flip any

