module Language.Marlowe.CLI.Data.Foldable
  where

tillFirstMatch :: Foldable f => f a -> (a -> Maybe b) -> Maybe b
tillFirstMatch l match = do
  let
    step a acc = case match a of
      res@(Just _) -> res
      _            -> acc
  foldr step Nothing l

-- | Use `foldMap` in `for` like fashion.
foldMapFlipped :: Foldable t => Monoid m => t a -> (a -> m) -> m
foldMapFlipped = flip foldMap
