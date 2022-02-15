module Contrib.Data.Unfoldable where

import Prelude

import Data.Foldable (length)
import Data.FoldableWithIndex (class FoldableWithIndex, foldrWithIndex)
import Data.List (List(..), uncons) as List
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (class Unfoldable, unfoldr)

type Move = { from :: Int, to :: Int }

-- | This function resides here in the `Unfoldable` namespace because
-- | `unfoldable` depends on `foldable-traversable` package and `Data`
-- | namespace seems to general for it.
move
  :: forall a f g
   . FoldableWithIndex Int f
  => Unfoldable g
  => Move
  -> f a
  -> Maybe (g a)
move { from, to } foldable = do
  when (to < 0 || to > length foldable)
    Nothing
  let
    foldStep idx elem acc
      | idx == from = acc { elem = Just elem }
      | otherwise = acc { items = List.Cons elem acc.items }

    { elem, items } = foldrWithIndex foldStep { elem: Nothing, items: List.Nil }
      foldable
    unfoldStep seed@{ idx, elem, items }
      | idx == to = Just (elem /\ seed { idx = idx + 1 })
      | otherwise = do
          { head, tail } <- List.uncons items
          Just (head /\ seed { idx = idx + 1, items = tail })
  -- | There is no UnfoldableWithIndex unfortunatelly... yet:
  -- | https://github.com/purescript/purescript-foldable-traversable/issues/84
  -- | so we have to carry the index ourselves.
  seed <- { idx: 0, elem: _, items } <$> elem
  pure $ unfoldr unfoldStep seed
