module Contrib.Data.List.Infinite.Finalize where

import Prelude

import Data.Foldable (class Foldable, foldl)
import Data.List (List(..), reverse, uncons) as List
import Data.List.Infinite (List, uncons) as Infinite
import Data.Maybe (Maybe(..))
import Data.Profunctor.Strong ((&&&))
import Data.Tuple (Tuple(..), fst)
import Data.Unfoldable (class Unfoldable, unfoldr)

take :: forall f. Unfoldable f => Int -> Infinite.List ~> f
take n xs = unfoldr step (Tuple xs 0)
  where
  step (Tuple xs' i)
    | i < n = case Infinite.uncons xs' of
        { head, tail } -> Just (Tuple head (Tuple tail (i + 1)))
    | otherwise = Nothing

zip
  :: forall a b f g
   . Foldable f
  => Unfoldable g
  => Infinite.List a
  -> f b
  -> g (Tuple a b)
zip xs ys = unfoldr (map (_.head &&& _.tail) <<< List.uncons)
  $ List.reverse <<< fst
  $ foldl step (Tuple List.Nil xs) ys
  where
  -- | Is it possible to provide a hylomorphism which
  -- | skips intermediate list buildup here?
  step (Tuple acc xs') y = case Infinite.uncons xs' of
    { head, tail } -> Tuple (List.Cons (Tuple head y) acc) tail

