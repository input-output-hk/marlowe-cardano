
{-# LANGUAGE DataKinds #-}

{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE MultiParamTypeClasses #-}



{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE RankNTypes #-}


module Language.Marlowe.Core.V1.Semantics.Next.Applicables.Bound
  ( compactAdjoinedBounds
  , difference
  , mkBound
  , overlapWith
  ) where

import Control.Monad (join)
import Data.List (tails)
import Data.Maybe (catMaybes)
import qualified Data.Range as R
import Language.Marlowe.Core.V1.Semantics.Types (Bound(..))
import Prelude

overlapWith :: Bound -> Bound -> Bool
overlapWith a b = R.rangesOverlap (toRange a) (toRange b)

compactAdjoinedBounds :: [Bound]  -> [Bound]
compactAdjoinedBounds [x] = [x]
compactAdjoinedBounds l
  = let adjoinedBoundsUnionized = join [ compact2AdjoinedBounds x y | (x:ys) <- tails l, y <- ys]
    in toBounds . R.mergeRanges $ toRange <$> adjoinedBoundsUnionized

compact2AdjoinedBounds :: Bound -> Bound -> [Bound]
compact2AdjoinedBounds (Bound a b) (Bound a' b')  |  b + 1 == a' = [Bound a b']
compact2AdjoinedBounds (Bound a b) (Bound a' b')  |  b' + 1 == a = [Bound a' b]
compact2AdjoinedBounds a b = [a,b]

difference
    :: [Bound]
    -> [Bound]
    -> [Bound]
difference xs ys = toBounds <$> R.difference (toRange <$> xs) $ (toRange <$> ys)


toRange :: Bound -> R.Range Integer
toRange (Bound a b) | a == b = R.SingletonRange a
toRange (Bound a b) = R.SpanRange (R.Bound a R.Inclusive) (R.Bound b R.Inclusive)

toBounds :: [R.Range Integer] -> [Bound]
toBounds xs = catMaybes $ toBound <$> xs

toBound :: R.Range Integer -> Maybe Bound
toBound (R.SpanRange (R.Bound a R.Inclusive)  (R.Bound b R.Inclusive))  = mkBound  a     b
toBound (R.SpanRange (R.Bound a R.Inclusive)  (R.Bound b R.Exclusive))  = mkBound  a    (b-1)
toBound (R.SpanRange (R.Bound a R.Exclusive ) (R.Bound b R.Inclusive))  = mkBound (a+1)  b
toBound (R.SpanRange (R.Bound a R.Exclusive ) (R.Bound b R.Exclusive))  = mkBound (a+1) (b-1)
toBound (R.SingletonRange a) = mkBound  a a
toBound x = error ("unexpected range value" ++ show x) -- not dealing with infinite ranges

mkBound ::  Integer -> Integer -> Maybe Bound
mkBound a b | a <= b = Just $ Bound a b
mkBound _ _ = Nothing -- Empty Set

