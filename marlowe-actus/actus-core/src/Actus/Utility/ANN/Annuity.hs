module Actus.Utility.ANN.Annuity
  ( annuity
  ) where

import Data.List (foldl', tails)

-- |annuity amount function (A), as described in section 3.8 in the
-- ACTUS reference v1.1
annuity :: Fractional a =>
     a   -- ^ actual interest rate
  -> [a] -- ^ ti
  -> a
annuity r ti = numerator / denominator

  where
    numerator   = _product $ map ((+1).(*r)) ti
    denominator = _sum (map _product $ tails $ map ((+1).(*r)) ti)

    -- note that _product [] == 1
    _product :: Num a => [a] -> a
    _product = foldl' (*) 1

    _sum :: Num a => [a] -> a
    _sum = foldl' (+) 0
