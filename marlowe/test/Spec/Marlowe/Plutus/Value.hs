-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Test the `Plutus.V1.Ledger.Value.Value` functions used by the Marlowe validators.
--
-----------------------------------------------------------------------------


{-# LANGUAGE TupleSections #-}


module Spec.Marlowe.Plutus.Value
  ( -- * Testing
    tests
  ) where


import Data.List (permutations, union)
import Plutus.V1.Ledger.Value (geq, leq, valueOf)
import Plutus.V2.Ledger.Api (CurrencySymbol, TokenName, Value(..), singleton)
import PlutusTx.Numeric (zero)
import Spec.Marlowe.Plutus.Arbitrary ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary(..), Property, elements, forAll, property, testProperty, (===))

import qualified PlutusTx.AssocMap as AM (empty, fromList, toList)
import qualified PlutusTx.Eq as P ((==))


-- | Run tests.
tests :: TestTree
tests =
  testGroup "Value"
    [
      testProperty "`mempty` has no tokens"              checkMempty
    , testProperty "`mappend` sums corresponding tokens" checkMappend
    , testProperty "`(==)` detects equality"             checkEq
    , testProperty "`leq` is partial order"              checkLeq
    , testProperty "`geq` is partial order"              checkGeq
    , testProperty "`valueOf` extracts quanity"          checkValueOf
    , testProperty "`singleton` creates a single token"  checkSingleton
    , testProperty "`zero` has no value"                 checkZero
    ]


-- | Check that the `mempty` instance for `Value` has no coins.
checkMempty :: Property
checkMempty = getValue mempty === AM.empty


-- | Extract the currency symbols and token names in a value.
tokens :: Value -> [(CurrencySymbol, TokenName)]
tokens = concatMap (\(c, ts) -> (c, ) . fst <$> AM.toList ts) . AM.toList . getValue


-- | Check that the `mappend` instance for `Value` sums quantities of corresponding tokens.
checkMappend :: Property
checkMappend =
  property
    . forAll arbitrary
    $ \(x, y) ->
      let
        z = x <> y
        check (c, t) = valueOf' z c t == valueOf' x c t + valueOf' y c t
      in
        all check . foldl1 union $ tokens <$> [x, y, z]


-- | Check equality operator on `Value`.
checkEq :: Property
checkEq =
  property
    $ let
        gen = do
          isEqual <- arbitrary
          x <- arbitrary
          x' <- elements . permutations . AM.toList $ getValue x
          x'' <- Value
                   . AM.fromList
                   <$> sequence
                   [
                     (c, ) . AM.fromList <$> elements (permutations $ AM.toList ts)
                   |
                     (c, ts) <- x'
                   ]
          y <- if isEqual then pure x'' else arbitrary
          pure (x, y)
      in forAll gen
        $ \(x, y) ->
          let
            check (c, t) = valueOf' x c t == valueOf' y c t
          in
            (x P.== y) == (all check . foldl1 union $ tokens <$> [x, y])


-- | Check that `leq` is a partial ordering requiring that quantity of each token in the first
--   operand is less than or equal to quanity of the corresponding token in the second operand,
--   where a missing token in one operand represents a zero quantity.
checkLeq :: Property
checkLeq =
  property
    . forAll arbitrary
    $ \(x, y) ->
      let
        check (c, t) = valueOf' x c t <= valueOf' y c t
      in
        (x `leq` y) == (all check . foldl1 union $ tokens <$> [x, y])


-- | Check that `geq` is a partial ordering requiring that quantity of each token in the first
--   operand is less than or equal to quanity of the corresponding token in the second operand,
--   where a missing token in one operand represents a zero quantity.
checkGeq :: Property
checkGeq =
  property
    . forAll arbitrary
    $ \(x, y) ->
      let
        check (c, t) = valueOf' x c t >= valueOf' y c t
      in
        (x `geq` y) == (all check . foldl1 union $ tokens <$> [x, y])


-- | An independent implementation of `valueOf`.
valueOf' :: Value -> CurrencySymbol -> TokenName -> Integer
valueOf' v c t =
  let
    v' = fmap snd . filter ((== c) . fst) . AM.toList $ getValue v
    v'' = fmap snd . filter ((== t) . fst) . AM.toList <$> v'
  in
    sum $ concat v''


-- | `valueOf` correctly extract the value of a currency and token name.
checkValueOf :: Property
checkValueOf =
  property
    . forAll arbitrary
    $ \(v, c, t) ->
      valueOf v c t == valueOf' v c t


-- | `singleton` creates a value for a single currency and token name.
checkSingleton :: Property
checkSingleton =
  property
    . forAll arbitrary
    $ \(c, t, i) ->
      let
        v = singleton c t i
      in
        valueOf v c t == i && length (AM.toList $ getValue v) == 1


-- | `zero` has not value.
checkZero :: Property
checkZero = getValue zero === AM.empty
