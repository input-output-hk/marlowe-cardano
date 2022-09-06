-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Test the `Plutus.V1.Ledger.Value` functions used by the Marlowe validators.
--
-----------------------------------------------------------------------------


{-# LANGUAGE TupleSections #-}


module Spec.Marlowe.Plutus.Value (
  tests
) where


import Data.List (union)
import Plutus.V1.Ledger.Value (CurrencySymbol, TokenName, Value (..), geq, leq, singleton, valueOf)
import Spec.Marlowe.Plutus.Arbitrary ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary (..), Property, arbitrary, forAll, property, testProperty, (===))

import qualified PlutusTx.AssocMap as AM


-- | Run tests.
tests :: TestTree
tests =
  testGroup "Plutus Value"
    [
      testProperty "mempty has no tokens"              checkMempty
    , testProperty "mappend sums corresponding tokens" checkMappend
    , testProperty "leq is partial order"              checkLeq
    , testProperty "geq is partial order"              checkGeq
    , testProperty "valueOf extracts quanity"          checkValueOf
    , testProperty "singleton creates a single token"  checkSingleton
    ]


-- | Check that the `mempty` instance for `Value` has no coins.
checkMempty :: Property
checkMempty = getValue mempty === AM.empty


-- | Extract the currency symbols and token names in a value.
tokens :: Value -> [(CurrencySymbol, TokenName)]
tokens = concatMap (\(c, ts) -> fmap (c, ) . fmap fst $ AM.toList ts) . AM.toList . getValue


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


-- | Check that `leq` is a partial ordering requiring that quantity of each token in the first
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


-- | Check the `valueOf` function.
checkValueOf :: Property
checkValueOf =
  property
    . forAll arbitrary
    $ \(v, c, t) ->
      valueOf v c t == valueOf' v c t


-- | Check the `singleton` function.
checkSingleton :: Property
checkSingleton =
  property
    . forAll arbitrary
    $ \(c, t, i) ->
      let
        v = singleton c t i
      in
        valueOf v c t == i && length (AM.toList $ getValue v) == 1
