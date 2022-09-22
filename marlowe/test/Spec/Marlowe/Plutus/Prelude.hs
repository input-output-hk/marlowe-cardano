-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Test the Plutus Prelude functions used by the Marlowe validators.
--
-----------------------------------------------------------------------------


module Spec.Marlowe.Plutus.Prelude
  ( -- * Testing
    tests
  ) where


import Data.List (find)
import Plutus.V2.Ledger.Api (DatumHash(..), TxId(..), TxOutRef(..), ValidatorHash(..))
import PlutusTx.Builtins.Class (stringToBuiltinByteString)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary(..), Gen, Property, elements, forAll, frequency, listOf, property, testProperty)

import qualified PlutusTx.Prelude as P (all, any, filter, find, fmap, foldMap, fromEnum, snd, (&&), (==))


-- | Run tests.
tests :: TestTree
tests =
  testGroup "Prelude"
    [
      testProperty "`snd` matches standard prelude"                   checkSnd
    , testProperty "`(&&)` matches standard prelude"                  checkAnd
    , testProperty "`any` matches standard prelude"                   checkAny
    , testProperty "`all` matches standard prelude"                   checkAll
    , testProperty "`fmap` matches standard prelude"                  checkFmap
    , testProperty "`foldMap` matches standard prelude"               checkFoldMap
    , testProperty "`filter` matches standard prelude"                checkFilter
    , testProperty "`find` matches standard prelude"                  checkFind
    , testProperty "`fromEnum` on `Bool`"                             checkBoolEnum
    , testProperty "`instance Eq (Maybe a)` matches standard prelude" checkEqMaybe
    , testProperty "`instance Eq DatumHash`"                          checkEqDatumHash
    , testProperty "`instance Eq ValidatorHash`"                      checkEqValidatorHash
    , testProperty "`instance Eq TxOutRef`"                           checkEqTxOutRef
    ]


-- | Compare `&&` to standard prelude.
checkAnd :: Property
checkAnd =
  property
    . forAll arbitrary
    $ \(x, y) -> (x P.&& y) == (x && y)



-- | Compare `snd` to standard prelude.
checkSnd :: Property
checkSnd =
  property
    . forAll (arbitrary :: Gen (Double, Double))
    $ \x -> P.snd x == snd x


-- | Compare `any` to standard prelude.
checkAny :: Property
checkAny =
  property
    $ let
        gen =
          do
            ys <- arbitrary :: Gen [Double]
            bs <- listOf $ frequency [(10, pure False), (1, pure True)]
            xs' <- sequence [if b && not (null ys) then elements ys else arbitrary | b <- bs]
            pure (ys, xs')
      in
        forAll gen
          $ \(ys, xs) ->
            let
              f = flip elem ys
            in
              P.any f xs == any f xs


-- | Compare `all` to standard prelude.
checkAll :: Property
checkAll =
  property
    $ let
        gen =
          do
            ys <- arbitrary :: Gen [Double]
            bs <- listOf $ frequency [(1, pure False), (10, pure True)]
            xs' <- sequence [if b && not (null ys) then elements ys else arbitrary | b <- bs]
            pure (ys, xs')
      in
        forAll gen
          $ \(ys, xs) ->
            let
              f = flip elem ys
            in
              P.all f xs == all f xs


-- | Compare `fmap` to standard prelude.
checkFmap :: Property
checkFmap =
  property
    . forAll (arbitrary :: Gen [Double])
    $ \xs ->
      let
        f = (+ 10) . (* 2)
      in
        P.fmap f xs == fmap f xs


-- | Compare `foldMap` to standard prelude.
checkFoldMap :: Property
checkFoldMap =
  property
    . forAll (arbitrary :: Gen [Double])
    $ \xs ->
      let
        f x = [i * x + 2 | i <- [0..10]]
      in
        P.foldMap f xs == foldMap f xs


-- | Compare `filter` to standard prelude.
checkFilter :: Property
checkFilter =
  property
    . forAll (arbitrary :: Gen (Double, [Double]))
    $ \(y, xs) ->
      let
        f = (< y)
      in
        P.filter f xs == filter f xs


-- | Compare `find` to standard prelude.
checkFind :: Property
checkFind =
  property
    . forAll (arbitrary :: Gen (Double, [Double]))
    $ \(y, xs) ->
      let
        f = (< y)
      in
        P.find f xs == find f xs


-- | Check that `fromEnum True == 1` and `fromEnum False == 0`
checkBoolEnum :: Property
checkBoolEnum =
  property
    . forAll arbitrary
    $ \x ->
      fromInteger (P.fromEnum x) == fromEnum x
        && if x
             then P.fromEnum x == 1
             else P.fromEnum x == 0


-- | Compare `instance Eq (Maybe a)` to standard prelude.
checkEqMaybe :: Property
checkEqMaybe =
  property
    $ let
        gen =
          do
            x <- arbitrary :: Gen Integer
            isEqual <- arbitrary
            y <- if isEqual then pure x else arbitrary
            pure (x, y)
      in
        forAll gen
          $ \(x, y) -> (x P.== y) == (x == y)


-- | Check `instance Eq DatumHash`.
checkEqDatumHash :: Property
checkEqDatumHash =
  property
    . forAll arbitrary
    $ \(x, y) ->
      let
        x' = DatumHash $ stringToBuiltinByteString x
        y' = DatumHash $ stringToBuiltinByteString y
      in
        (x' P.== y') == (x == y)


-- | Check `instance Eq ValidatorHash`.
checkEqValidatorHash :: Property
checkEqValidatorHash =
  property
    . forAll arbitrary
    $ \(x, y) ->
      let
        x' = ValidatorHash $ stringToBuiltinByteString x
        y' = ValidatorHash $ stringToBuiltinByteString y
      in
        (x' P.== y') == (x == y)


-- | Check `instance Eq TxOutRef`.
checkEqTxOutRef :: Property
checkEqTxOutRef =
  property
    . forAll arbitrary
    $ \(x, i, y, j) ->
      let
        xi = TxOutRef (TxId $ stringToBuiltinByteString x) i
        yi = TxOutRef (TxId $ stringToBuiltinByteString y) j
      in
        (xi P.== yi) == (x == y && i == j)
