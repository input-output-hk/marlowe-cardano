-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Test the `PlutusTx.AssocMap` functions used by the Marlowe validators.
--
-----------------------------------------------------------------------------


module Spec.Marlowe.Plutus.AssocMap
  ( -- * Testing
    tests
  ) where


import Spec.Marlowe.Semantics.Arbitrary (arbitraryAssocMap)
import Spec.Marlowe.Semantics.AssocMap (assocMapDelete, assocMapEq, assocMapInsert, assocMapLookup, assocMapMember)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary(..), Gen, Property, elements, forAll, property, testProperty)

import qualified PlutusTx.AssocMap as AM (Map, delete, empty, fromList, insert, lookup, member, null, singleton, toList)


-- | Run tests.
tests :: TestTree
tests =
  testGroup "AssocMap"
    [
      testProperty "`fromList` is the inverse of toList" checkFromList
    , testProperty "`toList` is the inverse of fromList" checkToList
    , testProperty "`empty` creates an empty list"       checkEmpty
    , testProperty "`null` detects an empty list"        checkNull
    , testProperty "`singleton` creates a one-item list" checkSingleton
    , testProperty "`member` detects a key"              checkMember
    , testProperty "`lookup` finds a value"              checkLookup
    , testProperty "`insert` adds a value"               checkInsert
    , testProperty "`delete` removes a key"              checkDelete
    ]


-- | `fromList` is the inverse of `toList`.
checkFromList :: Property
checkFromList =
  property
    . forAll (arbitrary :: Gen [(Int, Double)])
    $ \x -> AM.toList (AM.fromList x) == x


-- | `toList` is the inverse of `fromList`.
checkToList :: Property
checkToList =
  property
    . forAll (arbitraryAssocMap arbitrary arbitrary :: Gen (AM.Map Integer Double))
    $ \x -> AM.fromList (AM.toList x) `assocMapEq` x


-- | `empty` creates an empty list.
checkEmpty :: Property
checkEmpty =
  property . null $ AM.toList AM.empty


-- | `null` detects an empty list.
checkNull :: Property
checkNull =
  property
    . forAll (arbitraryAssocMap arbitrary arbitrary :: Gen (AM.Map Integer Double))
    $ \x -> AM.null x == null (AM.toList x)


-- | `singleton` creates a one-item list.
checkSingleton :: Property
checkSingleton =
  property
    . forAll (arbitrary :: Gen (Integer, Double))
    $ \(k, v) ->
      AM.toList (AM.singleton k v) == [(k, v)]


-- | Generate a key and value that are perhaps in a map.
perhapsEntry :: Eq k => Arbitrary k => Arbitrary v => Gen (k, v, AM.Map k v)
perhapsEntry =
  do
    isKey <- arbitrary
    isValue <- arbitrary
    x <- arbitraryAssocMap arbitrary arbitrary
    k <- if isKey   && not (null $ AM.toList x) then elements $ fst <$> AM.toList x else arbitrary
    v <- if isValue && not (null $ AM.toList x) then elements $ snd <$> AM.toList x else arbitrary
    pure (k, v, x)


-- | Generate a key that is perhaps in a map.
perhapsMember :: Eq k => Arbitrary k => Arbitrary v => Gen (k, AM.Map k v)
perhapsMember =
  do
    (k, _, x) <- perhapsEntry
    pure (k, x)


-- | `member` detects a key.
checkMember :: Property
checkMember =
  property
    . forAll (perhapsMember :: Gen (Integer, AM.Map Integer Double))
    $ \(k, x) -> assocMapMember k x == AM.member k x


-- | `lookup` finds a value.
checkLookup :: Property
checkLookup =
  property
    . forAll (perhapsMember :: Gen (Integer, AM.Map Integer Double))
    $ \(k, x) -> assocMapLookup k x == AM.lookup k x


-- | `insert` adds a value.
checkInsert :: Property
checkInsert =
  property
    . forAll (perhapsEntry :: Gen (Integer, Double, AM.Map Integer Double))
    $ \(k, v, x) -> AM.insert k v x `assocMapEq` assocMapInsert k v x


-- | `delete` removes a value.
checkDelete :: Property
checkDelete =
  property
    . forAll (perhapsMember :: Gen (Integer, AM.Map Integer Double))
    $ \(k, x) -> AM.delete k x `assocMapEq` assocMapDelete k x
