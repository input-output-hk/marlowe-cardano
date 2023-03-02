-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Compare the `PlutusTx.AssocMap` to Isabelle's `MList`.
--
-----------------------------------------------------------------------------


module Spec.Marlowe.Plutus.MList
  ( -- * Testing
    tests
  ) where


import Data.Function (on)
import Data.List (nubBy, sortBy)
import Data.Maybe (fromMaybe, isJust)
import Prelude hiding (lookup)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)
import Test.Tasty.QuickCheck (Arbitrary(..), Gen, Property, elements, forAll, property, testProperty)

import qualified PlutusTx.AssocMap as AM
  (Map, delete, empty, fromList, insert, lookup, member, null, singleton, toList, unionWith)
import qualified PlutusTx.Eq as P (Eq)


-- | An association list in Isabelle.
type MList a b = [(a, b)]


-- | Empty Isabelle `MList`.
--
-- @
-- definition empty :: "('a::linorder \<times> 'b) list" where
--   "empty = Nil"
-- @
empty :: MList a b
empty = []


{-# ANN insert "HLint: ignore Use guards" #-}
{-# ANN insert "HLint: ignore Use list literal" #-}

-- | Insert into Isabelle `MList`.
--
-- @
-- fun insert :: "'a::linorder \<Rightarrow> 'b \<Rightarrow> ('a \<times> 'b) list \<Rightarrow> ('a \<times> 'b) list" where
--   "insert a b Nil = Cons (a, b) Nil" |
--   "insert a b (Cons (x, y) z) =
--     (if a < x
--      then (Cons (a, b) (Cons (x, y) z))
--      else (if a > x
--            then (Cons (x, y) (insert a b z))
--            else (Cons (x, b) z)))"
-- @
insert :: Ord a => a -> b -> MList a b -> MList a b
insert a b [] = (a, b) : []
insert a b ((x, y) : z) =
  if a < x
    then (a, b) : (x, y) : z
    else if a > x
      then (x, y) : insert a b z
      else (x, b) : z


{-# ANN delete "HLint: ignore Use guards" #-}

-- | Delete from Isabelle `MList`.
--
-- @
-- fun delete :: "'a::linorder \<Rightarrow> ('a \<times> 'b) list \<Rightarrow> ('a \<times> 'b) list" where
--   "delete a Nil = Nil" |
--   "delete a (Cons (x, y) z) =
--     (if a = x
--      then z
--      else (if a > x
--            then (Cons (x, y) (delete a z))
--            else (Cons (x, y) z)))"
-- @
delete :: Ord a => a -> MList a b -> MList a b
delete _a [] = []
delete a ((x, y) : z) =
  if a == x
    then z
    else if a > x
      then (x, y) : delete a z
      else (x, y) : z


{-# ANN lookup "HLint: ignore Use guards" #-}

-- | Lookup in Isabelle `MList`.
--
-- @
-- fun lookup :: "'a::linorder \<Rightarrow> ('a \<times> 'b) list \<Rightarrow> 'b option" where
--   "lookup a Nil = None" |
--   "lookup a (Cons (x, y) z) =
--     (if a = x
--      then Some y
--      else (if a > x
--            then lookup a z
--            else None))"
-- @
lookup :: Ord a => a -> MList a b -> Maybe b
lookup _a [] = Nothing
lookup a ((x, y) : z) =
  if a == x
    then Just y
    else if a > x
      then lookup a z
      else Nothing


{-# ANN unionWith "HLint: ignore Use guards" #-}

-- | Union with Isabelle `MList`.
--
-- @
-- fun unionWith :: "('b \<Rightarrow> 'b \<Rightarrow> 'b) \<Rightarrow> ('a \<times> 'b) list \<Rightarrow>
--                   ('a \<times> 'b) list \<Rightarrow> (('a::linorder) \<times> 'b) list" where
--   "unionWith f (Cons (x, y) t) (Cons (x2, y2) t2) =
--     (if x < x2
--      then Cons (x, y) (unionWith f t (Cons (x2, y2) t2))
--      else (if x > x2
--            then Cons (x2, y2) (unionWith f (Cons (x, y) t) t2)
--            else Cons (x, f y y2) (unionWith f t t2)))" |
--   "unionWith f Nil l = l" |
--   "unionWith f l Nil = l"
-- @
unionWith :: Ord a => (b -> b -> b) -> MList a b -> MList a b -> MList a b
unionWith f ((x, y) : t) ((x2, y2) : t2) =
  if x < x2
    then (x, y) : unionWith f t ((x2, y2) : t2)
    else if x > x2
      then (x2, y2) : unionWith f ((x, y) : t) t2
      else (x, f y y2) : unionWith f t t2
unionWith _f [] l = l
unionWith _f l [] = l


-- | Find with default for Isabelle `MList`.
--
-- @
-- fun findWithDefault :: "'b \<Rightarrow> 'a \<Rightarrow> (('a::linorder) \<times> 'b) list \<Rightarrow> 'b" where
--   "findWithDefault d k l = (case lookup k l of
--                               None \<Rightarrow> d
--                             | Some x \<Rightarrow> x)"
-- @
findWithDefault :: Ord a => b -> a -> MList a b -> b
findWithDefault d k l =
  case lookup k l of
    Nothing -> d
    Just x -> x


-- | Run tests.
tests :: TestTree
tests =
  testGroup "MList vs AssocMap"
    [
      testCase     "`empty` is equivalent"           checkEmpty
    , testProperty "`null` is equivalent"            checkNull
    , testProperty "`singleton` is equivalent"       checkSingleton
    , testProperty "`insert` is equivalent"          checkInsert
    , testProperty "`delete` is equivalent"          checkDelete
    , testProperty "`lookup` is equivalent"          checkLookup
    , testProperty "`member` is equivalent"          checkMember
    , testProperty "`unionWith` is equivalent"       checkUnionWith
    , testProperty "`findWithDefault` is equivalent" checkFindWithDefault
    ]


-- | Key type for testing.
type Key = Integer


-- | Value type for testing.
type Value = [()]


-- | Generate a sorted `MList` with no duplicate keys.
arbitraryMList :: Gen (MList Key Value, AM.Map Key Value)
arbitraryMList =
  do
    items <- nubBy ((==) `on` fst) <$> arbitrary
    pure (sortBy (compare `on` fst) items, AM.fromList items)


-- | Compare an `MList` to an `AssocMap`, ignoring ordering.
equivalent :: Ord a => P.Eq a => Eq b => MList a b -> AM.Map a b -> Bool
equivalent mlist assocmap =
  mlist == sortBy (compare `on` fst) (AM.toList assocmap)
    && and [AM.lookup a assocmap == Just b | (a, b) <- mlist]
    && and [lookup a mlist == Just b | (a, b) <- AM.toList assocmap]


-- | Compare `empty` for `MList` and `AssocMap`, provided the `MList` is sorted and neither contains duplicate keys.
checkEmpty :: Assertion
checkEmpty = assertBool "Empty MList and AssocMap" $ (empty :: MList Key Value) `equivalent` AM.empty


-- | Compare `null` for `MList` and `AssocMap`, provided the `MList` is sorted and neither contains duplicate keys.
checkNull :: Property
checkNull = property $ do
  let
    gen = do
      (mlist, assocmap) <- arbitraryMList
      pure (mlist, assocmap)
  forAll gen
    $ \(mlist, assocmap) -> (== empty) mlist == AM.null assocmap


-- | Compare `singleton` for `MList` and `AssocMap`, provided the `MList` is sorted and neither contains duplicate keys.
checkSingleton :: Property
checkSingleton = property $ do
  let
    gen = do
      a <- arbitrary :: Gen Key
      b <- arbitrary :: Gen Value
      pure (a, b)
  forAll gen
    $ \(a, b) -> [(a, b)] `equivalent` AM.singleton a b


-- | Compare `insert` for `MList` and `AssocMap`, provided the `MList` is sorted and neither contains duplicate keys.
checkInsert :: Property
checkInsert = property $ do
  let
    gen = do
      (mlist, assocmap) <- arbitraryMList
      a <- arbitrary
      b <- arbitrary
      pure (mlist, assocmap, a, b)
  forAll gen
    $ \(mlist, assocmap, a, b) -> insert a b mlist `equivalent` AM.insert a b assocmap


-- | Compare `delete` for `MList` and `AssocMap`, provided the `MList` is sorted and neither contains duplicate keys.
checkDelete :: Property
checkDelete = property $ do
  let
    gen = do
      (mlist, assocmap) <- arbitraryMList
      a <- arbitrary
      pure (mlist, assocmap, a)
  forAll gen
    $ \(mlist, assocmap, a) -> delete a mlist `equivalent` AM.delete a assocmap


-- | Compare `lookup` for `MList` and `AssocMap`, provided the `MList` is sorted and neither contains duplicate keys.
checkLookup :: Property
checkLookup = property $ do
  let
    gen = do
      (mlist, assocmap) <- arbitraryMList
      a <- arbitrary
      pure (mlist, assocmap, a)
  forAll gen
    $ \(mlist, assocmap, a) -> lookup a mlist == AM.lookup a assocmap


-- | Compare `member` for `MList` and `AssocMap`, provided the `MList` is sorted and neither contains duplicate keys.
checkMember :: Property
checkMember = property $ do
  let
    gen = do
      (mlist, assocmap) <- arbitraryMList
      a <- arbitrary
      pure (mlist, assocmap, a)
  forAll gen
    $ \(mlist, assocmap, a) -> isJust (lookup a mlist) == AM.member a assocmap


-- | Compare `unionWith` for `MList` and `AssocMap`, provided the `MList` is sorted and neither contains duplicate keys.
checkUnionWith :: Property
checkUnionWith = property $ do
  let
    gen = do
      (mlist, assocmap) <- arbitraryMList
      (mlist', assocmap') <- arbitraryMList
      function <- elements ["concat", "shortest", "longest", "first", "second"]
      pure (mlist, assocmap, mlist', assocmap', function)
  forAll gen
    $ \(mlist, assocmap, mlist', assocmap', function) ->
      let
        f = case function of
              "shortest" -> \x y -> if length x < length y then x else y
              "longest"  -> \x y -> if length x > length y then x else y
              "first"    -> const
              "second"   -> const id
              _          -> (<>)
      in
        unionWith f mlist mlist' `equivalent` AM.unionWith f assocmap assocmap'


-- | Compare `findWithDefault` for `MList` and `AssocMap`, provided the `MList` is sorted and neither contains duplicate keys.
checkFindWithDefault :: Property
checkFindWithDefault = property $ do
  let
    gen = do
      (mlist, assocmap) <- arbitraryMList
      a <- arbitrary
      b <- arbitrary
      pure (mlist, assocmap, a, b)
  forAll gen
    $ \(mlist, assocmap, a, b) -> findWithDefault b a mlist == fromMaybe b (AM.lookup a assocmap)
