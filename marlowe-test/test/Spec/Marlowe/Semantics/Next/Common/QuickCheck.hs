module Spec.Marlowe.Semantics.Next.Common.QuickCheck (
  forAllSuchThat,
) where

import Test.QuickCheck (Arbitrary (..), Property, Testable, forAllShrink, suchThat)

-- | Test properties with shrinkage, applying a predicate to the generator and the shrink.
forAllSuchThat
  :: (Arbitrary a)
  => (Show a)
  => (Testable prop)
  => (a -> Bool)
  -- ^ The test-case predicate.
  -> (a -> prop)
  -- ^ The test.
  -> Property
  -- ^ The result of multiple applications of the test.
forAllSuchThat p = forAllShrink (arbitrary `suchThat` p) (filter p . shrink)
