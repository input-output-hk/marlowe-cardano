module Spec.Marlowe.Semantics.Next.Common.QuickCheck
  ( forAll'
  ) where

import Test.QuickCheck (Arbitrary(shrink), Gen, Property, Testable, forAllShrink)


-- | Test properties with shrinkage.
forAll' :: Arbitrary a
        => Show a
        => Testable prop
        => Gen a        -- ^ The test-case generator.
        -> (a -> prop)  -- ^ The test.
        -> Property     -- ^ The result of multiple applications of the test.
forAll' = flip forAllShrink shrink
