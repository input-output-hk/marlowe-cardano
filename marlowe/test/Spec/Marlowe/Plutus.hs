-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Tests of Marlowe's Plutus implementation.
--
-----------------------------------------------------------------------------


module Spec.Marlowe.Plutus (
-- * Testing
tests
) where


import Test.Tasty (TestTree, testGroup)

import qualified Spec.Marlowe.Plutus.Specification (tests)


-- | Run tests.
tests :: TestTree
tests =
  testGroup "Plutus"
    [
      Spec.Marlowe.Plutus.Specification.tests
    ]

