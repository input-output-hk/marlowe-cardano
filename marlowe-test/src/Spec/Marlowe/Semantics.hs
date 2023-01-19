-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Tests of Marlowe semantics.
--
-----------------------------------------------------------------------------


module Spec.Marlowe.Semantics
  ( -- * Testing
    tests
  ) where


import Test.Tasty (TestTree, testGroup)

import qualified Spec.Marlowe.Semantics.Compute (tests)
import qualified Spec.Marlowe.Semantics.Entropy (tests)
import qualified Spec.Marlowe.Semantics.Functions (tests)
import qualified Spec.Marlowe.Semantics.Golden (tests)


-- | Run the tests.
tests :: TestTree
tests =
  testGroup "Semantics"
    [
      Spec.Marlowe.Semantics.Entropy.tests
    , Spec.Marlowe.Semantics.Functions.tests
    , Spec.Marlowe.Semantics.Compute.tests
    , Spec.Marlowe.Semantics.Golden.tests
    ]
