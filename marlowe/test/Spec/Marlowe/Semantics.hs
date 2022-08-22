
module Spec.Marlowe.Semantics (
  tests
) where


import Test.Tasty (TestTree, testGroup)

import qualified Spec.Marlowe.Semantics.Compute (tests)
import qualified Spec.Marlowe.Semantics.Entropy (tests)
import qualified Spec.Marlowe.Semantics.Functions (tests)
import qualified Spec.Marlowe.Semantics.Golden (tests)


tests :: TestTree
tests =
  testGroup "Semantics"
    [
      Spec.Marlowe.Semantics.Entropy.tests
    , Spec.Marlowe.Semantics.Functions.tests
    , Spec.Marlowe.Semantics.Compute.tests
    , Spec.Marlowe.Semantics.Golden.tests
    ]
