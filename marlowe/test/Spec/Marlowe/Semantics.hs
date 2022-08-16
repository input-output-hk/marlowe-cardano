
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections  #-}


module Spec.Marlowe.Semantics (
  tests
) where


import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import qualified Spec.Marlowe.Semantics.Entropy (tests)
import qualified Spec.Marlowe.Semantics.Functions (tests)
import qualified Spec.Marlowe.Semantics.Transaction (tests)


tests :: TestTree
tests =
  testGroup "Semantics"
    [
      Spec.Marlowe.Semantics.Entropy.tests
    , Spec.Marlowe.Semantics.Functions.tests
    , Spec.Marlowe.Semantics.Transaction.tests
    ]
