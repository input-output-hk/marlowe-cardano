-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--

-----------------------------------------------------------------------------

-- | Tests of Marlowe semantics.
module Spec.Marlowe.StaticAnalysis (
  -- * Testing
  tests,
) where

import Test.Tasty (TestTree, testGroup)

import qualified Spec.Marlowe.StaticAnalysis.Regression (tests)

-- | Run the tests.
tests :: TestTree
tests =
  testGroup
    "StaticAnalysis"
    [ Spec.Marlowe.StaticAnalysis.Regression.tests
    ]
