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
  main,
  tests,
) where

import Test.Tasty (TestTree, defaultMain, testGroup)

import qualified Spec.Marlowe.StaticAnalysis.Regression (tests)

-- | Entry point for the tests.
main :: IO ()
main = defaultMain tests

-- | Run the tests.
tests :: TestTree
tests =
  testGroup
    "StaticAnalysis"
    [ Spec.Marlowe.StaticAnalysis.Regression.tests
    ]
