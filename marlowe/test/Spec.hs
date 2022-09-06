-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Marlowe tests.
--
-----------------------------------------------------------------------------


{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}


module Main(
-- * Testing
  main
) where


import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import qualified Spec.Marlowe.Marlowe (prop_contractJsonLoops, prop_marloweParamsJsonLoops, prop_noFalsePositives,
                                       tests)
import qualified Spec.Marlowe.Plutus (tests)
import qualified Spec.Marlowe.Semantics (tests)


-- | Timeout seconds for static analysis, which can take so much time on a complex contract
--   that it exceeds hydra/CI resource limits, see SCP-4267.
timeout :: Maybe Int
#ifdef STATIC_ANALYSIS_TIMEOUT
timeout = Just STATIC_ANALYSIS_TIMEOUT
#else
timeout = Nothing
#endif


-- | Entry point for the tests.
main :: IO ()
main = defaultMain tests


-- | Run the tests.
tests :: TestTree
tests =
  testGroup "Marlowe"
    [
      Spec.Marlowe.Marlowe.tests
    , testGroup "Static Analysis"
      [
        testProperty "No false positives" $ Spec.Marlowe.Marlowe.prop_noFalsePositives timeout
      ]
    , testGroup "JSON Serialisation"
      [
        testProperty "Serialise deserialise Contract loops" Spec.Marlowe.Marlowe.prop_contractJsonLoops
      , testProperty "Serialise deserialise MarloweParams loops" Spec.Marlowe.Marlowe.prop_marloweParamsJsonLoops
      ]
    , Spec.Marlowe.Semantics.tests
    , Spec.Marlowe.Plutus.tests
    ]
