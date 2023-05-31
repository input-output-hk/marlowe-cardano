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


{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}


module Main
  ( -- * Testing
    main
  ) where


import Spec.Marlowe.Reference (ReferencePath, readReferencePaths)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.QuickCheck

import qualified Spec.Marlowe.Marlowe (prop_noFalsePositives, tests)
import qualified Spec.Marlowe.Plutus (tests)
import qualified Spec.Marlowe.Semantics (tests)
import qualified Spec.Marlowe.Serialization (tests)
import qualified Spec.Marlowe.Service.Isabelle (tests)


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
main = defaultMain . tests =<< readReferencePaths


-- | Run the tests.
tests :: [ReferencePath] -> TestTree
tests referencePaths =
  testGroup "Marlowe"
    [
      Spec.Marlowe.Marlowe.tests
    , testGroup "Static Analysis"
      [
        testProperty "No false positives" $ Spec.Marlowe.Marlowe.prop_noFalsePositives timeout
      ]
    , Spec.Marlowe.Serialization.tests
    , Spec.Marlowe.Semantics.tests
    , Spec.Marlowe.Plutus.tests referencePaths
    , Spec.Marlowe.Service.Isabelle.tests
    ]
