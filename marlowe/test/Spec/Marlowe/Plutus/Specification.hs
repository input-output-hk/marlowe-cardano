-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Tests of Marlowe's Plutus implementation against its on-chain specification.
--
-----------------------------------------------------------------------------


module Spec.Marlowe.Plutus.Specification (
-- * Testing
  tests
) where


import Spec.Marlowe.Plutus.Types ()
import Test.Tasty (TestTree, testGroup)


-- | Run tests.
tests :: TestTree
tests =
  testGroup "Marlowe On-Chain Specification"
    [
    ]
