-- | Tests of Marlowe's Plutus implementation.
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
module Spec.Marlowe.Plutus (
  -- * Testing
  tests,
) where

import Test.Tasty (TestTree, testGroup)

import qualified Spec.Marlowe.Plutus.AssocMap (tests)
import qualified Spec.Marlowe.Plutus.MList (tests)
import qualified Spec.Marlowe.Plutus.Prelude (tests)
import qualified Spec.Marlowe.Plutus.ScriptContext (tests)
import qualified Spec.Marlowe.Plutus.Value (tests)

-- | Run tests.
tests :: TestTree
tests =
  testGroup
    "Plutus"
    [ Spec.Marlowe.Plutus.Prelude.tests
    , Spec.Marlowe.Plutus.AssocMap.tests
    , Spec.Marlowe.Plutus.MList.tests
    , Spec.Marlowe.Plutus.Value.tests
    , Spec.Marlowe.Plutus.ScriptContext.tests
    ]
