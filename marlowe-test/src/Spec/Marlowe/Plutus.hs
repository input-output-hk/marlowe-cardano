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


module Spec.Marlowe.Plutus
  ( -- * Testing
    tests
  ) where


import Spec.Marlowe.Reference (ReferencePath)
import Test.Tasty (TestTree, testGroup)

import qualified Spec.Marlowe.Plutus.AssocMap (tests)
import qualified Spec.Marlowe.Plutus.MList (tests)
import qualified Spec.Marlowe.Plutus.Prelude (tests)
import qualified Spec.Marlowe.Plutus.ScriptContext (tests)
import qualified Spec.Marlowe.Plutus.Specification (tests)
import qualified Spec.Marlowe.Plutus.Value (tests)


-- | Run tests.
tests :: [ReferencePath] -> TestTree
tests referencePaths =
  testGroup "Plutus"
    [
      Spec.Marlowe.Plutus.Prelude.tests
    , Spec.Marlowe.Plutus.AssocMap.tests
    , Spec.Marlowe.Plutus.MList.tests
    , Spec.Marlowe.Plutus.Value.tests
    , Spec.Marlowe.Plutus.ScriptContext.tests
    , Spec.Marlowe.Plutus.Specification.tests referencePaths
    ]
