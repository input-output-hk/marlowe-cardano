-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Tests of Marlowe serialisation.
--
-----------------------------------------------------------------------------


module Spec.Marlowe.Serialization
  ( -- * Testing
    tests
  ) where


import Test.Tasty (TestTree, testGroup)

import qualified Spec.Marlowe.Serialization.CoreJson (tests)
import qualified Spec.Marlowe.Serialization.ExtendedJson (tests)


-- | Run the tests.
tests :: TestTree
tests =
  testGroup "Serialization"
    [
      Spec.Marlowe.Serialization.CoreJson.tests
    , Spec.Marlowe.Serialization.ExtendedJson.tests
    ]
