-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Test Marlowe's Cardano implementation against the Isabelle-derived test server.
--
-----------------------------------------------------------------------------


module Spec.Marlowe.Service.Isabelle
  ( -- * Testing
    tests
  ) where


import Spec.Marlowe.Service (handleValues)
import Test.Tasty (TestTree, testGroup)

import qualified Data.Aeson as A
import qualified Marlowe.Spec.Core (tests)


-- | Run the tests.
tests :: TestTree
tests =
  testGroup "Marlowe Client"
    [
      Marlowe.Spec.Core.tests
        $ \request ->
          do
            response <- handleValues $ A.toJSON request
            case A.fromJSON response of
              A.Success response' -> pure response'
              A.Error message -> error message
    ]
