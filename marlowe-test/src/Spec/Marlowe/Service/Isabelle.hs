

module Spec.Marlowe.Service.Isabelle
  ( tests
  ) where


import Spec.Marlowe.Service (handleValues)
import Test.Tasty (TestTree, testGroup)

import qualified Data.Aeson as A
import qualified Marlowe.Spec.Core (tests)


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
