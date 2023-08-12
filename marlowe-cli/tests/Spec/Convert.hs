-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-----------------------------------------------------------------------------

-- | Quickcheck tests for contract format conversion
module Spec.Convert (
  -- * Testing
  tests,
) where

import Data.Text (pack)
import Language.Marlowe.CLI.Convert
import Language.Marlowe.Core.V1.Semantics.Types
import Language.Marlowe.Pretty
import Spec.Marlowe.Semantics.Arbitrary ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Property, testProperty, (===))
import Text.Megaparsec

-- | Run tests.
tests :: TestTree
tests =
  testGroup
    "Pretty printing"
    [ testGroup
        "Test pretty / un-pretty"
        [ testPrettyUnPretty
        ]
    ]

testPrettyUnPretty :: TestTree
testPrettyUnPretty =
  testProperty "Pretty print and parse contract" prop

prop :: Contract -> Property
prop c =
  pure c === runParser contractParser "string" (pack . show $ pretty c)
