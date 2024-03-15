-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Golden tests of contract execution.
module Spec.Marlowe.Semantics.Golden.Tests (
  tests,
) where

import Data.List (intercalate)
import Language.Marlowe.Core.V1.Semantics (TransactionOutput (..), playTrace)
import Language.Marlowe.Core.V1.Semantics.Types (Contract)
import Language.Marlowe.FindInputs (getAllInputs)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

import Language.Marlowe.Analysis.FSSemantics (SlotLength (..))
import Spec.Marlowe.Semantics.Golden (GoldenCase)
import qualified Spec.Marlowe.Semantics.Golden.Escrow as Escrow (contract, invalids, valids)
import qualified Spec.Marlowe.Semantics.Golden.Negative as Negative (contract, invalids, valids)
import qualified Spec.Marlowe.Semantics.Golden.Pangram as Pangram (contract, invalids, valids)
import qualified Spec.Marlowe.Semantics.Golden.Swap as Swap (contract, invalids, valids)
import qualified Spec.Marlowe.Semantics.Golden.Trivial as Trivial (contract, invalids, valids)
import qualified Spec.Marlowe.Semantics.Golden.ZeroCouponBond as ZCB (contract, invalids, valids)

-- | Set to `True` to print the paths through the golden contracts.
_GENERATE_TEST_CASES_ :: Bool
_GENERATE_TEST_CASES_ = False

-- | Run tests.
tests :: TestTree
tests =
  testGroup
    "Golden"
    [ testGolden "Escrow" _GENERATE_TEST_CASES_ Escrow.contract Escrow.valids Escrow.invalids
    , testGolden "Pangram" _GENERATE_TEST_CASES_ Pangram.contract Pangram.valids Pangram.invalids
    , testGolden "Swap" _GENERATE_TEST_CASES_ Swap.contract Swap.valids Swap.invalids
    , testGolden "Trivial" _GENERATE_TEST_CASES_ Trivial.contract Trivial.valids Trivial.invalids
    , testGolden "Zero Coupon Bond" _GENERATE_TEST_CASES_ ZCB.contract ZCB.valids ZCB.invalids
    , testGolden "Negative" _GENERATE_TEST_CASES_ Negative.contract Negative.valids Negative.invalids
    ]

-- | Run a golden test.
testGolden
  :: String
  -- ^ Name of the test.
  -> Bool
  -- ^ Whether to print the valid test cases instead of doing testing.
  -> Contract
  -- ^ The contract.
  -> [GoldenCase]
  -- ^ Cases where the contract should succeed.
  -> [GoldenCase]
  -- ^ Cases where the contract should fail.
  -> TestTree
  -- ^ The tests.
testGolden name printIt contract valids invalids =
  testGroup name $
    if printIt
      then [testCase "Generate valid inputs" $ printValids contract]
      else
        [ testCase "Valid inputs" $ testValid contract valids
        , testCase "Invalid inputs" $ testInvalid contract invalids
        ]

-- | Use static analysis to generate and print successful golden test cases for a contract. This may take many minutes to execute.
printValids :: Contract -> IO ()
printValids contract =
  do
    let slotLength = SlotLength 1000
    Right is <- getAllInputs slotLength contract Nothing
    sequence_
      [ case playTrace t contract i of
        Error{} -> pure ()
        o -> putStrLn $ intercalate "\t" [show t, show i, show o]
      | (t, i) <- is
      ]

-- | Test successful execution for the Pangram contract.
testValid :: Contract -> [GoldenCase] -> IO ()
testValid = testValidity True

-- | Test erroneous transactions for the Pangram contract.
testInvalid :: Contract -> [GoldenCase] -> IO ()
testInvalid = testValidity False

-- | Test erroneous transactions for a contract.
testValidity
  :: Bool
  -- ^ Whether the test cases should pass.
  -> Contract
  -- ^ The contract.
  -> [GoldenCase]
  -- ^ The test cases.
  -> IO ()
  -- ^ Run the tests.
testValidity shouldSucceed contract invalids =
  sequence_
    [ assertBool (show (t, i)) $ (playTrace t contract i == o) == shouldSucceed
    | (t, i, o) <- invalids
    ]
