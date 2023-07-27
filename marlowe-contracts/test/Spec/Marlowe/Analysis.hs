{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Spec.Marlowe.Analysis where

import Data.Maybe (isJust)
import Data.Time.Clock (UTCTime)
import Language.Marlowe.Analysis.FSSemantics (warningsTrace)
import Language.Marlowe.Core.V1.Semantics.Types
import Marlowe.Contracts.Common
import Marlowe.Contracts.UTC.Options
import Marlowe.Contracts.UTC.Swap
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Marlowe Contract"
    [ testCase "Swap test" swapTest
    , testCase "Sequentially Swap twice test" sequentiallySwapTwiceTest
    , testCase "Concurrently Swap twice test" concurrentlySwapTwiceTest
    , testCase "Covered Call test" coveredCallTest
    , testCase "Option test" optionTest
    ]

hasWarnings :: Contract -> IO Bool
hasWarnings c = do
  result <- warningsTrace c
  case result of
    Left errDesc -> assertFailure ("Analysis failed: " ++ show errDesc)
    Right counterExample -> return $ isJust counterExample

testNoWarnings :: Contract -> Assertion
testNoWarnings c =
  assertBool "Has no warnings" . not =<< hasWarnings c

testExpectedWarnings :: Contract -> Assertion
testExpectedWarnings c =
  assertBool "Has warnings" =<< hasWarnings c

partyA, partyB :: Party
partyA = Role "a"
partyB = Role "b"

ada :: Token
ada = Token "" ""

coin :: Token
coin = Token "" "coin"

timestamp :: UTCTime
timestamp = read "2022-01-01 00:00:00.000000 UTC"

-- | == Test Cases, no warnings expected
swapTest :: IO ()
swapTest =
  testNoWarnings $
    swap partyA ada (Constant 1) timestamp partyB coin (Constant 10) timestamp Close

sequentiallySwapTwiceTest :: IO ()
sequentiallySwapTwiceTest =
  testNoWarnings $
    swap partyA ada (Constant 1) timestamp partyB coin (Constant 10) timestamp $
      swap partyA coin (Constant 10) timestamp partyB ada (Constant 1) timestamp Close

concurrentlySwapTwiceTest :: IO ()
concurrentlySwapTwiceTest =
  testNoWarnings $
    let s1 = swap partyA ada (Constant 1) timestamp partyB coin (Constant 10) timestamp Close
        s2 = swap partyA coin (Constant 10) timestamp partyB ada (Constant 1) timestamp Close
     in s1 `both` s2

coveredCallTest :: IO ()
coveredCallTest =
  testNoWarnings $
    coveredCall
      (Role "buyer")
      (Role "seller")
      Nothing
      ada
      coin
      (Constant 1)
      (Constant 1)
      (read "2022-01-01 00:00:00.000000 UTC")
      (read "2022-03-31 17:30:00.000000 UTC")
      (read "2022-03-31 18:00:00.000000 UTC")

-- | == Test Cases, warnings expected
optionTest :: IO ()
optionTest =
  testExpectedWarnings $
    option
      European
      Call
      partyA
      partyB
      Nothing
      (ada, Constant 1)
      (coin, Constant 1)
      (read "2022-03-31 17:30:00.000000 UTC")
      (read "2022-03-31 18:00:00.000000 UTC")
