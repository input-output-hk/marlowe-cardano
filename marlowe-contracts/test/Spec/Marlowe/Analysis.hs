{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Spec.Marlowe.Analysis
where

import Data.Either (isRight)
import Data.Maybe (isJust, isNothing)
import Language.Marlowe
import Language.Marlowe.Analysis.FSSemantics (warningsTrace)
import Marlowe.Contracts.Options
import Marlowe.Contracts.Swap
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Marlowe"
    [ testCase "Swap test" swapTest
    , testCase "Sequentially Swap twice test" sequentiallySwapTwiceTest
    , testCase "Concurrently Swap twice test" concurrentlySwapTwiceTest
    , testCase "Covered Call test" coveredCallTest
    , testCase "Option test" optionTest
    ]

testWarnings :: Bool -> Contract -> IO ()
testWarnings b contract = do
  result <- warningsTrace contract
  assertBool "Analysis ok" $ isRight result && either (const False) f result
  where
    f | b = isJust
      | otherwise = isNothing

testNoWarnings :: Contract -> IO ()
testNoWarnings = testWarnings False

testExpectedWarnings :: Contract -> IO ()
testExpectedWarnings = testWarnings True

partyA, partyB :: Party
partyA = Role "a"
partyB = Role "b"

coin :: Token
coin = Token "" "coin"

-- |== Test Cases, no warnings expected

swapTest :: IO ()
swapTest =
  testNoWarnings $
    swap partyA ada (Constant 1) partyB coin (Constant 10) (Slot 10) Close

sequentiallySwapTwiceTest :: IO ()
sequentiallySwapTwiceTest =
  testNoWarnings $
    swap partyA ada (Constant 1) partyB coin (Constant 10) (Slot 10) $
    swap partyA coin (Constant 10) partyB ada (Constant 1) (Slot 10) Close

concurrentlySwapTwiceTest :: IO ()
concurrentlySwapTwiceTest =
  testNoWarnings $
    let s1 = swap partyA ada (Constant 1) partyB coin (Constant 10) (Slot 10) Close
        s2 = swap partyA coin (Constant 10) partyB ada (Constant 1) (Slot 10) Close
     in s1 `both` s2

coveredCallTest :: IO ()
coveredCallTest =
  testNoWarnings $
    coveredCall (Role "buyer") (Role "seller") ada coin (Constant 1) (Constant 1) (Slot 10) (Slot 100) (Slot 110)

-- |== Test Cases, warnings expected

optionTest :: IO ()
optionTest =
  testExpectedWarnings $
    option European Call partyA partyB (ada, Constant 1) (coin, Constant 1) (Slot 100) (Slot 110)
