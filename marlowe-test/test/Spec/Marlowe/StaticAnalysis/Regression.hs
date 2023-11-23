-----------------------------------------------------------------------------
--
-- Module      :  Spec.Marlowe.StaticAnalysis.Regression
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

-- | Regression tests for the important behaviour of the Static Analysis functions.
module Spec.Marlowe.StaticAnalysis.Regression (
  -- * Testing
  tests,
) where

import Control.Monad (join)
import Language.Marlowe.Util (ada)
import Test.Tasty (TestTree, testGroup)
import Prelude

import Control.Error (listToMaybe)
import Control.Error.Util (hush)
import Data.Either (fromRight, isRight)
import Data.Functor ((<&>))
import Data.Maybe (isNothing)
import Language.Marlowe (
  Action (..),
  Case (..),
  Contract (..),
  Observation (..),
  Payee (..),
  Token (..),
  TransactionWarning (..),
  Value (..),
 )
import Language.Marlowe.Analysis.FSSemantics (warningsTrace)
import Spec.Marlowe.Common (alicePk)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

isTransactionPartialPay :: TransactionWarning -> Bool
isTransactionPartialPay (TransactionPartialPay{}) = True
isTransactionPartialPay _ = False

isTransactionNonPositivePay :: TransactionWarning -> Bool
isTransactionNonPositivePay (TransactionNonPositivePay{}) = True
isTransactionNonPositivePay _ = False

isTransactionNonPositiveDeposit :: TransactionWarning -> Bool
isTransactionNonPositiveDeposit (TransactionNonPositiveDeposit{}) = True
isTransactionNonPositiveDeposit _ = False

isTransactionShadowing :: TransactionWarning -> Bool
isTransactionShadowing (TransactionShadowing{}) = True
isTransactionShadowing _ = False

isTransactionAssertionFailed :: TransactionWarning -> Bool
isTransactionAssertionFailed (TransactionAssertionFailed{}) = True
isTransactionAssertionFailed _ = False

getWarning :: Contract -> IO (Maybe TransactionWarning)
getWarning contract =
  warningsTrace contract <&> \res -> do
    (_, _, t) <- join $ hush res
    listToMaybe t

analysisWorks :: IO ()
analysisWorks = do
  let contract n d =
        If
          (DivValue (Constant n) (Constant d) `ValueGE` Constant 5)
          Close
          (Pay alicePk (Party alicePk) ada (Constant (-100)) Close)
  result <- warningsTrace (contract 10 11)
  assertBool "Analyse a contract" $ isRight result

emptyTrace :: IO ()
emptyTrace = do
  let contract = Close
  result <- warningsTrace contract
  assertBool "Empty trace" $ isRight result && isNothing (fromRight Nothing result)

nonPositivePay :: IO ()
nonPositivePay = do
  let contract n = Pay alicePk (Party alicePk) ada (Constant n) Close
  result <- getWarning $ contract (-100)
  assertBool "Detect negative pay" $ maybe False isTransactionNonPositivePay result
  result2 <- getWarning $ contract 0
  assertBool "Detect zero pay" $ maybe False isTransactionNonPositivePay result2

partialPay :: IO ()
partialPay = do
  let contract = Pay alicePk (Party alicePk) ada (Constant 100) Close
  result <- getWarning contract
  assertBool "Detect partial pay" $ maybe False isTransactionPartialPay result

nonPositiveDeposit :: IO ()
nonPositiveDeposit = do
  let contract v =
        When
          [ Case
              ( Deposit
                  alicePk
                  alicePk
                  (Token "" "")
                  (Constant v)
              )
              Close
          ]
          1699974289397
          Close
  result <- getWarning (contract (-100))
  assertBool "Negative deposit" $ maybe False isTransactionNonPositiveDeposit result
  result2 <- getWarning (contract 0)
  assertBool "Zero deposit" $ maybe False isTransactionNonPositiveDeposit result2

transactionShadowing :: IO ()
transactionShadowing = do
  let contract = Let "x" (Constant 1) (Let "x" (Constant 2) Close)
  result <- getWarning contract
  assertBool "Shadowing x" $ maybe False isTransactionShadowing result

assertionFailed :: IO ()
assertionFailed = do
  let contract = Let "x" (Constant 1) (Assert (ValueEQ (UseValue "x") (Constant 2)) Close)
  result <- getWarning contract
  assertBool "Detect wrong assertion" $ maybe False isTransactionAssertionFailed result

-- The current UI drops in instrumentation in the form of assertions and then checks for AssertionFailed
reachability :: IO ()
reachability = do
  let contract =
        Let
          "x"
          (Constant 1)
          ( If
              ( ValueGE
                  (UseValue "x")
                  (Constant 2)
              )
              ( Assert
                  ( ValueGE
                      (UseValue "x")
                      (Constant 2)
                  )
                  Close
              )
              ( Assert
                  ( ValueLT
                      (UseValue "x")
                      (Constant 2)
                  )
                  (Assert FalseObs Close)
              )
          )
  result <- getWarning contract
  assertBool "Detect reachable path" $ maybe False isTransactionAssertionFailed result
  let contract2 =
        Let
          "x"
          (Constant 1)
          ( If
              ( ValueGE
                  (UseValue "x")
                  (Constant 2)
              )
              ( Assert
                  ( ValueGE
                      (UseValue "x")
                      (Constant 2)
                  )
                  (Assert FalseObs Close)
              )
              ( Assert
                  ( ValueLT
                      (UseValue "x")
                      (Constant 2)
                  )
                  Close
              )
          )
  result2 <- getWarning contract2
  assertEqual "Don't trigger a warning on the unreachable path" result2 Nothing

-- The current UI drops in instrumentation in the form of assertions and then checks for AssertionFailed
fundsOnClose :: IO ()
fundsOnClose = do
  let contract =
        When
          [Case (Deposit alicePk alicePk ada (Constant 100)) (Assert (ValueEQ (AvailableMoney alicePk ada) (Constant 0)) Close)]
          1699974289397
          Close
  result <- getWarning contract
  assertBool "Detect funds on close" $ maybe False isTransactionAssertionFailed result

tests :: TestTree
tests =
  testGroup
    "Static Analysis Regression"
    [ testCase "Analysis works" analysisWorks
    , testCase "Empty trace" emptyTrace
    , testCase "Partial Pay" partialPay
    , testCase "Non-Positive Pay" nonPositivePay
    , testCase "Non-Positive Deposit" nonPositiveDeposit
    , testCase "Transaction shadowing" transactionShadowing
    , testCase "Assertion failed" assertionFailed
    , testCase "Reachability checks" reachability
    , testCase "Funds on Close" fundsOnClose
    ]
