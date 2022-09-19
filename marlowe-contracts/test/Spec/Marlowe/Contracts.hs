{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Spec.Marlowe.Contracts
  ( tests
  ) where

import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Language.Marlowe as C
import Language.Marlowe.Extended.V1
import Marlowe.Contracts.Common
import Marlowe.Contracts.UTC.Common
import Marlowe.Contracts.UTC.CouponBond
import Marlowe.Contracts.UTC.Futures
import Marlowe.Contracts.UTC.Options
import Marlowe.Contracts.UTC.StructuredProducts
import Marlowe.Contracts.UTC.Swap
import Marlowe.Contracts.UTC.ZeroCouponBond
import Plutus.V1.Ledger.Ada (lovelaceValueOf)
import Plutus.V1.Ledger.Api (TokenName, singleton)
import Plutus.V1.Ledger.Value (CurrencySymbol)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Marlowe Contract"
  [ testCase "ZeroCouponBond" zeroCouponBondTest
  , testCase "CouponBond" couponBondTest
  , testCase "Swap Contract" swapContractTest
  , testCase "Future Contract (repayment of initial margin)" futureNoChange
  , testCase "Future Contract (without margin calls)" futureNoMarginCall
  , testCase "Future Contract (with margins calls)" futureWithMarginCall
  , testCase "American Call Option test" americanCallOptionTest
  , testCase "American Call Option (exercised) test" americanCallOptionExercisedTest
  , testCase "European Call Option test" europeanCallOptionTest
  , testCase "European Call Option (exercised) test" europeanCallOptionExercisedTest
  , testCase "Reverse Convertible test" reverseConvertibleTest
  , testCase "Reverse Convertible (exercised) test" reverseConvertibleExercisedTest
  ]

w1Pk, w2Pk :: Party
w1Pk = Role "party1"
w2Pk = Role "party2"

tokName :: TokenName
tokName = "testcoin"

tokSymbol :: CurrencySymbol
tokSymbol = ""

tok :: Token
tok = Token tokSymbol tokName

tokValueOf :: Integer -> C.Money
tokValueOf = singleton tokSymbol tokName

assertTotalPayments :: Party -> [C.Payment] -> C.Money -> Assertion
assertTotalPayments p t = assertEqual "total payments to party" $ totalPayments t
  where
    totalPayments = foldMap C.paymentMoney . filter (\(C.Payment _ a _ _) -> a == C.Party p)

assertNoWarnings :: [a] -> Assertion
assertNoWarnings [] = pure ()
assertNoWarnings t  = assertBool "Assert no warnings" $ null t

assertClose :: C.Contract -> Assertion
assertClose = assertBool "Contract is in Close" . (C.Close==)

assertNoFailedTransactions :: C.TransactionError -> Assertion
assertNoFailedTransactions err = assertFailure $ "Transactions are not expected to fail: " ++ show err

toPOSIX :: String -> C.POSIXTime
toPOSIX = C.POSIXTime . floor . utcTimeToPOSIXSeconds . read

-- |Zero-coupon Bond test
zeroCouponBondTest :: IO ()
zeroCouponBondTest =
  let Just contract = toCore $
        zeroCouponBond
          w1Pk
          w2Pk
          (read "2022-01-01 00:00:00.000000 UTC")
          (read "2023-01-01 00:00:00.000000 UTC")
          (Constant 75_000_000)
          (Constant 90_000_000)
          ada
          Close
      txIn =
        [ C.TransactionInput (toPOSIX "2021-12-31 00:00:00.000000 UTC", toPOSIX "2021-12-31 23:59:59.999999 UTC") [C.NormalInput $ C.IDeposit w1Pk w1Pk ada 75_000_000]
        , C.TransactionInput (toPOSIX "2022-12-31 00:00:00.000000 UTC", toPOSIX "2022-12-31 23:59:59.999999 UTC") [C.NormalInput $ C.IDeposit w2Pk w2Pk ada 90_000_000]
        ]
   in case C.playTrace 0 contract txIn of
        C.TransactionOutput {..} -> do
          assertClose txOutContract
          assertNoWarnings txOutWarnings
          assertTotalPayments w1Pk txOutPayments (lovelaceValueOf 90_000_000)
        C.Error err ->
          assertNoFailedTransactions err

-- |Coupon Bond test
couponBondTest :: IO ()
couponBondTest =
  let Just contract = toCore $
        couponBond
          w1Pk
          w2Pk
          (read "2022-01-01 00:00:00.000000 UTC")
          (read "2023-01-01 00:00:00.000000 UTC")
          (Cycle 1 HalfYear False)
          (Constant 75_000_000)
          (Constant 1_000_000)
          (Constant 90_000_000)
          ada
          Close
      txIn =
        [ C.TransactionInput (toPOSIX "2021-12-31 00:00:00.000000 UTC", toPOSIX "2021-12-31 23:59:59.999999 UTC") [C.NormalInput $ C.IDeposit w1Pk w1Pk ada 75_000_000]
        , C.TransactionInput (toPOSIX "2022-06-30 00:00:00.000000 UTC", toPOSIX "2022-06-30 23:59:59.999999 UTC") [C.NormalInput $ C.IDeposit w2Pk w2Pk ada 1_000_000]
        , C.TransactionInput (toPOSIX "2022-12-31 00:00:00.000000 UTC", toPOSIX "2022-12-31 23:59:59.999999 UTC") [C.NormalInput $ C.IDeposit w2Pk w2Pk ada 1_000_000]
        , C.TransactionInput (toPOSIX "2022-12-31 00:00:00.000000 UTC", toPOSIX "2022-12-31 23:59:59.999999 UTC") [C.NormalInput $ C.IDeposit w2Pk w2Pk ada 90_000_000]
        ]
   in case C.playTrace 0 contract txIn of
        C.TransactionOutput {..} -> do
          assertClose txOutContract
          assertNoWarnings txOutWarnings
          assertTotalPayments w1Pk txOutPayments (lovelaceValueOf 92_000_000)
        C.Error err ->
          assertNoFailedTransactions err


-- |Swap contract test
swapContractTest :: IO ()
swapContractTest =
  let Just contract = toCore $
        swap w1Pk ada (Constant 10_000_000) timestamp w2Pk tok (Constant 30) timestamp $
        swap w2Pk ada (Constant 10_000_000) timestamp w1Pk tok (Constant 30) timestamp Close
      timestamp = read "2022-01-01 00:00:00.000000 UTC"
      txIn =
        [ C.TransactionInput (0, 0)
            [ C.NormalInput $ C.IDeposit w1Pk w1Pk ada 10_000_000
            , C.NormalInput $ C.IDeposit w2Pk w2Pk tok 30
            , C.NormalInput $ C.IDeposit w2Pk w2Pk ada 10_000_000
            , C.NormalInput $ C.IDeposit w1Pk w1Pk tok 30
            ]
        ]
   in case C.playTrace 0 contract txIn of
        C.TransactionOutput {..} -> do
          assertClose txOutContract
          assertNoWarnings txOutWarnings
          assertTotalPayments w1Pk txOutPayments (lovelaceValueOf 10_000_000 <> tokValueOf 30)
          assertTotalPayments w2Pk txOutPayments (lovelaceValueOf 10_000_000 <> tokValueOf 30)
        C.Error err ->
          assertNoFailedTransactions err

-- |American Call Option test (No exercise)
americanCallOptionTest :: IO ()
americanCallOptionTest =
  let Just contract = toCore $
        option
          American
          Call
          w1Pk
          w2Pk
          Nothing
          (tok, Constant 30)
          (ada, Constant 10_000_000)
          (read "2022-03-01 09:00:00.000000 UTC")
          (read "2022-03-31 17:30:00.000000 UTC")
      txIn =
        [ C.TransactionInput (0, 0) [C.NormalInput $ C.IChoice (ChoiceId "Exercise Call" w1Pk) 0] ]
   in case C.playTrace 0 contract txIn of
        C.TransactionOutput {..} -> do
          assertClose txOutContract
          assertNoWarnings txOutWarnings
          assertTotalPayments w1Pk txOutPayments mempty
          assertTotalPayments w2Pk txOutPayments mempty
        C.Error err ->
          assertNoFailedTransactions err

-- |American Call Option test (Exercise)
americanCallOptionExercisedTest :: IO ()
americanCallOptionExercisedTest =
  let americanCall =
        option
          American
          Call
          w1Pk
          w2Pk
          Nothing
          (tok, Constant 30)
          (ada, Constant 10_000_000)
          (read "2022-03-31 17:00:00.000000 UTC")
          (read "2022-03-31 17:30:00.000000 UTC")
      Just contract = toCore $
        deposit
          w2Pk
          w2Pk
          (tok, Constant 30)
          (toTimeout $ read "2022-03-01 08:00:00.000000 UTC")
          Close
          americanCall

      t0 = toPOSIX "2022-03-01 07:30:00.000000 UTC"
      t1 = toPOSIX "2022-03-15 07:30:00.000000 UTC"
      t2 = toPOSIX "2022-03-15 08:00:00.000000 UTC"

      txIn =
        [ C.TransactionInput (t0, t0) [C.NormalInput $ C.IDeposit w2Pk w2Pk tok 30]
        , C.TransactionInput (t1, t1) [C.NormalInput $ C.IChoice (ChoiceId "Exercise Call" w1Pk) 1]
        , C.TransactionInput (t2, t2) [C.NormalInput $ C.IDeposit w1Pk w1Pk ada 10_000_000]
        ]
   in case C.playTrace 0 contract txIn of
        C.TransactionOutput {..} -> do
          assertClose txOutContract
          assertNoWarnings txOutWarnings
          assertTotalPayments w1Pk txOutPayments (tokValueOf 30)
          assertTotalPayments w2Pk txOutPayments (lovelaceValueOf 10_000_000)
        C.Error err ->
          assertNoFailedTransactions err

-- |European Call Option test (No exercise)
europeanCallOptionTest :: IO ()
europeanCallOptionTest =
  let Just contract = toCore $
        option
          European
          Call
          w1Pk
          w2Pk
          Nothing
          (tok, Constant 30)
          (ada, Constant 10_000_000)
          (read "2022-03-19 17:30:00.000000 UTC")
          (read "2022-03-20 17:30:00.000000 UTC")
      exerciseTime = toPOSIX "2022-03-19 17:31:00.000000 UTC"
      txIn =
        [ C.TransactionInput (exerciseTime, exerciseTime) [C.NormalInput $ C.IChoice (ChoiceId "Exercise Call" w1Pk) 0] ]
   in case C.playTrace 0 contract txIn of
        C.TransactionOutput {..} -> do
          assertClose txOutContract
          assertNoWarnings txOutWarnings
          assertTotalPayments w1Pk txOutPayments mempty
          assertTotalPayments w2Pk txOutPayments mempty
        C.Error err ->
          assertNoFailedTransactions err

-- |European Call Option test (Exercise)
europeanCallOptionExercisedTest :: IO ()
europeanCallOptionExercisedTest =
  let europeanCall =
        option
          European
          Call
          w1Pk
          w2Pk
          Nothing
          (tok, Constant 30)
          (ada, Constant 10_000_000)
          (read "2022-03-19 17:30:00.000000 UTC")
          (read "2022-03-20 17:30:00.000000 UTC")
      Just contract = toCore $
        deposit
          w2Pk
          w2Pk
          (tok, Constant 30)
          (toTimeout $ read "2022-03-19 08:00:00.000000 UTC")
          Close
          Close
        `both`
        europeanCall
      exerciseTime = toPOSIX "2022-03-19 17:31:00.000000 UTC"
      depositTime = toPOSIX "2022-03-19 17:32:00.000000 UTC"
      txIn =
        [ C.TransactionInput (0, 0)                       [C.NormalInput $ C.IDeposit w2Pk w2Pk tok 30]
        , C.TransactionInput (exerciseTime, exerciseTime) [C.NormalInput $ C.IChoice (ChoiceId "Exercise Call" w1Pk) 1]
        , C.TransactionInput (depositTime, depositTime)   [C.NormalInput $ C.IDeposit w1Pk w1Pk ada 10_000_000]
        ]
   in case C.playTrace 0 contract txIn of
        C.TransactionOutput {..} -> do
          assertClose txOutContract
          assertNoWarnings txOutWarnings
          assertTotalPayments w1Pk txOutPayments (tokValueOf 30)
          assertTotalPayments w2Pk txOutPayments (lovelaceValueOf 10_000_000)
        C.Error err ->
          assertNoFailedTransactions err

-- |Future, scenario repayment of initial margins
futureNoChange :: IO ()
futureNoChange =
  -- At maturity:
  --        w1 ------  80 ADA -----> w2
  --        w1 <----- 100 USD ------ w2
  -- The contract is cash settled, i.e. USD is delivered in ADA
  -- resp. the difference between 80 ADA and 100 USD in ADA is
  -- due at maturity
  let Just contract = toCore $
        future
          w1Pk
          w2Pk
          (Constant 80_000_000) -- 80 ADA
          (Constant 8_000_000) -- 8 ADA
          (read "2022-03-31 08:00:00.000000 UTC")
          [] -- no margin calls
          (read "2023-03-31 08:00:00.000000 UTC")

      t0 = toPOSIX "2022-03-31 07:30:00.000000 UTC"
      t1 = toPOSIX "2023-03-31 07:30:00.000000 UTC"

      txIn =
        [ C.TransactionInput (t0, t0)
            [ C.NormalInput $ C.IDeposit w1Pk w1Pk ada 8_000_000
            , C.NormalInput $ C.IDeposit w2Pk w2Pk ada 8_000_000 ]
        , C.TransactionInput (t1, t1)
            [ C.NormalInput $ C.IChoice dirRate 125_000_000
            , C.NormalInput $ C.IChoice invRate 80_000_000 ]
        ]
   in case C.playTrace 0 contract txIn of
        C.TransactionOutput {..} -> do
          assertClose txOutContract
          assertNoWarnings $ filter nonZeroPay txOutWarnings
          assertTotalPayments w1Pk txOutPayments (lovelaceValueOf 8_000_000)
          assertTotalPayments w2Pk txOutPayments (lovelaceValueOf 8_000_000)
        C.Error err ->
          assertNoFailedTransactions err
  where
    nonZeroPay (C.TransactionNonPositivePay _ _ _ i) = i /= 0
    nonZeroPay _                                     = True

-- |Future, scenario without any margin calls
futureNoMarginCall :: IO ()
futureNoMarginCall =
  let Just contract = toCore $
        future
          w1Pk
          w2Pk
          (Constant 80_000_000) -- 80 ADA
          (Constant 8_000_000) -- 8 ADA
          (read "2022-03-31 08:00:00.000000 UTC")
          [] -- no margin calls
          (read "2023-03-31 08:00:00.000000 UTC")

      t0 = toPOSIX "2022-03-31 07:30:00.000000 UTC"
      t1 = toPOSIX "2023-03-31 07:30:00.000000 UTC"

      txIn =
        [ C.TransactionInput (t0, t0)
            [ C.NormalInput $ C.IDeposit w1Pk w1Pk ada 8_000_000
            , C.NormalInput $ C.IDeposit w2Pk w2Pk ada 8_000_000 ]
        , C.TransactionInput (t1, t1)
            [ C.NormalInput $ C.IChoice dirRate 133_333_333
            , C.NormalInput $ C.IChoice invRate 75_000_000 ]
        ]
   in case C.playTrace 0 contract txIn of
        C.TransactionOutput {..} -> do
          assertClose txOutContract
          assertNoWarnings txOutWarnings
          assertTotalPayments w1Pk txOutPayments (lovelaceValueOf 1_333_334)
          assertTotalPayments w2Pk txOutPayments (lovelaceValueOf 14_666_666)
        C.Error err ->
          assertNoFailedTransactions err

-- |Future, scenario with margin call
futureWithMarginCall :: IO ()
futureWithMarginCall =
  let Just contract = toCore $
        future
          w1Pk
          w2Pk
          (Constant 80_000_000) -- 80 ADA
          (Constant 8_000_000) -- 8 ADA
          (read "2022-03-31 08:00:00.000000 UTC")
          [read "2022-09-30 08:30:00.000000 UTC"] -- margin call
          (read "2023-03-31 09:00:00.000000 UTC")

      t0 = toPOSIX "2022-03-31 07:30:00.000000 UTC"
      t1 = toPOSIX "2022-09-30 07:00:00.000000 UTC"
      t2 = toPOSIX "2022-09-30 07:30:00.000000 UTC"
      t3 = toPOSIX "2023-03-31 07:30:00.000000 UTC"

      txIn =
        [ C.TransactionInput (t0, t0)
            [ C.NormalInput $ C.IDeposit w1Pk w1Pk ada 8_000_000
            , C.NormalInput $ C.IDeposit w2Pk w2Pk ada 8_000_000 ]
        , C.TransactionInput (t1, t1)
            [ C.NormalInput $ C.IChoice dirRate 200_000_000
            , C.NormalInput $ C.IChoice invRate 50_000_000 ]
        , C.TransactionInput (t2, t2)
            [ C.NormalInput $ C.IDeposit w1Pk w1Pk ada 60_000_000 ]
        , C.TransactionInput (t3, t3)
            [ C.NormalInput $ C.IChoice dirRate 133_333_333
            , C.NormalInput $ C.IChoice invRate 75_000_000 ]
        ]
   in case C.playTrace 0 contract txIn of
        C.TransactionOutput {..} -> do
          assertClose txOutContract
          assertNoWarnings txOutWarnings
          assertTotalPayments w1Pk txOutPayments (lovelaceValueOf 61_333_334)
          assertTotalPayments w2Pk txOutPayments (lovelaceValueOf 14_666_666)
        C.Error err ->
          assertNoFailedTransactions err

-- |Reverse Convertible test (Exercise)
reverseConvertibleExercisedTest :: IO ()
reverseConvertibleExercisedTest =
  let Just contract = toCore $
        reverseConvertible
          w1Pk
          (read "2022-03-19 17:30:00.000000 UTC")
          (read "2022-03-19 17:30:00.000000 UTC")
          (read "2022-03-20 17:30:00.000000 UTC")
          Nothing
          ada
          tok
          (Constant 10_000_000)
          (Constant 30)
          (Constant 9_000_000)
      repaymentTime = toPOSIX "2022-03-19 17:29:59.000000 UTC"
      exerciseTime = toPOSIX "2022-03-19 17:30:00.000000 UTC"
      txIn =
        [ C.TransactionInput (0, 0)   [ C.NormalInput $ C.IDeposit w1Pk w1Pk ada 9_000_000 ]
        , C.TransactionInput (repaymentTime, repaymentTime) [ C.NormalInput $ C.IDeposit w1Pk (Role "BondProvider") ada 10_000_000 ]
        , C.TransactionInput (exerciseTime, exerciseTime)
            [ C.NormalInput $ C.IChoice (ChoiceId "Exercise Put" (Role "OptionCounterparty")) 1
            , C.NormalInput $ C.IDeposit (Role "OptionCounterparty") (Role "OptionCounterparty") tok 30 ]
        ]
   in case C.playTrace 0 contract txIn of
        C.TransactionOutput {..} -> do
          assertClose txOutContract
          assertNoWarnings txOutWarnings
          assertTotalPayments w1Pk txOutPayments (tokValueOf 30)
        C.Error err ->
          assertNoFailedTransactions err

-- |Reverse Convertible test (No exercise)
reverseConvertibleTest :: IO ()
reverseConvertibleTest =
  let Just contract = toCore $
        reverseConvertible
          w1Pk
          (read "2022-03-19 17:30:00.000000 UTC")
          (read "2022-03-19 17:30:00.000000 UTC")
          (read "2022-03-20 17:30:00.000000 UTC")
          Nothing
          ada
          tok
          (Constant 10_000_000)
          (Constant 30)
          (Constant 9_000_000)
      repaymentTime = toPOSIX "2022-03-19 17:29:59.000000 UTC"
      exerciseTime = toPOSIX "2022-03-19 17:30:00.000000 UTC"
      txIn =
        [ C.TransactionInput (0, 0)                         [ C.NormalInput $ C.IDeposit w1Pk w1Pk ada 9_000_000 ]
        , C.TransactionInput (repaymentTime, repaymentTime) [ C.NormalInput $ C.IDeposit w1Pk (Role "BondProvider") ada 10_000_000 ]
        , C.TransactionInput (exerciseTime, exerciseTime)   [ C.NormalInput $ C.IChoice (ChoiceId "Exercise Put" (Role "OptionCounterparty")) 0 ]
        ]
   in case C.playTrace 0 contract txIn of
        C.TransactionOutput {..} -> do
          assertClose txOutContract
          assertNoWarnings txOutWarnings
          assertTotalPayments w1Pk txOutPayments (lovelaceValueOf 10_000_000)
        C.Error err ->
          assertNoFailedTransactions err
