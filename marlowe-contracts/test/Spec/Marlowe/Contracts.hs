{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
module Spec.Marlowe.Contracts
    (tests)
where

import Language.Marlowe
import Ledger.Ada
import Ledger.Value
import Marlowe.Contracts.Common
import Marlowe.Contracts.Futures
import Marlowe.Contracts.Options
import Marlowe.Contracts.StructuredProducts
import Marlowe.Contracts.Swap
import Marlowe.Contracts.ZeroCouponBond
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Marlowe Contract"
  [ testCase "ZeroCouponBond" zeroCouponBondTest
  , testCase "Swap Contract" swapContractTest
  , testCase "Future Contract (repayment of initial margin)" futureNoChange
  , testCase "Future Contract (without margin calls)" futureNoMarginCall
  , testCase "Future Contract (with margins calls)" futureWithMarinCall
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

tokValueOf :: Integer -> Money
tokValueOf = singleton tokSymbol tokName

assertTotalPayments :: Party -> [Payment] -> Money -> Assertion
assertTotalPayments p t x = assertBool "total payments to party" (totalPayments t == x)
  where
    totalPayments = mconcat . map (\(Payment _ _ a) -> a) . filter (\(Payment _ a _) -> a == Party p)

assertNoWarnings :: [a] -> Assertion
assertNoWarnings [] = pure ()
assertNoWarnings t  = assertBool "No warnings" $ null t

assertClose :: Contract -> Assertion
assertClose = assertBool "Contract is in Close" . (Close==)

assertNoFailedTransactions :: TransactionError -> Assertion
assertNoFailedTransactions err = assertFailure $ "Transactions are not expected to fail: " ++ show err

-- |Zero-coupon Bond test
zeroCouponBondTest :: IO ()
zeroCouponBondTest =
  let contract =
        zeroCouponBond
          w1Pk
          w2Pk
          (Slot 100)
          (Slot 200)
          (Constant 75_000_000)
          (Constant 90_000_000)
          ada
          Close
      txIn =
        [ TransactionInput (0, 0)     [NormalInput $ IDeposit w1Pk w1Pk ada 75_000_000]
        , TransactionInput (100, 110) [NormalInput $ IDeposit w1Pk w2Pk ada 90_000_000]
        ]
   in case playTrace 0 contract txIn of
        TransactionOutput {..} -> do
          assertClose txOutContract
          assertNoWarnings txOutWarnings
          assertTotalPayments w1Pk txOutPayments (lovelaceValueOf 90_000_000)
        Error err ->
          assertNoFailedTransactions err

-- |Swap contract test
swapContractTest :: IO ()
swapContractTest =
  let contract =
        swap w1Pk ada (Constant 10_000_000) w2Pk tok (Constant 30) (Slot 100) $
        swap w2Pk ada (Constant 10_000_000) w1Pk tok (Constant 30) (Slot 200) Close
      txIn =
        [ TransactionInput (0, 0)
            [ NormalInput $ IDeposit w1Pk w1Pk ada 10_000_000
            , NormalInput $ IDeposit w2Pk w2Pk tok 30
            , NormalInput $ IDeposit w2Pk w2Pk ada 10_000_000
            , NormalInput $ IDeposit w1Pk w1Pk tok 30
            ]
        ]
   in case playTrace 0 contract txIn of
        TransactionOutput {..} -> do
          assertClose txOutContract
          assertNoWarnings txOutWarnings
          assertTotalPayments w1Pk txOutPayments (lovelaceValueOf 10_000_000 <> tokValueOf 30)
          assertTotalPayments w2Pk txOutPayments (lovelaceValueOf 10_000_000 <> tokValueOf 30)
        Error err ->
          assertNoFailedTransactions err

-- |American Call Option test (No exercise)
americanCallOptionTest :: IO ()
americanCallOptionTest =
  let contract =
        option
          American
          Call
          w1Pk
          w2Pk
          (tok, Constant 30)
          (ada, Constant 10_000_000)
          (Slot 100)
          (Slot 200)
      txIn =
        [ TransactionInput (0, 0) [NormalInput $ IChoice (ChoiceId "Exercise Call" w1Pk) 0] ]
   in case playTrace 0 contract txIn of
        TransactionOutput {..} -> do
          assertClose txOutContract
          assertNoWarnings txOutWarnings
          assertTotalPayments w1Pk txOutPayments mempty
          assertTotalPayments w2Pk txOutPayments mempty
        Error err ->
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
          (tok, Constant 30)
          (ada, Constant 10_000_000)
          (Slot 100)
          (Slot 200)
      contract =
        deposit
          w2Pk
          w2Pk
          (tok, Constant 30)
          (Slot 10)
          Close
          americanCall
      txIn =
        [ TransactionInput (0, 0)     [NormalInput $ IDeposit w2Pk w2Pk tok 30]
        , TransactionInput (99, 99)   [NormalInput $ IChoice (ChoiceId "Exercise Call" w1Pk) 1]
        , TransactionInput (101, 101) [NormalInput $ IDeposit w1Pk w1Pk ada 10_000_000]
        ]
   in case playTrace 0 contract txIn of
        TransactionOutput {..} -> do
          assertClose txOutContract
          assertNoWarnings txOutWarnings
          assertTotalPayments w1Pk txOutPayments (tokValueOf 30)
          assertTotalPayments w2Pk txOutPayments (lovelaceValueOf 10_000_000)
        Error err ->
          assertNoFailedTransactions err

-- |European Call Option test (No exercise)
europeanCallOptionTest :: IO ()
europeanCallOptionTest =
  let contract =
        option
          European
          Call
          w1Pk
          w2Pk
          (tok, Constant 30)
          (ada, Constant 10_000_000)
          (Slot 100)
          (Slot 200)
      txIn =
        [ TransactionInput (101, 101) [NormalInput $ IChoice (ChoiceId "Exercise Call" w1Pk) 0] ]
   in case playTrace 0 contract txIn of
        TransactionOutput {..} -> do
          assertClose txOutContract
          assertNoWarnings txOutWarnings
          assertTotalPayments w1Pk txOutPayments mempty
          assertTotalPayments w2Pk txOutPayments mempty
        Error err ->
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
          (tok, Constant 30)
          (ada, Constant 10_000_000)
          (Slot 100)
          (Slot 200)
      contract =
        deposit
          w2Pk
          w2Pk
          (tok, Constant 30)
          (Slot 10) Close Close
        `both`
        europeanCall
      txIn =
        [ TransactionInput (0, 0)     [NormalInput $ IDeposit w2Pk w2Pk tok 30]
        , TransactionInput (101, 101) [NormalInput $ IChoice (ChoiceId "Exercise Call" w1Pk) 1]
        , TransactionInput (102, 102) [NormalInput $ IDeposit w1Pk w1Pk ada 10_000_000]
        ]
   in case playTrace 0 contract txIn of
        TransactionOutput {..} -> do
          assertClose txOutContract
          assertNoWarnings txOutWarnings
          assertTotalPayments w1Pk txOutPayments (tokValueOf 30)
          assertTotalPayments w2Pk txOutPayments (lovelaceValueOf 10_000_000)
        Error err ->
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
  let contract =
        future
          w1Pk
          w2Pk
          (Constant 80_000_000) -- 80 ADA
          (Constant 8_000_000) -- 8 ADA
          (Slot 1)
          [] -- no margin calls
          (Slot 100) -- maturity
      txIn =
        [ TransactionInput (0, 0)
            [ NormalInput $ IDeposit w1Pk w1Pk ada 8_000_000
            , NormalInput $ IDeposit w2Pk w2Pk ada 8_000_000 ]
        , TransactionInput (99, 99)
            [ NormalInput $ IChoice dirRate 125_000_000
            , NormalInput $ IChoice invRate 80_000_000 ]
        ]
   in case playTrace 0 contract txIn of
        TransactionOutput {..} -> do
          assertClose txOutContract
          assertNoWarnings $ filter nonZeroPay txOutWarnings
          assertTotalPayments w1Pk txOutPayments (lovelaceValueOf 8_000_000)
          assertTotalPayments w2Pk txOutPayments (lovelaceValueOf 8_000_000)
        Error err ->
          assertNoFailedTransactions err
  where
    nonZeroPay (TransactionNonPositivePay _ _ _ i) = i /= 0
    nonZeroPay _                                   = True

-- |Future, scenario without any margin calls
futureNoMarginCall :: IO ()
futureNoMarginCall =
  let contract =
        future
          w1Pk
          w2Pk
          (Constant 80_000_000) -- 80 ADA
          (Constant 8_000_000) -- 8 ADA
          (Slot 1)
          [] -- no margin calls
          (Slot 100) -- maturity
      txIn =
        [ TransactionInput (0, 0)
            [ NormalInput $ IDeposit w1Pk w1Pk ada 8_000_000
            , NormalInput $ IDeposit w2Pk w2Pk ada 8_000_000 ]
        , TransactionInput (99, 99)
            [ NormalInput $ IChoice dirRate 133_333_333
            , NormalInput $ IChoice invRate 75_000_000 ]
        ]
   in case playTrace 0 contract txIn of
        TransactionOutput {..} -> do
          assertClose txOutContract
          assertNoWarnings txOutWarnings
          assertTotalPayments w1Pk txOutPayments (lovelaceValueOf 1_333_333)
          assertTotalPayments w2Pk txOutPayments (lovelaceValueOf 14_666_667)
        Error err ->
          assertNoFailedTransactions err

-- |Future, scenario with margin call
futureWithMarinCall :: IO ()
futureWithMarinCall =
  let contract =
        future
          w1Pk
          w2Pk
          (Constant 80_000_000) -- 80 ADA
          (Constant 8_000_000) -- 8 ADA
          (Slot 1)
          [Slot 50] -- margin call
          (Slot 100) -- maturity
      txIn =
        [ TransactionInput (0, 0)
            [ NormalInput $ IDeposit w1Pk w1Pk ada 8_000_000
            , NormalInput $ IDeposit w2Pk w2Pk ada 8_000_000 ]
        , TransactionInput (40, 40)
            [ NormalInput $ IChoice dirRate 200_000_000
            , NormalInput $ IChoice invRate 50_000_000 ]
        , TransactionInput (42, 42)
            [ NormalInput $ IDeposit w1Pk w1Pk ada 60_000_000 ]
        , TransactionInput (99, 99)
            [ NormalInput $ IChoice dirRate 133_333_333
            , NormalInput $ IChoice invRate 75_000_000 ]
        ]
   in case playTrace 0 contract txIn of
        TransactionOutput {..} -> do
          assertClose txOutContract
          assertNoWarnings txOutWarnings
          assertTotalPayments w1Pk txOutPayments (lovelaceValueOf 61_333_333)
          assertTotalPayments w2Pk txOutPayments (lovelaceValueOf 14_666_667)
        Error err ->
          assertNoFailedTransactions err

-- |Reverse Convertible test (Exercise)
reverseConvertibleExercisedTest :: IO ()
reverseConvertibleExercisedTest =
  let contract =
        reverseConvertible
          w1Pk
          (Slot 10)
          (Slot 100)
          (Slot 200)
          ada
          tok
          (Constant 10_000_000)
          (Constant 30)
          (Constant 9_000_000)
      txIn =
        [ TransactionInput (0, 0)   [ NormalInput $ IDeposit w1Pk w1Pk ada 9_000_000 ]
        , TransactionInput (99, 99) [ NormalInput $ IDeposit w1Pk (Role "BondProvider") ada 10_000_000 ]
        , TransactionInput (100, 100)
            [ NormalInput $ IChoice (ChoiceId "Exercise Call" (Role "OptionCounterparty")) 1
            , NormalInput $ IDeposit (Role "OptionCounterparty") (Role "OptionCounterparty") tok 30 ]
        ]
   in case playTrace 0 contract txIn of
        TransactionOutput {..} -> do
          assertClose txOutContract
          assertNoWarnings txOutWarnings
          assertTotalPayments w1Pk txOutPayments (tokValueOf 30)
        Error err ->
          assertNoFailedTransactions err

-- |Reverse Convertible test (No exercise)
reverseConvertibleTest :: IO ()
reverseConvertibleTest =
  let contract =
        reverseConvertible
          w1Pk
          (Slot 10)
          (Slot 100)
          (Slot 200)
          ada
          tok
          (Constant 10_000_000)
          (Constant 30)
          (Constant 9_000_000)
      txIn =
        [ TransactionInput (0, 0)     [ NormalInput $ IDeposit w1Pk w1Pk ada 9_000_000 ]
        , TransactionInput (99, 99)   [ NormalInput $ IDeposit w1Pk (Role "BondProvider") ada 10_000_000 ]
        , TransactionInput (100, 100) [ NormalInput $ IChoice (ChoiceId "Exercise Call" (Role "OptionCounterparty")) 0 ]
        ]
   in case playTrace 0 contract txIn of
        TransactionOutput {..} -> do
          assertClose txOutContract
          assertNoWarnings txOutWarnings
          assertTotalPayments w1Pk txOutPayments (lovelaceValueOf 10_000_000)
        Error err ->
          assertNoFailedTransactions err
