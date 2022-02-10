{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
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

step :: TransactionInput -> ([TransactionWarning], [Payment], State, Contract) -> Either TransactionError ([TransactionWarning], [Payment], State, Contract)
step i (_,_,s,c) =
  case computeTransaction i s c of
    Error err                   -> Left err
    TransactionOutput w p s' c' -> Right (w, p, s', c')

-- |Zero-coupon Bond test
zeroCouponBondTest :: IO ()
zeroCouponBondTest =
  let zcb =
        zeroCouponBond
          w1Pk
          w2Pk
          (Slot 100)
          (Slot 200)
          (Constant 75_000_000)
          (Constant 90_000_000)
          ada
          Close
   in either left right $
        step (TransactionInput (0, 0)     [NormalInput $ IDeposit w1Pk w1Pk ada 75_000_000]) ([], [], emptyState 0, zcb)
    >>= step (TransactionInput (100, 110) [NormalInput $ IDeposit w1Pk w2Pk ada 90_000_000])
  where
    left err = assertFailure $ "Transactions are not expected to fail: " ++ show err
    right (w,p,_,c) = do
      assertClose c
      assertNoWarnings w
      assertTotalPayments w1Pk p (lovelaceValueOf 90_000_000)

-- |Swap contract test
swapContractTest :: IO ()
swapContractTest =
  let backSwap =
        swap w1Pk ada (Constant 10_000_000) w2Pk tok (Constant 30) (Slot 100) $
        swap w2Pk ada (Constant 10_000_000) w1Pk tok (Constant 30) (Slot 200) Close
   in either left right $
        step (TransactionInput (0, 0)
              [ NormalInput $ IDeposit w1Pk w1Pk ada 10_000_000
              , NormalInput $ IDeposit w2Pk w2Pk tok 30
              , NormalInput $ IDeposit w2Pk w2Pk ada 10_000_000
              , NormalInput $ IDeposit w1Pk w1Pk tok 30
              ]) ([], [], emptyState 0, backSwap)
  where
    left err = assertFailure $ "Transactions are not expected to fail: " ++ show err
    right (w,p,_,c) = do
      assertClose c
      assertNoWarnings w
      assertTotalPayments w1Pk p (lovelaceValueOf 10_000_000 <> tokValueOf 30)
      assertTotalPayments w2Pk p (lovelaceValueOf 10_000_000 <> tokValueOf 30)

-- |American Call Option test (No exercise)
americanCallOptionTest :: IO ()
americanCallOptionTest =
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
   in either left right $
        step (TransactionInput (0, 0) [NormalInput $ IChoice (ChoiceId "Exercise Call" w1Pk) 0]) ([], [], emptyState 0, americanCall)
  where
    left err = assertFailure $ "Transactions are not expected to fail: " ++ show err
    right (w, p, _, c) =
      do
        assertClose c
        assertNoWarnings w
        assertTotalPayments w1Pk p mempty
        assertTotalPayments w2Pk p mempty

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
   in either left right $
        step (TransactionInput (0, 0)     [NormalInput $ IDeposit w2Pk w2Pk tok 30]) ([], [], emptyState 0, contract)
    >>= step (TransactionInput (99, 99)   [NormalInput $ IChoice (ChoiceId "Exercise Call" w1Pk) 1])
    >>= step (TransactionInput (101, 101) [NormalInput $ IDeposit w1Pk w1Pk ada 10_000_000])
  where
    left err = assertFailure $ "Transactions are not expected to fail: " ++ show err
    right (w, p, _, c) =
      do
        assertClose c
        assertNoWarnings w
        assertTotalPayments w1Pk p (tokValueOf 30)
        assertTotalPayments w2Pk p (lovelaceValueOf 10_000_000)

-- |European Call Option test (No exercise)
europeanCallOptionTest :: IO ()
europeanCallOptionTest =
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
   in either left right $
        step (TransactionInput (101, 101) [NormalInput $ IChoice (ChoiceId "Exercise Call" w1Pk) 0]) ([], [], emptyState 0, europeanCall)
  where
    left err = assertFailure $ "Transactions are not expected to fail: " ++ show err
    right (w, p, _, c) =
      do
        assertClose c
        assertNoWarnings w
        assertTotalPayments w1Pk p mempty
        assertTotalPayments w2Pk p mempty

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
   in either left right $
        step (TransactionInput (0, 0)     [NormalInput $ IDeposit w2Pk w2Pk tok 30]) ([], [], emptyState 0, contract)
    >>= step (TransactionInput (101, 101) [NormalInput $ IChoice (ChoiceId "Exercise Call" w1Pk) 1])
    >>= step (TransactionInput (102, 102) [NormalInput $ IDeposit w1Pk w1Pk ada 10_000_000])
  where
    left err = assertFailure $ "Transactions are not expected to fail: " ++ show err
    right (w, p, _, c) =
      do
        assertClose c
        assertNoWarnings w
        assertTotalPayments w1Pk p (tokValueOf 30)
        assertTotalPayments w2Pk p (lovelaceValueOf 10_000_000)

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
   in either left right $
        -- initial margin payments
        step (TransactionInput (0, 0)
                [ NormalInput $ IDeposit w1Pk w1Pk ada 8_000_000
                , NormalInput $ IDeposit w2Pk w2Pk ada 8_000_000
                ]) ([], [], emptyState 0, contract)
        -- settlement
    >>= step (TransactionInput (99, 99)
                [ NormalInput $ IChoice dirRate 125_000_000
                , NormalInput $ IChoice invRate 80_000_000
                ])
  where
    left err = assertFailure $ "Transactions are not expected to fail: " ++ show err
    right (w, p, _, c) =
      do
        assertClose c
        assertNoWarnings $ filter nonZeroPay w
        -- repayments of inital margins
        assertTotalPayments w1Pk p (lovelaceValueOf 8_000_000)
        assertTotalPayments w2Pk p (lovelaceValueOf 8_000_000)

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
   in either left right $
        -- initial margin payments
        step (TransactionInput (0, 0)
                [ NormalInput $ IDeposit w1Pk w1Pk ada 8_000_000
                , NormalInput $ IDeposit w2Pk w2Pk ada 8_000_000
                ]) ([], [], emptyState 0, contract)
        -- settlement
    >>= step (TransactionInput (99, 99)
                [ NormalInput $ IChoice dirRate 133_333_333
                , NormalInput $ IChoice invRate 75_000_000
                ])
  where
    left err = assertFailure $ "Transactions are not expected to fail: " ++ show err
    right (w, p, _, c) =
      do
        assertClose c
        assertNoWarnings w
        assertTotalPayments w1Pk p (lovelaceValueOf 1_333_333)
        assertTotalPayments w2Pk p (lovelaceValueOf 14_666_667)

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
   in either left right $
        -- initial margin payments
        step (TransactionInput (0, 0)
                [ NormalInput $ IDeposit w1Pk w1Pk ada 8_000_000
                , NormalInput $ IDeposit w2Pk w2Pk ada 8_000_000
                ]) ([], [], emptyState 0, contract)
        -- margin call
    >>= step (TransactionInput (40, 40)
                [ NormalInput $ IChoice dirRate 200_000_000
                , NormalInput $ IChoice invRate 50_000_000
                ])
    >>= step (TransactionInput (42, 42)
                [ NormalInput $ IDeposit w1Pk w1Pk ada 60_000_000
                ])
        -- settlement
    >>= step (TransactionInput (99, 99)
                [ NormalInput $ IChoice dirRate 133_333_333
                , NormalInput $ IChoice invRate 75_000_000
                ])
  where
    left err = assertFailure $ "Transactions are not expected to fail: " ++ show err
    right (w, p, _, c) =
      do
        assertClose c
        assertNoWarnings w
        assertTotalPayments w1Pk p (lovelaceValueOf 61_333_333)
        assertTotalPayments w2Pk p (lovelaceValueOf 14_666_667)

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
   in either left right $
        step (TransactionInput (0, 0) [ NormalInput $ IDeposit w1Pk w1Pk ada 9_000_000 ]) ([], [], emptyState 0, contract)
    >>= step (TransactionInput (99, 99) [ NormalInput $ IDeposit w1Pk (Role "BondProvider") ada 10_000_000 ])
    >>= step (TransactionInput (100, 100)
                [ NormalInput $ IChoice (ChoiceId "Exercise Call" (Role "OptionCounterparty")) 1
                , NormalInput $ IDeposit (Role "OptionCounterparty") (Role "OptionCounterparty") tok 30
                ])
  where
    left err = assertFailure $ "Transactions are not expected to fail: " ++ show err
    right (w, p, _, c) =
      do
        assertClose c
        assertNoWarnings w
        assertTotalPayments w1Pk p (tokValueOf 30)

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
   in either left right $
        step (TransactionInput (0, 0) [ NormalInput $ IDeposit w1Pk w1Pk ada 9_000_000 ]) ([], [], emptyState 0, contract)
    >>= step (TransactionInput (99, 99) [ NormalInput $ IDeposit w1Pk (Role "BondProvider") ada 10_000_000 ])
    >>= step (TransactionInput (100, 100) [ NormalInput $ IChoice (ChoiceId "Exercise Call" (Role "OptionCounterparty")) 0 ])
  where
    left err = assertFailure $ "Transactions are not expected to fail: " ++ show err
    right (w, p, _, c) =
      do
        assertClose c
        assertNoWarnings w
        assertTotalPayments w1Pk p (lovelaceValueOf 10_000_000)
