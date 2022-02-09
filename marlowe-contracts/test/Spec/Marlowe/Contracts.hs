{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}

module Spec.Marlowe.Contracts
    (tests)
where

import Language.Marlowe
import Ledger.Ada
import Ledger.Value
import Marlowe.Contracts.Common
import Marlowe.Contracts.Options
import Marlowe.Contracts.Swap
import Marlowe.Contracts.ZeroCouponBond
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Marlowe Contract"
  [ testCase "ZeroCouponBond" zeroCouponBondTest
  , testCase "Swap Contract" swapContractTest
  , testCase "American Call Option test" americanCallOptionTest
  , testCase "American Call Option test - Exercise" americanCallOptionExercisedTest
  , testCase "European Call Option test" europeanCallOptionTest
  , testCase "European Call Option test - Exercise" europeanCallOptionExercisedTest
  ]

w1Pk, w2Pk :: Party
w1Pk = Role "party1"
w2Pk = Role "party2"

tok :: Token
tok = Token "" "testcoin" -- tokSymbol tokName

tokSymbol :: CurrencySymbol
tokSymbol = ""

tokName :: TokenName
tokName = "testcoin"

tokValueOf :: Integer -> Money
tokValueOf = singleton tokSymbol tokName

assertTotalPayments :: Party -> [Payment] -> Money -> Assertion
assertTotalPayments p t x = assertBool "total payments to party" (totalPayments (Party p) t == x)

assertNoWarnings :: [a] -> Assertion
assertNoWarnings t = assertBool "No warnings" $ null t

assertClose :: Contract -> Assertion
assertClose = assertBool "Contract is in Close" . (Close==)

totalPayments :: Payee -> [Payment] -> Money
totalPayments p' = mconcat . map m . filter f
  where
    m (Payment _ _ mon) = mon
    f (Payment _ p _) = p == p'

step :: TransactionInput -> ([TransactionWarning], [Payment], State, Contract) -> Either TransactionError ([TransactionWarning], [Payment], State, Contract)
step i (_,_,s,c) =
  case computeTransaction i s c of
    Error err                     -> Left err
    (TransactionOutput w p s' c') -> Right (w, p, s', c')

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
  let americanCall =
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
          (Slot 10)
          Close
          americanCall
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
