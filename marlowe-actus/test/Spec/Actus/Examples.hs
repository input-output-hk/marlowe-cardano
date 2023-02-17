{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Spec.Actus.Examples
  ( tests
  ) where

import qualified Actus.Core as Core
import Actus.Domain
import Actus.Marlowe
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy as B (readFile)
import Data.Maybe (fromJust, mapMaybe)
import Data.Time.LocalTime
import Language.Marlowe
import Language.Marlowe.Extended.V1 as Ex (ada, toCore)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Marlowe represenation of sample ACTUS contracts"
  [ testCase "PAM examples" ex_pam1
  , testCase "LAM examples" ex_lam1
  , testCase "NAM examples" ex_nam1
  , testCase "ANN examples" ex_ann1
  , testCase "OPTNS examples" ex_optns1
  , testCase "COM examples" ex_com1
  ]

-- |ex_pam1 defines a contract of type PAM
--
-- principal: 10000
-- interest rate: 2% p.a.
-- annual interest payments
-- term: 10 years
--
-- cashflows:
-- 0 : -10000
-- 1 :    200
-- 2 :    200
-- 3 :    200
-- 4 :    200
-- 5 :    200
-- 6 :    200
-- 7 :    200
-- 8 :    200
-- 9 :    200
-- 10:  10200
ex_pam1 :: IO ()
ex_pam1 =
  contractFromFile "test/Spec/Actus/ex_pam1.json"
    >>= either
      (\err -> assertFailure ("Error parsing file: " ++ err))
      ( \ct ->
          let Just contract = toCore $ genContract' ("party", "counterparty") emptyRiskFactors ct
              cashFlows = Core.genProjectedCashflows emptyRiskFactors ct []
           in case computeTransaction
                ( TransactionInput
                    (0, 0)
                    (mapMaybe cashFlowToInput cashFlows)
                )
                (emptyState 0)
                contract of
                Error err -> assertFailure $ "Transactions are not expected to fail: " ++ show err
                TransactionOutput txWarn txPay _ con -> do
                  assertBool "Contract is in Close" $ con == Close
                  assertBool "No warnings" $ null txWarn

                  assertBool "total payments to counterparty" (totalPayments (Party "counterparty") txPay == 10_000)
                  assertBool "total payments to party" (totalPayments (Party "party") txPay == 12_000)
      )

-- |ex_lam1 defines a contract of type LAM
--
-- principal: 10000
-- interest rate: 2% p.a.
-- annual interest payments
-- term: 10 years
--
-- cashflows:
-- 0 : -10000
-- 1 :   1200
-- 2 :   1180
-- 3 :   1160
-- 4 :   1140
-- 5 :   1120
-- 6 :   1100
-- 7 :   1080
-- 8 :   1060
-- 9 :   1040
-- 10:   1020
ex_lam1 :: IO ()
ex_lam1 =
  contractFromFile "test/Spec/Actus/ex_lam1.json"
    >>= either
      (\err -> assertFailure ("Error parsing file: " ++ err))
      ( \ct ->
          let Just contract = toCore $ genContract' ("party", "counterparty") emptyRiskFactors ct
              cashFlows = Core.genProjectedCashflows emptyRiskFactors ct []
           in case computeTransaction
                ( TransactionInput
                    (0, 0)
                    (mapMaybe cashFlowToInput cashFlows)
                )
                (emptyState 0)
                contract of
                Error err -> assertFailure $ "Transactions are not expected to fail: " ++ show err
                TransactionOutput txWarn txPay _ con -> do
                  assertBool "Contract is in Close" $ con == Close
                  assertBool "No warnings" $ null txWarn

                  assertBool "total payments to counterparty" (totalPayments (Party "counterparty") txPay == 10_000)
                  let tc = totalPayments (Party "party") txPay
                  assertBool ("total payments to party: " ++ show tc) (tc == 11_100)
      )


-- |ex_nam1 defines a contract of type NAM
--
-- principal: 10000
-- interest rate: 2% p.a.
-- annual interest payments
-- term: 10 years
--
-- cashflows:
-- 0 : -10000
-- 1 :   1000
-- 2 :   1000
-- 3 :   1000
-- 4 :   1000
-- 5 :   1000
-- 6 :   1000
-- 7 :   1000
-- 8 :   1000
-- 9 :   1000
-- 10:   2240
ex_nam1 :: IO ()
ex_nam1 =
  contractFromFile "test/Spec/Actus/ex_nam1.json"
    >>= either
      (\err -> assertFailure ("Error parsing file: " ++ err))
      ( \ct ->
          let Just contract = toCore $ genContract' ("party", "counterparty") emptyRiskFactors ct
              cashFlows = Core.genProjectedCashflows emptyRiskFactors ct []
           in case computeTransaction
                ( TransactionInput
                    (0, 0)
                    (mapMaybe cashFlowToInput cashFlows)
                )
                (emptyState 0)
                contract of
                Error err -> assertFailure $ "Transactions are not expected to fail: " ++ show err
                TransactionOutput txWarn txPay _ con -> do
                  assertBool "Contract is in Close" $ con == Close
                  assertBool "No warnings" $ null txWarn

                  assertBool "total payments to counterparty" (totalPayments (Party "counterparty") txPay == 10_000)
                  let tc = totalPayments (Party "party") txPay
                  assertBool ("total payments to party: " ++ show tc) (tc == 11_240)
      )


-- |ex_ann1 defines a contract of type ANN
--
-- principal: 10000
-- interest rate: 2% p.a.
-- annual interest payments
-- term: 10 years
--
-- cashflows:
-- 0 : -10000
-- 1 :   1000
-- 2 :   1000
-- 3 :   1000
-- 4 :   1000
-- 5 :   1000
-- 6 :   1000
-- 7 :   1000
-- 8 :   1000
-- 9 :   1000
-- 10:   2240
ex_ann1 :: IO ()
ex_ann1 =
  contractFromFile "test/Spec/Actus/ex_ann1.json"
    >>= either
      (\err -> assertFailure ("Error parsing file: " ++ err))
      ( \ct ->
          let Just contract = toCore $ genContract' ("party", "counterparty") emptyRiskFactors ct
              cashFlows = Core.genProjectedCashflows emptyRiskFactors ct []
           in case computeTransaction
                ( TransactionInput
                    (0, 0)
                    (mapMaybe cashFlowToInput cashFlows)
                )
                (emptyState 0)
                contract of
                Error err -> assertFailure $ "Transactions are not expected to fail: " ++ show err
                TransactionOutput txWarn txPay _ con -> do
                  assertBool "Contract is in Close" $ con == Close
                  assertBool "No warnings" $ null txWarn

                  assertBool "total payments to counterparty" (totalPayments (Party "counterparty") txPay == 10_000)
                  let tc = totalPayments (Party "party") txPay
                  assertBool ("total payments to party: " ++ show tc) (tc == 11_240)
      )

-- |ex_optns1 defines a contract of type OPTNS
ex_optns1 :: IO ()
ex_optns1 =
  contractFromFile "test/Spec/Actus/ex_optns1.json"
    >>= either
      (\err -> assertFailure ("Error parsing file: " ++ err))
      ( \ct ->
          let Just contract = toCore $ genContract' ("party", "counterparty") rf ct
              rf :: Num a => String -> EventType -> LocalTime -> RiskFactors a
              rf _ XD d
                | d == fromJust (maturityDate ct) =
                  RiskFactors
                    { o_rf_CURS = 1,
                      o_rf_RRMO = 1,
                      o_rf_SCMO = 1,
                      pp_payoff = 0,
                      xd_payoff = 120,
                      dv_payoff = 0
                    }
              rf _ _ _ =
                RiskFactors
                  { o_rf_CURS = 1,
                    o_rf_RRMO = 1,
                    o_rf_SCMO = 1,
                    pp_payoff = 0,
                    xd_payoff = 0,
                    dv_payoff = 0
                  }
              cashFlows = Core.genProjectedCashflows rf ct []
           in case computeTransaction
                ( TransactionInput
                    (0, 0)
                    (mapMaybe cashFlowToInput cashFlows)
                )
                (emptyState 0)
                contract of
                Error err -> assertFailure $ "Transactions are not expected to fail: " ++ show err
                TransactionOutput txWarn txPay _ con -> do
                  assertBool "Contract is in Close" $ con == Close
                  assertBool "No warnings" $ null txWarn

                  assertBool "total payments to counterparty" (totalPayments (Party "counterparty") txPay == 10)
                  let tc = totalPayments (Party "party") txPay
                  assertBool ("total payments to party: " ++ show tc) (tc == 40)
      )

-- |ex_com1 defines a contract of type COM
ex_com1 :: IO ()
ex_com1 =
  contractFromFile "test/Spec/Actus/ex_com1.json"
    >>= either
      (\err -> assertFailure ("Error parsing file: " ++ err))
      ( \ct ->
          let Just contract = toCore $ genContract' ("party", "counterparty") emptyRiskFactors ct
              cashFlows = Core.genProjectedCashflows emptyRiskFactors ct []
           in case computeTransaction
                ( TransactionInput
                    (0, 0)
                    (mapMaybe cashFlowToInput cashFlows)
                )
                (emptyState 0)
                contract of
                Error err -> assertFailure $ "Transactions are not expected to fail: " ++ show err
                TransactionOutput txWarn txPay _ con -> do
                  assertBool "Contract is in Close" $ con == Close
                  assertBool "No warnings" $ null txWarn

                  assertBool "total payments to counterparty" (totalPayments (Party "counterparty") txPay == 0)
                  let tc = totalPayments (Party "party") txPay
                  assertBool ("total payments to party: " ++ show tc) (tc == 1_400)
      )

emptyRiskFactors :: Num a => String -> EventType -> LocalTime -> RiskFactors a
emptyRiskFactors _ _ _ =
  RiskFactors
    { o_rf_CURS = 1,
      o_rf_RRMO = 1,
      o_rf_SCMO = 1,
      pp_payoff = 0,
      xd_payoff = 0,
      dv_payoff = 0
    }

-- |totalPayments calculates the sum of the payments provided as argument
totalPayments :: Payee -> [Payment] -> Integer
totalPayments payee = fromMarloweFixedPoint . sum . map m . filter f
  where
    m (Payment _ _ (Token "" "") amt) = amt
    m _                               = 0
    f (Payment _ pay _ _) = pay == payee

contractFromFile :: FilePath -> IO (Either String (ContractTerms Double))
contractFromFile f = eitherDecode <$> B.readFile f

partyDeposit :: Integer -> Input
partyDeposit = NormalInput . IDeposit (Role "party") "party" ada

counterpartyDeposit :: Integer -> Input
counterpartyDeposit = NormalInput . IDeposit (Role "counterparty") "counterparty" ada

cashFlowToInput :: CashFlow Double -> Maybe Input
cashFlowToInput CashFlow{..} | amount < 0 = Just $ partyDeposit $ toMarloweFixedPoint (-amount)
cashFlowToInput CashFlow{..} | amount > 0 = Just $ counterpartyDeposit $ toMarloweFixedPoint amount
cashFlowToInput _                         = Nothing
