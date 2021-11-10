{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.Marlowe.ACTUS.Examples
    (tests)
where

import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy as B (readFile)
import Data.Maybe (fromJust)
import Data.Time.LocalTime
import Data.Validation (Validation (..))
import Language.Marlowe
import Language.Marlowe.ACTUS.Domain.BusinessEvents (EventType (..), RiskFactors, RiskFactorsPoly (..))
import Language.Marlowe.ACTUS.Domain.ContractTerms
import Language.Marlowe.ACTUS.Generator.GeneratorFs
import Language.Marlowe.ACTUS.Generator.GeneratorStatic
import qualified Ledger.Value as Val
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Marlowe represenation of sample ACTUS contracts"
  [ testCase "PAM examples" ex_pam1
  , testCase "LAM examples" ex_lam1
  , testCase "NAM examples" ex_nam1
  , testCase "ANN examples" ex_ann1
  , testCase "OPTNS examples" ex_optns1
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
  contractFromFile "test/Spec/Marlowe/ACTUS/ex_pam1.json"
    >>= either
      ( \err -> assertFailure ("Error parsing file: " ++ err))
      ( \ct -> case genFsContract defaultRiskFactors ct of
          Failure _ -> assertFailure "Terms validation should not fail"
          Success contract ->
            let principal = IDeposit (Role "counterparty") "counterparty" ada 10000
                ip = IDeposit (Role "party") "party" ada 200
                redemption = IDeposit (Role "party") "party" ada 10000

                out =
                  computeTransaction
                    ( TransactionInput
                        (0, 0)
                        [ principal,
                          ip,
                          ip,
                          ip,
                          ip,
                          ip,
                          ip,
                          ip,
                          ip,
                          ip,
                          ip,
                          redemption
                        ]
                    )
                    (emptyState 0)
                    contract
             in case out of
                  Error _ -> assertFailure "Transactions are not expected to fail"
                  TransactionOutput txWarn txPay _ con -> do
                    assertBool "Contract is in Close" $ con == Close
                    assertBool "No warnings" $ null txWarn

                    assertBool "total payments to party" (totalPayments (Party "party") txPay == 10000)
                    assertBool "total payments to counterparty" (totalPayments (Party "counterparty") txPay == 12000)
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
  contractFromFile "test/Spec/Marlowe/ACTUS/ex_lam1.json"
    >>= either
      ( \err -> assertFailure ("Error parsing file: " ++ err))
      ( \ct -> case genFsContract defaultRiskFactors ct of
          Failure _ -> assertFailure "Terms validation should not fail"
          Success contract -> do
            let principal = IDeposit (Role "counterparty") "counterparty" ada 10000
                pr i = IDeposit (Role "party") "party" ada i
                ip i = IDeposit (Role "party") "party" ada i
                out =
                  computeTransaction
                    ( TransactionInput
                        (0, 0)
                        [ principal,
                          pr 1000, ip 200,
                          pr 1000, ip 180,
                          pr 1000, ip 160,
                          pr 1000, ip 140,
                          pr 1000, ip 120,
                          pr 1000, ip 100,
                          pr 1000, ip 80,
                          pr 1000, ip 60,
                          pr 1000, ip 40,
                                   ip 20,
                          pr 1000
                        ]
                    )
                    (emptyState 0)
                    contract
             in case out of
                  Error _ -> assertFailure "Transactions are not expected to fail"
                  TransactionOutput txWarn txPay _ con -> do
                    assertBool "Contract is in Close" $ con == Close
                    assertBool "No warnings" $ null txWarn

                    assertBool "total payments to party" (totalPayments (Party "party") txPay == 10000)
                    let tc = totalPayments (Party "counterparty") txPay
                    assertBool ("total payments to counterparty: " ++ show tc) (tc == 11100)
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
  contractFromFile "test/Spec/Marlowe/ACTUS/ex_nam1.json"
    >>= either
      ( \err -> assertFailure ("Error parsing file: " ++ err))
      ( \ct -> case genFsContract defaultRiskFactors ct of
        Failure _ -> assertFailure "Terms validation should not fail"
        Success contract -> do
          let principal = IDeposit (Role "counterparty") "counterparty" ada 10000
              pr i = IDeposit (Role "party") "party" ada i
              ip i = IDeposit (Role "party") "party" ada i
              out =
                computeTransaction
                  ( TransactionInput
                      (0, 0)
                      [ principal,
                        pr 800, ip 200,
                        pr 816, ip 184,
                        pr 832, ip 168,
                        pr 849, ip 151,
                        pr 866, ip 134,
                        pr 883, ip 117,
                        pr 901, ip 99,
                        pr 919, ip 81,
                        pr 937, ip 63,
                                ip 44,
                        pr 2196
                      ]
                  )
                  (emptyState 0)
                  contract
           in case out of
                Error _ -> assertFailure "Transactions are not expected to fail"
                TransactionOutput txWarn txPay _ con -> do
                  assertBool "Contract is in Close" $ con == Close
                  assertBool "No warnings" $ null txWarn

                  assertBool "total payments to party" (totalPayments (Party "party") txPay == 10000)
                  let tc = totalPayments (Party "counterparty") txPay
                  assertBool ("total payments to counterparty: " ++ show tc) (tc == 11240)
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
  contractFromFile "test/Spec/Marlowe/ACTUS/ex_ann1.json"
    >>= either
      ( \err -> assertFailure ("Error parsing file: " ++ err))
      ( \ct -> case genFsContract defaultRiskFactors ct of
        Failure _ -> assertFailure "Terms validation should not fail"
        Success contract -> do
          let principal = IDeposit (Role "counterparty") "counterparty" ada 10000
              pr i = IDeposit (Role "party") "party" ada i
              ip i = IDeposit (Role "party") "party" ada i
              out =
                computeTransaction
                  ( TransactionInput
                      (0, 0)
                      [ principal,
                        pr 800, ip 200,
                        pr 816, ip 184,
                        pr 832, ip 168,
                        pr 849, ip 151,
                        pr 866, ip 134,
                        pr 883, ip 117,
                        pr 901, ip 99,
                        pr 919, ip 81,
                        pr 937, ip 63,
                                ip 44,
                        pr 2196
                      ]
                  )
                  (emptyState 0)
                  contract
           in case out of
                Error _ -> assertFailure "Transactions are not expected to fail"
                TransactionOutput txWarn txPay _ con -> do
                  assertBool "Contract is in Close" $ con == Close
                  assertBool "No warnings" $ null txWarn

                  assertBool "total payments to party" (totalPayments (Party "party") txPay == 10000)
                  let tc = totalPayments (Party "counterparty") txPay
                  assertBool ("total payments to counterparty: " ++ show tc) (tc == 11240)
      )

-- |ex_optns1 defines a contract of type OPTNS
ex_optns1 :: IO ()
ex_optns1 =
  contractFromFile "test/Spec/Marlowe/ACTUS/ex_optns1.json"
    >>= either msg run
  where
    msg err = putStr err
    run ct = case genStaticContract rf ct of
      Failure _ -> assertFailure "Terms validation should not fail"
      Success contract -> -- Prelude.writeFile "option2.marlowe" (show $ pretty contract)
          let principal = IDeposit (Role "counterparty") "counterparty" ada
              ex = IDeposit (Role "party") "party" ada
              out =
                computeTransaction
                  ( TransactionInput
                      (0, 0)
                      [ principal 10,
                        ex 40
                      ]
                  )
                  (emptyState 0)
                  contract
           in case out of
                Error _ -> assertFailure "Transactions are not expected to fail"
                TransactionOutput txWarn txPay _ con -> do
                  assertBool "Contract is in Close" $ con == Close
                  assertBool "No warnings" $ null txWarn

                  assertBool "total payments to party" (totalPayments (Party "party") txPay == 10)
                  let tc = totalPayments (Party "counterparty") txPay
                  assertBool ("total payments to counterparty: " ++ show tc) (tc == 40)

      where
        rf :: EventType -> LocalTime -> RiskFactors
        rf XD d
          | d == (fromJust $ maturityDate ct) =
            RiskFactorsPoly
              { o_rf_CURS = 1.0,
                o_rf_RRMO = 1.0,
                o_rf_SCMO = 1.0,
                pp_payoff = 0.0,
                xd_payoff = 120.0,
                dv_payoff = 0.0
              }
        rf _ _ =
          RiskFactorsPoly
            { o_rf_CURS = 1.0,
              o_rf_RRMO = 1.0,
              o_rf_SCMO = 1.0,
              pp_payoff = 0.0,
              xd_payoff = 0.0,
              dv_payoff = 0.0
            }

contractFromFile :: FilePath -> IO (Either String ContractTerms)
contractFromFile f = eitherDecode <$> B.readFile f

defaultRiskFactors :: EventType -> LocalTime -> RiskFactors
defaultRiskFactors _ _ =
  RiskFactorsPoly
    { o_rf_CURS = 1.0,
      o_rf_RRMO = 1.0,
      o_rf_SCMO = 1.0,
      pp_payoff = 0.0,
      xd_payoff = 0.0,
      dv_payoff = 0.0
    }

-- |totalPayments calculates the sum of the payments provided as argument
totalPayments :: Payee -> [Payment] -> Integer
totalPayments payee = sum . map m . filter f
  where
    m (Payment _ _ mon) = Val.valueOf mon "" ""
    f (Payment _ pay _) = pay == payee
