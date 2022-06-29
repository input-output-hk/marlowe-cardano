{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.Actus.Examples
    (tests)
where

import Actus.Domain
import Actus.Marlowe
import Data.Maybe (fromJust)
import Data.Time.Calendar
import Data.Time.LocalTime
import Language.Marlowe
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
  let terms =
        nullContract
          { scheduleConfig =
              ScheduleConfig
                { businessDayConvention = Just BDC_NULL,
                  endOfMonthConvention = Just EOMC_EOM,
                  calendar = Just CLDR_NC
                },
            maturityDate = Just $ read "2030-01-01 00:00:00",
            contractId = "0",
            enableSettlement = False,
            initialExchangeDate = Just $ read "2020-01-01 00:00:00",
            contractRole = CR_RPA,
            penaltyType = Just PYTP_O,
            cycleAnchorDateOfInterestPayment = Just $ read "2020-01-01 00:00:00",
            contractType = PAM,
            notionalPrincipal = Just $ constant 10000,
            contractPerformance = Just PRF_PF,
            dayCountConvention = Just DCC_E30_360,
            accruedInterest = Just $ constant 0,
            statusDate = read "2019-12-31 00:00:00",
            cycleOfInterestPayment =
              Just $
                Cycle
                  { n = 1,
                    p = P_Y,
                    stub = ShortStub,
                    includeEndDay = False
                  },
            prepaymentEffect = Just PPEF_N,
            nominalInterestRate = Just $ constant 0.02,
            interestCalculationBase = Just IPCB_NT
          }
      contract = genContract' defaultRiskFactors terms
      principal = NormalInput $ IDeposit (Role "counterparty") "counterparty" ada 10000
      ip = NormalInput $ IDeposit (Role "party") "party" ada 200
      redemption = NormalInput $ IDeposit (Role "party") "party" ada 10000
   in case computeTransaction
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
        contract of
        Error err -> assertFailure $ "Transactions are not expected to fail: " ++ show err
        TransactionOutput txWarn txPay _ con -> do
          assertBool "Contract is in Close" $ con == Close
          assertBool "No warnings" $ null txWarn

          assertBool "total payments to party" (totalPayments (Party "party") txPay == 10000)
          assertBool "total payments to counterparty" (totalPayments (Party "counterparty") txPay == 12000)

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
  let terms =
        nullContract
          { scheduleConfig =
              ScheduleConfig
                { businessDayConvention = Just BDC_NULL,
                  endOfMonthConvention = Just EOMC_EOM,
                  calendar = Just CLDR_NC
                },
            maturityDate = Just $ read "2030-01-01 00:00:00",
            contractId = "0",
            enableSettlement = False,
            initialExchangeDate = Just $ read "2020-01-01 00:00:00",
            contractRole = CR_RPA,
            penaltyType = Just PYTP_O,
            cycleAnchorDateOfInterestPayment = Just $ read "2020-01-01 00:00:00",
            nextPrincipalRedemptionPayment = Just $ constant 1000,
            contractType = LAM,
            notionalPrincipal = Just $ constant 10000,
            contractPerformance = Just PRF_PF,
            cycleOfPrincipalRedemption =
              Just $
                Cycle
                  { n = 1,
                    p = P_Y,
                    stub = ShortStub,
                    includeEndDay = False
                  },
            dayCountConvention = Just DCC_E30_360,
            accruedInterest = Just $ constant 0,
            cycleAnchorDateOfPrincipalRedemption = Just $ read "2021-01-01 00:00:00",
            statusDate = read "2019-12-31 00:00:00",
            cycleOfInterestPayment =
              Just $
                Cycle
                  { n = 1,
                    p = P_Y,
                    stub = ShortStub,
                    includeEndDay = False
                  },
            prepaymentEffect = Just PPEF_N,
            nominalInterestRate = Just $ constant 0.02,
            interestCalculationBase = Just IPCB_NT
          }
      contract = genContract' defaultRiskFactors terms
      principal = NormalInput $ IDeposit (Role "counterparty") "counterparty" ada 10000
      pr i = NormalInput $ IDeposit (Role "party") "party" ada i
      ip i = NormalInput $ IDeposit (Role "party") "party" ada i
   in case computeTransaction
        ( TransactionInput
            (0, 0)
            [ principal,
              pr 1000,
              ip 200,
              pr 1000,
              ip 180,
              pr 1000,
              ip 160,
              pr 1000,
              ip 140,
              pr 1000,
              ip 120,
              pr 1000,
              ip 100,
              pr 1000,
              ip 80,
              pr 1000,
              ip 60,
              pr 1000,
              ip 40,
              ip 20,
              pr 1000
            ]
        )
        (emptyState 0)
        contract of
        Error _ -> assertFailure "Transactions are not expected to fail"
        TransactionOutput txWarn txPay _ con -> do
          assertBool "Contract is in Close" $ con == Close
          assertBool "No warnings" $ null txWarn

          assertBool "total payments to party" (totalPayments (Party "party") txPay == 10000)
          let tc = totalPayments (Party "counterparty") txPay
          assertBool ("total payments to counterparty: " ++ show tc) (tc == 11100)

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
  let terms =
        nullContract
          { scheduleConfig =
              ScheduleConfig
                { businessDayConvention = Just BDC_NULL,
                  endOfMonthConvention = Just EOMC_EOM,
                  calendar = Just CLDR_NC
                },
            maturityDate = Just $ read "2030-01-01 00:00:00",
            contractId = "0",
            enableSettlement = False,
            initialExchangeDate = Just $ read "2020-01-01 00:00:00",
            contractRole = CR_RPA,
            cycleAnchorDateOfInterestCalculationBase = Just $ read "2020-01-01 00:00:00",
            penaltyType = Just PYTP_O,
            cycleOfInterestCalculationBase =
              Just $
                Cycle
                  { n = 1,
                    p = P_Y,
                    stub = ShortStub,
                    includeEndDay = False
                  },
            interestCalculationBaseA = Just $ constant 1000,
            cycleAnchorDateOfInterestPayment = Just $ read "2020-01-01 00:00:00",
            nextPrincipalRedemptionPayment = Just $ constant 1000,
            contractType = NAM,
            notionalPrincipal = Just $ constant 10000,
            contractPerformance = Just PRF_PF,
            cycleOfPrincipalRedemption =
              Just $
                Cycle
                  { n = 1,
                    p = P_Y,
                    stub = ShortStub,
                    includeEndDay = False
                  },
            dayCountConvention = Just DCC_E30_360,
            accruedInterest = Just $ constant 0,
            cycleAnchorDateOfPrincipalRedemption = Just $ read "2021-01-01 00:00:00",
            statusDate = read "2019-12-31 00:00:00",
            cycleOfInterestPayment =
              Just $
                Cycle
                  { n = 1,
                    p = P_Y,
                    stub = ShortStub,
                    includeEndDay = False
                  },
            prepaymentEffect = Just PPEF_N,
            nominalInterestRate = Just $ constant 0.02,
            interestCalculationBase = Just IPCB_NT
          }
      contract = genContract' defaultRiskFactors terms
      principal = NormalInput $ IDeposit (Role "counterparty") "counterparty" ada 10000
      pr i = NormalInput $ IDeposit (Role "party") "party" ada i
      ip i = NormalInput $ IDeposit (Role "party") "party" ada i
   in case computeTransaction
        ( TransactionInput
            (0, 0)
            [ principal,
              pr 800,
              ip 200,
              pr 816,
              ip 184,
              pr 832,
              ip 168,
              pr 849,
              ip 151,
              pr 866,
              ip 134,
              pr 883,
              ip 117,
              pr 901,
              ip 99,
              pr 919,
              ip 81,
              pr 937,
              ip 63,
              ip 44,
              pr 2196
            ]
        )
        (emptyState 0)
        contract of
        Error _ -> assertFailure "Transactions are not expected to fail"
        TransactionOutput txWarn txPay _ con -> do
          assertBool "Contract is in Close" $ con == Close
          assertBool "No warnings" $ null txWarn

          assertBool "total payments to party" (totalPayments (Party "party") txPay == 10000)
          let tc = totalPayments (Party "counterparty") txPay
          assertBool ("total payments to counterparty: " ++ show tc) (tc == 11240)

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
  let terms =
        nullContract
          { scheduleConfig =
              ScheduleConfig
                { businessDayConvention = Just BDC_NULL,
                  endOfMonthConvention = Just EOMC_EOM,
                  calendar = Just CLDR_NC
                },
            maturityDate = Just $ read "2030-01-01 00:00:00",
            contractId = "0",
            enableSettlement = False,
            initialExchangeDate = Just $ read "2020-01-01 00:00:00",
            contractRole = CR_RPA,
            penaltyType = Just PYTP_O,
            cycleAnchorDateOfInterestPayment = Just $ read "2020-01-01 00:00:00",
            nextPrincipalRedemptionPayment = Just $ constant 1000,
            contractType = ANN,
            notionalPrincipal = Just $ constant 10000,
            contractPerformance = Just PRF_PF,
            cycleOfPrincipalRedemption =
              Just $
                Cycle
                  { n = 1,
                    p = P_Y,
                    stub = ShortStub,
                    includeEndDay = False
                  },
            dayCountConvention = Just DCC_E30_360,
            accruedInterest = Just $ constant 0,
            cycleAnchorDateOfPrincipalRedemption = Just $ read "2021-01-01 00:00:00",
            statusDate = read "2019-12-31 00:00:00",
            cycleOfInterestPayment =
              Just $
                Cycle
                  { n = 1,
                    p = P_Y,
                    stub = ShortStub,
                    includeEndDay = False
                  },
            prepaymentEffect = Just PPEF_N,
            nominalInterestRate = Just $ constant 0.02,
            interestCalculationBase = Just IPCB_NT
          }
      contract = genContract' defaultRiskFactors terms
      principal = NormalInput $ IDeposit (Role "counterparty") "counterparty" ada 10000
      pr i = NormalInput $ IDeposit (Role "party") "party" ada i
      ip i = NormalInput $ IDeposit (Role "party") "party" ada i
   in case computeTransaction
        ( TransactionInput
            (0, 0)
            [ principal,
              pr 800,
              ip 200,
              pr 816,
              ip 184,
              pr 832,
              ip 168,
              pr 849,
              ip 151,
              pr 866,
              ip 134,
              pr 883,
              ip 117,
              pr 901,
              ip 99,
              pr 919,
              ip 81,
              pr 937,
              ip 63,
              ip 44,
              pr 2196
            ]
        )
        (emptyState 0)
        contract of
        Error _ -> assertFailure "Transactions are not expected to fail"
        TransactionOutput txWarn txPay _ con -> do
          assertBool "Contract is in Close" $ con == Close
          assertBool "No warnings" $ null txWarn

          assertBool "total payments to party" (totalPayments (Party "party") txPay == 10000)
          let tc = totalPayments (Party "counterparty") txPay
          assertBool ("total payments to counterparty: " ++ show tc) (tc == 11240)

-- |ex_optns1 defines a contract of type OPTNS
ex_optns1 :: IO ()
ex_optns1 =
  let terms =
        nullContract
          { priceAtPurchaseDate = Just $ constant 10,
            purchaseDate = Just $ read "2020-01-02 00:00:00",
            contractStructure =
              [ ContractStructure
                  { referenceRole = UDL,
                    referenceType = MOC,
                    reference =
                      ReferenceId $
                        Identifier
                          { marketObjectCode = Just "XXX",
                            contractIdentifier = Nothing
                          }
                  }
              ],
            scheduleConfig =
              ScheduleConfig
                { businessDayConvention = Nothing,
                  endOfMonthConvention = Nothing,
                  calendar = Just CLDR_NC
                },
            maturityDate = Just $ read "2020-03-30 00:00:00",
            contractId = "option01",
            enableSettlement = False,
            deliverySettlement = Just DS_S,
            contractRole = CR_RPA,
            optionType = Just OPTP_C,
            settlementPeriod =
              Just $
                Cycle
                  { n = 0,
                    p = P_D,
                    stub = LongStub,
                    includeEndDay = False
                  },
            optionStrike1 = Just $ constant 80,
            contractType = OPTNS,
            statusDate = read "2020-01-01 00:00:00",
            optionExerciseType = Just OPXT_E
          }
      contract = genContract' rf terms
      principal = NormalInput . IDeposit (Role "counterparty") "counterparty" ada
      ex = NormalInput . IDeposit (Role "party") "party" ada
      rf XD d
        | d == (fromJust $ maturityDate terms) =
          RiskFactors
            { o_rf_CURS = _one,
              o_rf_RRMO = _one,
              o_rf_SCMO = _one,
              pp_payoff = _zero,
              xd_payoff = constant 120,
              dv_payoff = _zero
            }
      rf _ _ =
        RiskFactors
          { o_rf_CURS = _one,
            o_rf_RRMO = _one,
            o_rf_SCMO = _one,
            pp_payoff = _zero,
            xd_payoff = _zero,
            dv_payoff = _zero
          }
   in case computeTransaction
        ( TransactionInput
            (0, 0)
            [ principal 10,
              ex 40
            ]
        )
        (emptyState 0)
        contract of
        Error _ -> assertFailure "Transactions are not expected to fail"
        TransactionOutput txWarn txPay _ con -> do
          assertBool "Contract is in Close" $ con == Close
          assertBool "No warnings" $ null txWarn

          assertBool "total payments to party" (totalPayments (Party "party") txPay == 10)
          let tc = totalPayments (Party "counterparty") txPay
          assertBool ("total payments to counterparty: " ++ show tc) (tc == 40)

-- |ex_com1 defines a contract of type COM
ex_com1 :: IO ()
ex_com1 =
  let terms =
        nullContract
          { contractType = COM,
            contractId = "com01",
            statusDate = read "2014-12-30 00:00:00",
            -- contractDealDate = Just $ read "2014-12-15 00:00:00",
            purchaseDate = Just $ read "2015-03-30 00:00:00",
            priceAtPurchaseDate = Just $ constant 700,
            contractRole = CR_RPL,
            -- currency = Just "USD",
            quantity = Just $ constant 2
            -- unit = Just "BRL"
          }
      contract = genContract' defaultRiskFactors terms
      principal = NormalInput . IDeposit (Role "party") "party" ada
   in case computeTransaction
        ( TransactionInput
            (0, 0)
            [ principal 1400
            ]
        )
        (emptyState 0)
        contract of
        Error _ -> assertFailure "Transactions are not expected to fail"
        TransactionOutput txWarn txPay _ con -> do
          assertBool "Contract is in Close" $ con == Close
          assertBool "No warnings" $ null txWarn

          assertBool "total payments to party" (totalPayments (Party "party") txPay == 0)
          let tc = totalPayments (Party "counterparty") txPay
          assertBool ("total payments to counterparty: " ++ show tc) (tc == 1400)

defaultRiskFactors :: ActusOps a => EventType -> LocalTime -> RiskFactors a
defaultRiskFactors _ _ =
  RiskFactors
    { o_rf_CURS = _one,
      o_rf_RRMO = _one,
      o_rf_SCMO = _one,
      pp_payoff = _zero,
      xd_payoff = _zero,
      dv_payoff = _zero
    }

-- |totalPayments calculates the sum of the payments provided as argument
totalPayments :: Payee -> [Payment] -> Integer
totalPayments payee = sum . map m . filter f
  where
    m (Payment _ _ mon) = Val.valueOf mon "" ""
    f (Payment _ pay _) = pay == payee

nullContract :: ContractTermsMarlowe
nullContract =
  ContractTerms
    { contractId = "",
      contractType = PAM,
      contractStructure = [],
      contractRole = CR_RPA,
      settlementCurrency = Nothing,
      initialExchangeDate = Nothing,
      dayCountConvention = Nothing,
      scheduleConfig = ScheduleConfig Nothing Nothing Nothing,
      statusDate = LocalTime (ModifiedJulianDay 0) (TimeOfDay 0 0 0),
      contractPerformance = Nothing,
      creditEventTypeCovered = Nothing,
      coverageOfCreditEnhancement = Nothing,
      guaranteedExposure = Nothing,
      cycleOfFee = Nothing,
      cycleAnchorDateOfFee = Nothing,
      feeAccrued = Nothing,
      feeBasis = Nothing,
      feeRate = Nothing,
      cycleAnchorDateOfInterestPayment = Nothing,
      cycleOfInterestPayment = Nothing,
      accruedInterest = Nothing,
      capitalizationEndDate = Nothing,
      cycleAnchorDateOfInterestCalculationBase = Nothing,
      cycleOfInterestCalculationBase = Nothing,
      interestCalculationBase = Nothing,
      interestCalculationBaseA = Nothing,
      nominalInterestRate = Nothing,
      nominalInterestRate2 = Nothing,
      interestScalingMultiplier = Nothing,
      notionalPrincipal = Nothing,
      premiumDiscountAtIED = Nothing,
      maturityDate = Nothing,
      amortizationDate = Nothing,
      exerciseDate = Nothing,
      cycleAnchorDateOfPrincipalRedemption = Nothing,
      cycleOfPrincipalRedemption = Nothing,
      nextPrincipalRedemptionPayment = Nothing,
      purchaseDate = Nothing,
      priceAtPurchaseDate = Nothing,
      terminationDate = Nothing,
      priceAtTerminationDate = Nothing,
      quantity = Nothing,
      scalingIndexAtStatusDate = Nothing,
      cycleAnchorDateOfScalingIndex = Nothing,
      cycleOfScalingIndex = Nothing,
      scalingEffect = Nothing,
      scalingIndexAtContractDealDate = Nothing,
      marketObjectCodeOfScalingIndex = Nothing,
      notionalScalingMultiplier = Nothing,
      cycleOfOptionality = Nothing,
      cycleAnchorDateOfOptionality = Nothing,
      optionType = Nothing,
      optionStrike1 = Nothing,
      optionExerciseType = Nothing,
      settlementPeriod = Nothing,
      deliverySettlement = Nothing,
      exerciseAmount = Nothing,
      futuresPrice = Nothing,
      penaltyRate = Nothing,
      penaltyType = Nothing,
      prepaymentEffect = Nothing,
      cycleOfRateReset = Nothing,
      cycleAnchorDateOfRateReset = Nothing,
      nextResetRate = Nothing,
      rateSpread = Nothing,
      rateMultiplier = Nothing,
      periodFloor = Nothing,
      periodCap = Nothing,
      lifeCap = Nothing,
      lifeFloor = Nothing,
      marketObjectCodeOfRateReset = Nothing,
      cycleOfDividend = Nothing,
      cycleAnchorDateOfDividend = Nothing,
      nextDividendPaymentAmount = Nothing,
      enableSettlement = False,
      constraints = Nothing
    }

