{-# LANGUAGE OverloadedStrings #-}
module Spec.Marlowe.ACTUS.Examples
    (tests)
where

import           Data.Maybe                                       (fromJust)
import           Data.Time.Format.ISO8601
import           Data.Validation                                  (Validation (..))
import           Language.Marlowe
import           Language.Marlowe.ACTUS.Definitions.ContractTerms
import           Language.Marlowe.ACTUS.Generator
import qualified Ledger.Value                                     as Val
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Marlowe represenation of sample ACTUS contracts"
  [
    testCase "PAM example01" example01
  , testCase "LAM example02" example02
  , testCase "NAM example03" example03
  , testCase "ANN example04" example04
  ]

-- |example01 defines a contract of type PAM
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
example01 :: IO ()
example01 =
  let ct =
        ContractTermsPoly
          { contractId = "0",
            contractType = PAM,
            contractStructure = [],
            initialExchangeDate = iso8601ParseM "2020-01-01T00:00:00",
            statusDate = fromJust $ iso8601ParseM "2019-12-31T00:00:00",
            maturityDate = iso8601ParseM "2030-01-01T00:00:00",
            amortizationDate = Nothing,
            exerciseDate = Nothing,
            terminationDate = Nothing,
            nextPrincipalRedemptionPayment = Nothing,
            purchaseDate = Nothing,
            contractRole = CR_RPA,
            premiumDiscountAtIED = Nothing,
            notionalPrincipal = Just 10000.0,
            priceAtPurchaseDate = Nothing,
            priceAtTerminationDate = Nothing,
            dayCountConvention = Just DCC_E30_360,
            prepaymentEffect = Just PPEF_N,
            contractPerformance = Just PRF_PF,
            scheduleConfig =
              ScheduleConfig
                { calendar = Just CLDR_NC,
                  endOfMonthConvention = Just EOMC_EOM,
                  businessDayConvention = Just BDC_NULL
                },
            -- Penalties
            penaltyRate = Nothing,
            penaltyType = Just PYTP_O, -- no penalty
            -- Scaling
            interestScalingMultiplier = Nothing,
            scalingIndexAtStatusDate = Nothing,
            scalingEffect = Nothing,
            scalingIndexAtContractDealDate = Nothing,
            marketObjectCodeOfScalingIndex = Nothing,
            notionalScalingMultiplier = Nothing,
            cycleOfScalingIndex = Nothing,
            cycleAnchorDateOfScalingIndex = Nothing,
            -- Optionality
            cycleOfOptionality = Nothing,
            cycleAnchorDateOfOptionality = Nothing,
            optionType = Nothing,
            optionStrike1 = Nothing,
            optionExerciseType = Nothing,
            -- Settlement
            settlementPeriod = Nothing,
            deliverySettlement = Nothing,
            exerciseAmount = Nothing,
            futuresPrice = Nothing,
            -- Rate Reset
            cycleOfRateReset = Nothing,
            cycleAnchorDateOfRateReset = Nothing,
            nextResetRate = Nothing,
            rateSpread = Nothing,
            rateMultiplier = Nothing,
            marketObjectCodeOfRateReset = Nothing,
            periodFloor  = Nothing,
            periodCap  = Nothing,
            lifeCap  = Nothing,
            lifeFloor  = Nothing,
            -- Interest
            capitalizationEndDate = Nothing,
            cycleOfInterestPayment = Just $ Cycle 1 P_Y ShortStub False,
            cycleAnchorDateOfInterestPayment = iso8601ParseM "2020-01-01T00:00:00",
            nominalInterestRate = Just 0.02,
            accruedInterest = Just 0.0,
            cycleOfPrincipalRedemption = Nothing,
            cycleAnchorDateOfPrincipalRedemption = Nothing,
            interestCalculationBase = Just IPCB_NT,
            interestCalculationBaseA = Nothing,
            cycleOfInterestCalculationBase = Nothing,
            cycleAnchorDateOfInterestCalculationBase = Nothing,
            -- Fee
            cycleOfFee = Nothing,
            cycleAnchorDateOfFee = Nothing,
            feeAccrued = Nothing,
            feeBasis = Nothing,
            feeRate = Nothing,
            settlementCurrency = Nothing,
            -- Dividend
            cycleOfDividend = Nothing,
            cycleAnchorDateOfDividend = Nothing,
            nextDividendPaymentAmount = Nothing,
            enableSettlement = False,
            constraints = Nothing,
            collateralAmount = 0
          }
   in case genFsContract ct of
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

-- |example02 defines a contract of type LAM
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
example02 :: IO ()
example02 =
  let ct =
        ContractTermsPoly
          { contractId = "0",
            contractType = LAM,
            contractStructure = [],
            initialExchangeDate = iso8601ParseM "2020-01-01T00:00:00",
            statusDate = fromJust $ iso8601ParseM "2019-12-31T00:00:00",
            maturityDate = iso8601ParseM "2030-01-01T00:00:00",
            amortizationDate = Nothing,
            exerciseDate = Nothing,
            terminationDate = Nothing,
            nextPrincipalRedemptionPayment = Just 1000.0,
            purchaseDate = Nothing,
            contractRole = CR_RPA,
            premiumDiscountAtIED = Nothing,
            notionalPrincipal = Just 10000.0,
            priceAtPurchaseDate = Nothing,
            priceAtTerminationDate = Nothing,
            dayCountConvention = Just DCC_E30_360,
            prepaymentEffect = Just PPEF_N,
            contractPerformance = Just PRF_PF,
            scheduleConfig =
              ScheduleConfig
                { calendar = Just CLDR_NC,
                  endOfMonthConvention = Just EOMC_EOM,
                  businessDayConvention = Just BDC_NULL
                },
            -- Penalties
            penaltyRate = Nothing,
            penaltyType = Just PYTP_O, -- no penalty
            -- Optionality
            cycleOfOptionality = Nothing,
            cycleAnchorDateOfOptionality = Nothing,
            optionType = Nothing,
            optionStrike1 = Nothing,
            optionExerciseType = Nothing,
            -- Settlement
            settlementPeriod = Nothing,
            deliverySettlement = Nothing,
            exerciseAmount = Nothing,
            futuresPrice = Nothing,
            -- Scaling
            interestScalingMultiplier = Nothing,
            scalingIndexAtStatusDate = Nothing,
            scalingEffect = Nothing,
            scalingIndexAtContractDealDate = Nothing,
            marketObjectCodeOfScalingIndex = Nothing,
            notionalScalingMultiplier = Nothing,
            cycleOfScalingIndex = Nothing,
            cycleAnchorDateOfScalingIndex = Nothing,
            -- Rate Reset
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
            -- Interest
            capitalizationEndDate = Nothing,
            cycleOfInterestPayment = Just $ Cycle 1 P_Y ShortStub False,
            cycleAnchorDateOfInterestPayment = iso8601ParseM "2020-01-01T00:00:00",
            nominalInterestRate = Just 0.02,
            accruedInterest = Just 0.0,
            cycleOfPrincipalRedemption = Just $ Cycle 1 P_Y ShortStub False,
            cycleAnchorDateOfPrincipalRedemption = iso8601ParseM "2021-01-01T00:00:00",
            interestCalculationBase = Just IPCB_NT,
            interestCalculationBaseA = Nothing,
            cycleOfInterestCalculationBase = Nothing,
            cycleAnchorDateOfInterestCalculationBase = Nothing,
            -- Fee
            cycleOfFee = Nothing,
            cycleAnchorDateOfFee = Nothing,
            feeAccrued = Nothing,
            feeBasis = Nothing,
            feeRate = Nothing,
            settlementCurrency = Nothing,
            -- Dividend
            cycleOfDividend = Nothing,
            cycleAnchorDateOfDividend = Nothing,
            nextDividendPaymentAmount = Nothing,
            enableSettlement = False,
            constraints = Nothing,
            collateralAmount = 0
          }
   in case genFsContract ct of
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

-- |example03 defines a contract of type NAM
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
example03 :: IO ()
example03 =
  let ct =
        ContractTermsPoly
          { contractId = "0",
            contractType = NAM,
            contractStructure = [],
            initialExchangeDate = iso8601ParseM "2020-01-01T00:00:00",
            statusDate = fromJust $ iso8601ParseM "2019-12-31T00:00:00",
            maturityDate = iso8601ParseM "2030-01-01T00:00:00",
            amortizationDate = Nothing,
            exerciseDate = Nothing,
            terminationDate = Nothing,
            nextPrincipalRedemptionPayment = Just 1000.0,
            purchaseDate = Nothing,
            contractRole = CR_RPA,
            premiumDiscountAtIED = Nothing,
            notionalPrincipal = Just 10000.0,
            priceAtPurchaseDate = Nothing,
            priceAtTerminationDate = Nothing,
            dayCountConvention = Just DCC_E30_360,
            prepaymentEffect = Just PPEF_N,
            contractPerformance = Just PRF_PF,
            scheduleConfig =
              ScheduleConfig
                { calendar = Just CLDR_NC,
                  endOfMonthConvention = Just EOMC_EOM,
                  businessDayConvention = Just BDC_NULL
                },
            -- Penalties
            penaltyRate = Nothing,
            penaltyType = Just PYTP_O, -- no penalty
            -- Optionality
            cycleOfOptionality = Nothing,
            cycleAnchorDateOfOptionality = Nothing,
            optionType = Nothing,
            optionStrike1 = Nothing,
            optionExerciseType = Nothing,
            -- Settlement
            settlementPeriod = Nothing,
            deliverySettlement = Nothing,
            exerciseAmount = Nothing,
            futuresPrice = Nothing,
            -- Scaling
            interestScalingMultiplier = Nothing,
            scalingIndexAtStatusDate = Nothing,
            scalingEffect = Nothing,
            scalingIndexAtContractDealDate = Nothing,
            marketObjectCodeOfScalingIndex = Nothing,
            notionalScalingMultiplier = Nothing,
            cycleOfScalingIndex = Nothing,
            cycleAnchorDateOfScalingIndex = Nothing,
            -- Rate Reset
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
            -- Interest
            capitalizationEndDate = Nothing,
            cycleOfInterestPayment = Just $ Cycle 1 P_Y ShortStub False,
            cycleAnchorDateOfInterestPayment = iso8601ParseM "2020-01-01T00:00:00",
            nominalInterestRate = Just 0.02,
            accruedInterest = Just 0.0,
            cycleOfPrincipalRedemption = Just $ Cycle 1 P_Y ShortStub False,
            cycleAnchorDateOfPrincipalRedemption = iso8601ParseM "2021-01-01T00:00:00",
            interestCalculationBase = Just IPCB_NT,
            interestCalculationBaseA = Just 1000,
            cycleOfInterestCalculationBase = Just $ Cycle 1 P_Y ShortStub False,
            cycleAnchorDateOfInterestCalculationBase = iso8601ParseM "2021-01-01T00:00:00",
            -- Fee
            cycleOfFee = Nothing,
            cycleAnchorDateOfFee = Nothing,
            feeAccrued = Nothing,
            feeBasis = Nothing,
            feeRate = Nothing,
            settlementCurrency = Nothing,
            -- Dividend
            cycleOfDividend = Nothing,
            cycleAnchorDateOfDividend = Nothing,
            nextDividendPaymentAmount = Nothing,
            enableSettlement = False,
            constraints = Nothing,
            collateralAmount = 0
          }
   in case genFsContract ct of
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

-- |example04 defines a contract of type ANN
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
example04 :: IO ()
example04 =
  let ct =
        ContractTermsPoly
          { contractId = "0",
            contractType = ANN,
            contractStructure = [],
            initialExchangeDate = iso8601ParseM "2020-01-01T00:00:00",
            statusDate = fromJust $ iso8601ParseM "2019-12-31T00:00:00",
            maturityDate = iso8601ParseM "2030-01-01T00:00:00",
            amortizationDate = Nothing,
            exerciseDate = Nothing,
            terminationDate = Nothing,
            nextPrincipalRedemptionPayment = Just 1000,
            purchaseDate = Nothing,
            contractRole = CR_RPA,
            premiumDiscountAtIED = Nothing,
            notionalPrincipal = Just 10000.0,
            priceAtPurchaseDate = Nothing,
            priceAtTerminationDate = Nothing,
            dayCountConvention = Just DCC_E30_360,
            prepaymentEffect = Just PPEF_N,
            contractPerformance = Just PRF_PF,
            scheduleConfig =
              ScheduleConfig
                { calendar = Just CLDR_NC,
                  endOfMonthConvention = Just EOMC_EOM,
                  businessDayConvention = Just BDC_NULL
                },
            -- Penalties
            penaltyRate = Nothing,
            penaltyType = Just PYTP_O, -- no penalty
            -- Optionality
            cycleOfOptionality = Nothing,
            cycleAnchorDateOfOptionality = Nothing,
            optionType = Nothing,
            optionStrike1 = Nothing,
            optionExerciseType = Nothing,
            -- Settlement
            settlementPeriod = Nothing,
            deliverySettlement = Nothing,
            exerciseAmount = Nothing,
            futuresPrice = Nothing,
            -- Scaling
            interestScalingMultiplier = Nothing,
            scalingIndexAtStatusDate = Nothing,
            scalingEffect = Nothing,
            scalingIndexAtContractDealDate = Nothing,
            marketObjectCodeOfScalingIndex = Nothing,
            notionalScalingMultiplier = Nothing,
            cycleOfScalingIndex = Nothing,
            cycleAnchorDateOfScalingIndex = Nothing,
            -- Rate Reset
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
            -- Interest
            capitalizationEndDate = Nothing,
            cycleOfInterestPayment = Just $ Cycle 1 P_Y ShortStub False,
            cycleAnchorDateOfInterestPayment = iso8601ParseM "2020-01-01T00:00:00",
            nominalInterestRate = Just 0.02,
            accruedInterest = Just 0.0,
            cycleOfPrincipalRedemption = Just $ Cycle 1 P_Y ShortStub False,
            cycleAnchorDateOfPrincipalRedemption = iso8601ParseM "2021-01-01T00:00:00",
            interestCalculationBase = Just IPCB_NT,
            interestCalculationBaseA = Nothing,
            cycleOfInterestCalculationBase = Nothing,
            cycleAnchorDateOfInterestCalculationBase = Nothing,
            -- Fee
            cycleOfFee = Nothing,
            cycleAnchorDateOfFee = Nothing,
            feeAccrued = Nothing,
            feeBasis = Nothing,
            feeRate = Nothing,
            settlementCurrency = Nothing,
            -- Dividend
            cycleOfDividend = Nothing,
            cycleAnchorDateOfDividend = Nothing,
            nextDividendPaymentAmount = Nothing,
            enableSettlement = False,
            constraints = Nothing,
            collateralAmount = 0
          }
   in case genFsContract ct of
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

-- |totalPayments calculates the sum of the payments provided as argument
totalPayments :: Payee -> [Payment] -> Integer
totalPayments payee = sum . map m . filter f
  where
    m (Payment _ _ mon) = Val.valueOf mon "" ""
    f (Payment _ pay _) = pay == payee
