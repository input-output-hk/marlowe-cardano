{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}

module Spec.Marlowe.ACTUS.QCGenerator where

import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Clock.System (SystemTime (MkSystemTime), utcToSystemTime)
import Language.Marlowe.ACTUS.Domain.BusinessEvents
import Language.Marlowe.ACTUS.Domain.ContractTerms
import Language.Marlowe.ACTUS.Domain.Schedule
import Language.Marlowe.ACTUS.Generator.Analysis
import Test.QuickCheck

largeamount :: Gen Double
largeamount = choose (0.0, 10000000.0)

smallamount :: Gen Double
smallamount = choose (0.0, 1000.0)

percentage :: Gen Double
percentage = choose (0.0, 100.0)

scalingFactor :: Gen Double
scalingFactor = choose (0.0, 100.0)

mightbe :: Gen a -> Gen (Maybe a)
mightbe original = oneof [ Just <$> original, return Nothing ]

zeroOr :: Gen Double -> Gen Double
zeroOr original = oneof [return 0.0, original]

oneOr :: Gen Double -> Gen Double
oneOr original = oneof [return 1.0, original]

anchor :: Integer
anchor = 1577836800 -- 2020-01-01T00:00:00

maxDate :: Integer
maxDate = 1893456000 -- 2030-01-01T00:00:00

secondsPerYear :: Integer
secondsPerYear = 31557600

date :: Gen LocalTime
date = epochToLocalTime <$> choose (anchor, maxDate)

dateBefore :: LocalTime -> Gen LocalTime
dateBefore d = epochToLocalTime <$> choose (anchor, localTime d)

localTime :: LocalTime -> Integer
localTime LocalTime {..} =
  let (MkSystemTime secs _) = utcToSystemTime (UTCTime localDay (timeOfDayToTime localTimeOfDay))
   in fromIntegral secs

epochToLocalTime :: Integer -> LocalTime
epochToLocalTime = utcToLocalTime utc . posixSecondsToUTCTime . fromIntegral

cyclePeriodFreq :: Gen Period
cyclePeriodFreq =
  frequency
    [ (1, return P_D),
      (10, return P_W),
      (20, return P_M),
      (5, return P_Q),
      (5, return P_H),
      (70, return P_Y)
    ]

datecycle :: Gen Cycle
datecycle = Cycle <$> sized (\n -> choose (1, max 1 (maxDate `div` (toInteger n+1 * secondsPerYear)))) <*> cyclePeriodFreq <*> elements [ShortStub, LongStub] <*> elements [True, False]

contractTermsGen :: Gen ContractTerms
contractTermsGen = elements [PAM, LAM, NAM, ANN] >>= contractTermsGen'

contractTermsGen' :: CT -> Gen ContractTerms
contractTermsGen' ct = do
  -- initial exchange date is fixed
  let ied = epochToLocalTime anchor

  -- set the status date before initial exchange date
  let sd = LocalTime (addDays (-2) $ localDay ied) (localTimeOfDay ied)

  interest <- percentage
  notional <- largeamount
  pdied <- zeroOr smallamount

  feeBasis <- elements [FEB_A, FEB_N]
  feeRate <- zeroOr percentage

  penaltytype <- elements [PYTP_A, PYTP_N, PYTP_I, PYTP_O]
  penaltyrate <- zeroOr percentage

  eomc <- elements [EOMC_EOM, EOMC_SD]
  bdc <- elements [BDC_NULL, BDC_SCF, BDC_SCMF, BDC_CSF, BDC_CSMF, BDC_SCP, BDC_SCMP, BDC_CSP, BDC_CSMP]
  calendar <- elements [CLDR_NC] -- TODO: add CLDR_MF
  dcc <- elements [DCC_A_AISDA, DCC_A_360, DCC_A_365, DCC_E30_360ISDA, DCC_E30_360] -- TODO: DCC_B_252 is not implemented
  ppef <- elements [PPEF_N, PPEF_A, PPEF_M]
  contractRole <- elements [CR_BUY, CR_SEL]

  scef <- elements [SE_OOO, SE_ONO, SE_OOM, SE_ONM, SE_IOO, SE_INO, SE_IOM, SE_INM]
  sccdd <- oneOr scalingFactor
  scied <- oneOr scalingFactor
  scip <- oneOr scalingFactor
  scnt <- oneOr scalingFactor

  rrsp <- zeroOr percentage
  rrmlt <- oneOr scalingFactor
  rrpf <- zeroOr percentage
  rrpc <- oneOr percentage
  rrlc <- oneOr percentage
  rrlf <- zeroOr percentage

  -- always set a maturity date
  maturityDate <- date
  amortizationDate <- case ct of
    ANN -> mightbe date
    _   -> return Nothing
  terminationDate <- mightbe date
  exerciseDate <- mightbe date

  let upperBound = minimum $ catMaybes [Just maturityDate, amortizationDate, terminationDate]

  nextPrincipalRedemption <- case ct of
    PAM -> return Nothing
    _   -> Just <$> largeamount
  purchaseDate <- mightbe $ dateBefore upperBound
  priceAtTerminationDate <- smallamount
  priceAtPurchaseDate <- smallamount

  optionalityCycle <- mightbe datecycle
  optionalityAnchor <- mightbe $ dateBefore upperBound

  scalingCycle <- mightbe datecycle
  scalingAnchor <- mightbe $ dateBefore upperBound

  rateResetCycle <- mightbe datecycle
  rateResetAnchor <- dateBefore upperBound
  nextRateReset <- percentage

  interestCapitalisationDate <- mightbe $ dateBefore upperBound
  interestPaymentCycle <- datecycle
  interestPaymentAnchor <- case ct of
    PAM -> Just <$> dateBefore upperBound
    _   -> mightbe $ dateBefore upperBound
  accruedInterest <- mightbe smallamount

  principalRedemptionCycle <- datecycle
  principalRedemptionAnchor <- mightbe $ dateBefore upperBound

  interestPaymentCalculationBase <- mightbe $ elements [IPCB_NT, IPCB_NTIED, IPCB_NTL]
  interestPaymentCalculationBaseAmount <- largeamount
  interestPaymentCalculationBaseCycle <- mightbe datecycle
  interestPaymentCalculationBaseAnchor <- mightbe $ dateBefore upperBound

  feeCycle <- mightbe datecycle
  feeAnchor <- mightbe $ dateBefore upperBound
  feeAccrued <- mightbe smallamount

  return
    ContractTermsPoly
      { contractId = "0",
        contractType = ct,
        contractStructure = [],
        initialExchangeDate = Just ied,
        statusDate = sd,
        maturityDate = Just maturityDate,
        amortizationDate = amortizationDate,
        exerciseDate = exerciseDate,
        terminationDate = terminationDate,
        nextPrincipalRedemptionPayment = nextPrincipalRedemption,
        purchaseDate = purchaseDate,
        contractRole = contractRole,
        premiumDiscountAtIED = Just pdied,
        notionalPrincipal = Just notional,
        priceAtPurchaseDate = priceAtPurchaseDate <$ purchaseDate,
        priceAtTerminationDate = priceAtTerminationDate <$ terminationDate,
        dayCountConvention = Just dcc,
        prepaymentEffect = Just ppef,
        contractPerformance = Just PRF_PF,
        scheduleConfig =
          ScheduleConfig
            { calendar = Just calendar,
              endOfMonthConvention = Just eomc,
              businessDayConvention = Just bdc
            },
        -- Penalties
        penaltyRate = Just penaltyrate,
        penaltyType = Just penaltytype,
        -- Optionality
        cycleOfOptionality = optionalityCycle,
        cycleAnchorDateOfOptionality = optionalityAnchor,
        optionType = Nothing,
        optionStrike1 = Nothing,
        optionExerciseType = Nothing,
        -- Settlement
        settlementPeriod = Nothing,
        deliverySettlement = Nothing,
        exerciseAmount = Nothing,
        futuresPrice = Nothing,
        -- Scaling:
        scalingIndexAtStatusDate = Just scied,
        scalingEffect = Just scef,
        cycleOfScalingIndex = scalingCycle,
        cycleAnchorDateOfScalingIndex = scalingAnchor,
        scalingIndexAtContractDealDate = Just sccdd,
        interestScalingMultiplier = Just scip,
        notionalScalingMultiplier = Just scnt,
        -- Rate Reset
        cycleOfRateReset = rateResetCycle,
        cycleAnchorDateOfRateReset = Just rateResetAnchor,
        nextResetRate = Just nextRateReset,
        rateSpread = Just rrsp,
        rateMultiplier = Just rrmlt,
        periodFloor = Just rrpf,
        periodCap = Just rrpc,
        lifeCap = Just rrlc,
        lifeFloor = Just rrlf,
        -- Interest
        capitalizationEndDate = interestCapitalisationDate,
        cycleOfInterestPayment = Just interestPaymentCycle,
        cycleAnchorDateOfInterestPayment = interestPaymentAnchor,
        nominalInterestRate = Just interest,
        accruedInterest = accruedInterest,
        cycleOfPrincipalRedemption = case ct of
          PAM -> Nothing
          _   -> Just principalRedemptionCycle,
        cycleAnchorDateOfPrincipalRedemption = principalRedemptionAnchor,
        interestCalculationBase = interestPaymentCalculationBase,
        interestCalculationBaseA = case interestPaymentCalculationBase of
          Just IPCB_NTIED -> Just interestPaymentCalculationBaseAmount
          _               -> Nothing,
        cycleOfInterestCalculationBase = interestPaymentCalculationBaseCycle,
        cycleAnchorDateOfInterestCalculationBase = interestPaymentCalculationBaseAnchor,
        -- Fee
        cycleOfFee = feeCycle,
        cycleAnchorDateOfFee = feeAnchor,
        feeAccrued = feeAccrued,
        feeBasis = Just feeBasis,
        feeRate = Just feeRate,
        settlementCurrency = Nothing,
        marketObjectCodeOfScalingIndex = Nothing,
        marketObjectCodeOfRateReset = Nothing,
        cycleOfDividend = Nothing,
        cycleAnchorDateOfDividend = Nothing,
        nextDividendPaymentAmount = Nothing,
        -- enable settlement currency
        enableSettlement = False,
        constraints = Nothing,
        collateralAmount = 0
      }

riskAtTGen :: Gen RiskFactors
riskAtTGen = RiskFactorsPoly
    <$> percentage
    <*> percentage
    <*> percentage
    <*> smallamount
    <*> smallamount
    <*> smallamount

riskFactorsGen :: ContractTerms -> Gen (M.Map LocalTime RiskFactors)
riskFactorsGen ct = do
    let riskFactors _ _ =
         RiskFactorsPoly
            { o_rf_CURS = 1.0,
              o_rf_RRMO = 1.0,
              o_rf_SCMO = 1.0,
              pp_payoff = 0.0,
              xd_payoff = 0.0,
              dv_payoff = 0.0
            }
    let days = cashCalculationDay <$> genProjectedCashflows riskFactors ct
    rf <- vectorOf (L.length days) riskAtTGen
    return $ M.fromList $ L.zip days rf

riskFactorsGenRandomWalkGen :: ContractTerms -> Gen (M.Map LocalTime RiskFactors)
riskFactorsGenRandomWalkGen contractTerms = do
    rfs <- riskFactorsGen contractTerms
    riskAtT <- riskAtTGen
    let
        (riskFactorsDates, riskFactorsValues) = unzip $ M.toList rfs

        fluctuate state fluctiation = state + (fluctiation - 50) / 100
        walk rf st =
            let fluctuate' extractor = fluctuate (extractor rf) (extractor st)
            in RiskFactorsPoly
                (fluctuate' o_rf_CURS)
                (fluctuate' o_rf_RRMO)
                (fluctuate' o_rf_SCMO)
                (fluctuate' pp_payoff)
                (fluctuate' xd_payoff)
                (fluctuate' dv_payoff)
        path = L.scanl walk riskAtT riskFactorsValues
    return $ M.fromList $ L.zip riskFactorsDates path
