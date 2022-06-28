{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Spec.Actus.Haskell where

import Actus.Domain.BusinessEvents (RiskFactors)
import Actus.Domain.ContractState (ContractState)
import Actus.Domain.ContractTerms
import Actus.Domain.Ops (ActusNum (..), ActusOps (..), RoleSignOps (..), ScheduleOps (..), YearFractionOps (..))
import Actus.Domain.Schedule (CashFlow)
import Actus.Utility.YearFraction (yearFraction)
import Control.Applicative ((<|>))
import Control.Monad (guard, mzero)
import Data.Aeson.Types as Aeson (FromJSON, Options (..), SumEncoding (..), Value (Object), defaultOptions,
                                  genericParseJSON, parseJSON, (.:), (.:?))
import Data.Maybe (fromMaybe)

type TestContractState = ContractState Double
type TestRiskFactors = RiskFactors Double
type TestCashFlow = CashFlow Double
type TestContractTerms = ContractTerms Double

setDefaultContractTermValues :: TestContractTerms -> TestContractTerms
setDefaultContractTermValues ct@ContractTerms {..} =
  ct
    { scheduleConfig =
        scheduleConfig
          { endOfMonthConvention = applyDefault EOMC_SD (endOfMonthConvention scheduleConfig),
            businessDayConvention = applyDefault BDC_NULL (businessDayConvention scheduleConfig),
            calendar = applyDefault CLDR_NC (calendar scheduleConfig)
          },
      contractPerformance            = applyDefault PRF_PF contractPerformance,
      interestCalculationBase        = applyDefault IPCB_NT interestCalculationBase,
      premiumDiscountAtIED           = applyDefault 0.0 premiumDiscountAtIED,
      scalingEffect                  = applyDefault SE_OOO scalingEffect,
      penaltyRate                    = applyDefault 0.0 penaltyRate,
      penaltyType                    = applyDefault PYTP_O penaltyType,
      prepaymentEffect               = applyDefault PPEF_N prepaymentEffect,
      rateSpread                     = applyDefault 0.0 rateSpread,
      rateMultiplier                 = applyDefault 1.0 rateMultiplier,
      feeAccrued                     = applyDefault 0.0 feeAccrued,
      feeRate                        = applyDefault 0.0 feeRate,
      accruedInterest                = applyDefault 0.0 accruedInterest,
      nominalInterestRate            = applyDefault 0.0 nominalInterestRate,
      priceAtPurchaseDate            = applyDefault 0.0 priceAtPurchaseDate,
      priceAtTerminationDate         = applyDefault 0.0 priceAtTerminationDate,
      scalingIndexAtContractDealDate = applyDefault 0.0 scalingIndexAtContractDealDate,
      periodFloor                    = applyDefault (- infinity) periodFloor,
      periodCap                      = applyDefault infinity periodCap,
      lifeCap                        = applyDefault infinity lifeCap,
      lifeFloor                      = applyDefault (- infinity) lifeFloor,
      interestCalculationBaseA       = applyDefault 0.0 interestCalculationBaseA,

      -- see ContractModel.java
      cycleAnchorDateOfInterestPayment = cycleAnchorDateOfInterestPayment <|> ((guard $ contractType == CLM) >> initialExchangeDate)
    }
  where
    infinity :: Double
    infinity = 1 Prelude./ 0 :: Double

    applyDefault :: a -> Maybe a -> Maybe a
    applyDefault v o = o <|> Just v

instance FromJSON (Reference Double) where
  parseJSON = genericParseJSON defaultOptions { sumEncoding = UntaggedValue }

instance FromJSON (ContractStructure Double) where
  parseJSON (Object v) =
    ContractStructure
      <$> v .: "object"
      <*> v .: "referenceType"
      <*> v .: "referenceRole"
  parseJSON _ = mzero

instance FromJSON TestContractTerms where
  parseJSON (Object v) =
    ContractTerms
      <$> (v .:  "contractID" <|> v .: "contractId")
      <*> v .:  "contractType"
      <*> (v .: "contractStructure" <|> return [])
      <*> v .:  "contractRole"
      <*> v .:? "settlementCurrency"
      <*> v .:? "initialExchangeDate"
      <*> v .:? "dayCountConvention"
      <*> (v .: "scheduleConfig"
           <|> ScheduleConfig
               <$> v .:? "calendar"
               <*> v .:? "endOfMonthConvention"
               <*> v .:? "businessDayConvention"
           )
      <*> v .:  "statusDate"
      <*> v .:? "contractPerformance"
      <*> v .:? "creditEventTypeCovered"
      <*> v .!? "coverageOfCreditEnhancement"
      <*> v .!? "guaranteedExposure"
      <*> v .:? "cycleOfFee"
      <*> v .:? "cycleAnchorDateOfFee"
      <*> v .:? "feeAccrued"
      <*> v .:? "feeBasis"
      <*> v .!? "feeRate"
      <*> v .:? "cycleAnchorDateOfInterestPayment"
      <*> v .:? "cycleOfInterestPayment"
      <*> v .!? "accruedInterest"
      <*> v .:? "capitalizationEndDate"
      <*> v .:? "cycleAnchorDateOfInterestCalculationBase"
      <*> v .:? "cycleOfInterestCalculationBase"
      <*> v .:? "interestCalculationBase"
      <*> v .!? "interestCalculationBaseAmount"
      <*> v .!? "nominalInterestRate"
      <*> v .!? "nominalInterestRate2"
      <*> v .!? "interestScalingMultiplier"
      <*> v .:? "maturityDate"
      <*> v .:? "amortizationDate"
      <*> v .:? "exerciseDate"
      <*> v .!? "notionalPrincipal"
      <*> v .!? "premiumDiscountAtIED"
      <*> v .:? "cycleAnchorDateOfPrincipalRedemption"
      <*> v .:? "cycleOfPrincipalRedemption"
      <*> v .!? "nextPrincipalRedemptionPayment"
      <*> v .:? "purchaseDate"
      <*> v .!? "priceAtPurchaseDate"
      <*> v .:? "terminationDate"
      <*> v .!? "priceAtTerminationDate"
      <*> v .!? "quantity"
      <*> v .:? "scalingIndexAtStatusDate"
      <*> v .:? "cycleAnchorDateOfScalingIndex"
      <*> v .:? "cycleOfScalingIndex"
      <*> v .:? "scalingEffect"
      <*> v .!? "scalingIndexAtContractDealDate"
      <*> v .:? "marketObjectCodeOfScalingIndex"
      <*> v .!? "notionalScalingMultiplier"
      <*> v .:? "cycleOfOptionality"
      <*> v .:? "cycleAnchorDateOfOptionality"
      <*> v .:? "optionType"
      <*> v .!? "optionStrike1"
      <*> v .:? "optionExerciseType"
      <*> v .:? "settlementPeriod"
      <*> v .:? "deliverySettlement"
      <*> v .!? "exerciseAmount"
      <*> v .!? "futuresPrice"
      <*> v .:? "penaltyRate"
      <*> v .:? "penaltyType"
      <*> v .:? "prepaymentEffect"
      <*> v .:? "cycleOfRateReset"
      <*> v .:? "cycleAnchorDateOfRateReset"
      <*> v .!? "nextResetRate"
      <*> v .!? "rateSpread"
      <*> v .!? "rateMultiplier"
      <*> v .:? "periodFloor"
      <*> v .:? "periodCap"
      <*> v .:? "lifeCap"
      <*> v .:? "lifeFloor"
      <*> v .:? "marketObjectCodeOfRateReset"
      <*> v .:? "cycleOfDividendPayment"
      <*> v .:? "cycleAnchorDateOfDividendPayment"
      <*> v .:? "nextDividendPaymentAmount"
      <*> (fromMaybe False <$> (v .:? "enableSettlement"))
      <*> v .:? "constraints"
    where
      (.!?) w s = w .:? s <|> (fmap read <$> w .:? s)
  parseJSON _ = mzero

instance RoleSignOps Double

instance ActusOps Double where
    _min  = min
    _max  = max
    _abs  = abs
    _zero = 0.0
    _one  = 1.0
    _fromInteger = fromInteger
    _negate = negate

instance ActusNum Double where
    a + b       = a Prelude.+ b
    a - b       = a Prelude.- b
    a * b       = a Prelude.* b
    a / b       = a Prelude./ b

instance YearFractionOps Double where
    _y = yearFraction

instance ScheduleOps Double where
    _ceiling = ceiling
