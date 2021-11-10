{-# LANGUAGE RecordWildCards #-}

module Language.Marlowe.ACTUS.Model.APPL.Applicability where

import Data.Maybe (isJust)
import Data.Validation
import Language.Marlowe.ACTUS.Domain.ContractTerms (CT (..), ContractTerms, ContractTermsPoly (..), ScheduleConfig (..),
                                                    TermValidationError (..))
import Language.Marlowe.ACTUS.Model.APPL.ApplicabilityModel

validateTerms :: ContractTerms -> Validation [TermValidationError] ContractTerms
validateTerms ct@ContractTermsPoly {contractType = PAM, ..} =
  ct
    <$ _NN initialExchangeDate ct "initial exchange date"
    <* _NN dayCountConvention ct "day count convention"
    <* _X (calendar scheduleConfig) ct "calendar"
    <* _X (businessDayConvention scheduleConfig) ct "business day convention"
    <* _X (endOfMonthConvention scheduleConfig) ct "end of month convention"
    <* _X_I_2 feeRate [isJust cycleAnchorDateOfFee, isJust cycleOfFee] ct "fee rate" ["cycle anchor date of fee", "cycle of fee"]
    <* _X feeAccrued ct "fee accrued"
    <* _NN_I_1 [isJust feeBasis, isJust feeRate] ct ["fee basis", "fee rate"]
    <* _NN nominalInterestRate ct "nominal interest rate"
    <* _X accruedInterest ct "accrued interest"
    <* _X capitalizationEndDate ct "capitalization end date"
    <* _X premiumDiscountAtIED ct "premium discount at IED"
    <* _X_I_2 scalingEffect [isJust cycleAnchorDateOfScalingIndex, isJust cycleOfScalingIndex] ct "scaling effect" ["cycle anchor date of scaling index", "cycle of scaling index"]
    <* _X_I_1
      [isJust cycleOfRateReset, isJust cycleAnchorDateOfRateReset]
      [isJust nextResetRate, isJust rateSpread, isJust rateMultiplier, isJust periodFloor, isJust periodCap, isJust lifeCap, isJust lifeFloor]
      ct
      ["cycle anchor date of rate reset", "cycle of rate reset"]
      ["next reset rate", "rate spread", "rate multiplier", "period floor", "period cap", "life cap", "life floor"]
    <* _NN notionalPrincipal ct "notional principal"
    <* _NN_I_1 [isJust purchaseDate, isJust priceAtPurchaseDate] ct ["purchase date", "price at purchase date"]
    <* _NN_I_1 [isJust terminationDate, isJust priceAtTerminationDate] ct ["termination date", "price at termination"]
    <* _NN maturityDate ct "maturity date"
    <* _NN_I_1 [isJust scalingEffect, isJust scalingIndexAtStatusDate, isJust scalingIndexAtContractDealDate] ct ["scaling effect", "scaling index at status date", "scaling index at contract deal date"]
    <* _X_I_1 [isJust penaltyRate, isJust penaltyType] [isJust prepaymentEffect] ct ["penalty rate", "penalty type"] ["prepayment effect"]

validateTerms ct@ContractTermsPoly {contractType = LAM, ..} =
  ct
    <$ _NN initialExchangeDate ct "initial exchange date"
    <* _NN dayCountConvention ct "day count convention"
    <* _X (calendar scheduleConfig) ct "calendar"
    <* _X (businessDayConvention scheduleConfig) ct "business day convention"
    <* _X (endOfMonthConvention scheduleConfig) ct "end of month convention"
    <* _X_I_4 [isJust cycleOfInterestCalculationBase, isJust cycleAnchorDateOfInterestCalculationBase] ct ["cycle of interest calculation base", "cycle anchor date of interest calculation base"]
    <* _X feeAccrued ct "fee accrued"
    <* _NN_I_1 [isJust feeBasis, isJust feeRate] ct ["fee basis", "fee rate"]
    <* _NN nominalInterestRate ct "nominal interest rate"
    <* _X accruedInterest ct "accrued interest"
    <* _X capitalizationEndDate ct "capitalization end date"
    <* _X premiumDiscountAtIED ct "premium discount at IED"
    <* _NN_I_3 interestCalculationBaseA ct "interest calculation base amount"
    <* _X_I_1
      [isJust cycleOfRateReset, isJust cycleAnchorDateOfRateReset]
      [isJust nextResetRate, isJust rateSpread, isJust rateMultiplier, isJust periodFloor, isJust periodCap, isJust lifeCap, isJust lifeFloor]
      ct
      ["cycle anchor date of rate reset", "cycle of rate reset"]
      ["next reset rate", "rate spread", "rate multiplier", "period floor", "period cap", "life cap", "life floor"]
    <* _NN notionalPrincipal ct "notional principal"
    <* _NN_I_1 [isJust purchaseDate, isJust priceAtPurchaseDate] ct ["purchase date", "price at purchase date"]
    <* _NN_I_1 [isJust terminationDate, isJust priceAtTerminationDate] ct ["termination date", "price at termination"]
    <* _NN cycleOfPrincipalRedemption ct "principal redemption cycle"
    <* _X maturityDate ct "maturity date"
    <* _X nextPrincipalRedemptionPayment ct "periodic payment amount"
    <* _NN_I_1 [isJust scalingEffect, isJust scalingIndexAtStatusDate, isJust scalingIndexAtContractDealDate] ct ["scaling effect", "scaling index at status date", "scaling index at contract deal date"]
    <* _X_I_1 [isJust penaltyRate, isJust penaltyType] [isJust prepaymentEffect] ct ["penalty rate", "penalty type"] ["prepayment effect"]

validateTerms ct@ContractTermsPoly {contractType = NAM, ..} =
  ct
    <$ _NN initialExchangeDate ct "initial exchange date"
    <* _NN dayCountConvention ct "day count convention"
    <* _X (calendar scheduleConfig) ct "calendar"
    <* _X (businessDayConvention scheduleConfig) ct "business day convention"
    <* _X (endOfMonthConvention scheduleConfig) ct "end of month convention"
    <* _X_I_4 [isJust cycleOfInterestCalculationBase, isJust cycleAnchorDateOfInterestCalculationBase] ct ["cycle of interest calculation base", "cycle anchor date of interest calculation base"]
    <* _X feeAccrued ct "fee accrued"
    <* _NN_I_1 [isJust feeBasis, isJust feeRate] ct ["fee basis", "fee rate"]
    <* _NN nominalInterestRate ct "nominal interest rate"
    <* _X accruedInterest ct "accrued interest"
    <* _X capitalizationEndDate ct "capitalization end date"
    <* _X premiumDiscountAtIED ct "premium discount at IED"
    <* _X cycleOfInterestPayment ct "cycle of interest payment"
    <* _X cycleAnchorDateOfInterestPayment ct "cycle anchor date of interest payment"
    <* _NN_I_3 interestCalculationBaseA ct "interest calculation base amount"
    <* _X_I_1
      [isJust cycleOfRateReset, isJust cycleAnchorDateOfRateReset]
      [isJust nextResetRate, isJust rateSpread, isJust rateMultiplier, isJust periodFloor, isJust periodCap, isJust lifeCap, isJust lifeFloor]
      ct
      ["cycle anchor date of rate reset", "cycle of rate reset"]
      ["next reset rate", "rate spread", "rate multiplier", "period floor", "period cap", "life cap", "life floor"]
    <* _NN notionalPrincipal ct "notional principal"
    <* _NN_I_1 [isJust purchaseDate, isJust priceAtPurchaseDate] ct ["purchase date", "price at purchase date"]
    <* _NN_I_1 [isJust terminationDate, isJust priceAtTerminationDate] ct ["termination date", "price at termination"]
    <* _NN cycleOfPrincipalRedemption ct "principal redemption cycle"
    <* _X maturityDate ct "maturity date"
    <* _NN nextPrincipalRedemptionPayment ct "periodic payment amount"
    <* _NN_I_1 [isJust scalingEffect, isJust scalingIndexAtStatusDate, isJust scalingIndexAtContractDealDate] ct ["scaling effect", "scaling index at status date", "scaling index at contract deal date"]
    <* _X_I_1 [isJust penaltyRate, isJust penaltyType] [isJust prepaymentEffect] ct ["penalty rate", "penalty type"] ["prepayment effect"]

validateTerms ct@ContractTermsPoly {contractType = ANN, ..} =
  ct
    <$ _NN initialExchangeDate ct "initial exchange date"
    <* _NN dayCountConvention ct "day count convention"
    <* _X (calendar scheduleConfig) ct "calendar"
    <* _X (businessDayConvention scheduleConfig) ct "business day convention"
    <* _X (endOfMonthConvention scheduleConfig) ct "end of month convention"
    <* _X_I_4 [isJust cycleOfInterestCalculationBase, isJust cycleAnchorDateOfInterestCalculationBase] ct ["cycle of interest calculation base", "cycle anchor date of interest calculation base"]
    <* _X feeAccrued ct "fee accrued"
    <* _NN_I_1 [isJust feeBasis, isJust feeRate] ct ["fee basis", "fee rate"]
    <* _NN nominalInterestRate ct "nominal interest rate"
    <* _X accruedInterest ct "accrued interest"
    <* _X capitalizationEndDate ct "capitalization end date"
    <* _X premiumDiscountAtIED ct "premium discount at IED"
    <* _X cycleOfInterestPayment ct "cycle of interest payment"
    <* _X cycleAnchorDateOfInterestPayment ct "cycle anchor date of interest payment"
    <* _NN_I_3 interestCalculationBaseA ct "interest calculation base amount"
    <* _X_I_1
      [isJust cycleOfRateReset, isJust cycleAnchorDateOfRateReset]
      [isJust nextResetRate, isJust rateSpread, isJust rateMultiplier, isJust periodFloor, isJust periodCap, isJust lifeCap, isJust lifeFloor]
      ct
      ["cycle anchor date of rate reset", "cycle of rate reset"]
      ["next reset rate", "rate spread", "rate multiplier", "period floor", "period cap", "life cap", "life floor"]
    <* _NN notionalPrincipal ct "notional principal"
    <* _NN_I_1 [isJust purchaseDate, isJust priceAtPurchaseDate] ct ["purchase date", "price at purchase date"]
    <* _NN_I_1 [isJust terminationDate, isJust priceAtTerminationDate] ct ["termination date", "price at termination"]
    <* _NN cycleOfPrincipalRedemption ct "principal redemption cycle"
    <* _X maturityDate ct "maturity date"
    <* _NN nextPrincipalRedemptionPayment ct "periodic payment amount"
    <* _NN_I_1 [isJust scalingEffect, isJust scalingIndexAtStatusDate, isJust scalingIndexAtContractDealDate] ct ["scaling effect", "scaling index at status date", "scaling index at contract deal date"]
    <* _X_I_1 [isJust penaltyRate, isJust penaltyType] [isJust prepaymentEffect] ct ["penalty rate", "penalty type"] ["prepayment effect"]

validateTerms t = Success t -- TODO: Generate applicability rules from the JSON specification (SCP-2882)
