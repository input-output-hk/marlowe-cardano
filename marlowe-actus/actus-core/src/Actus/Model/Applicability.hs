{-# LANGUAGE RecordWildCards #-}

{-|
= ACTUS applicability rules
[Applicability rules](https://www.actusfrf.org/aplicability) are defined per contract type.
The rules check the consistency of the contract attributes for the given ACTUS contract terms.
If contract terms are successfully validated, we expect that the contract is meaningful and
cash flows can be projected.

There are different types of rules, like

  * an attribute is /mandatory/ \/ /optional/ \/ /not applicable/
  * an attribute is /mandatory/ if another attribute is /present/

-}
module Actus.Model.Applicability
  ( validateTerms
  ) where

import Actus.Domain (CT(..), ContractTerms(..), IPCB(..), ScheduleConfig(..), TermValidationError(..))
import Data.Maybe (isJust)
import Data.Validation (Validation(..))

-- |Contract terms are validated with applicability rules
validateTerms ::
  ContractTerms a                                       -- ^ Contract terms
  -> Validation [TermValidationError] (ContractTerms a) -- ^ Validated contract terms or validation errors
validateTerms ct@ContractTerms {contractType = PAM, ..} =
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

validateTerms ct@ContractTerms {contractType = LAM, ..} =
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

validateTerms ct@ContractTerms {contractType = NAM, ..} =
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

validateTerms ct@ContractTerms {contractType = ANN, ..} =
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

-- |Optional
_X :: Maybe c -> ContractTerms a -> b -> Validation [TermValidationError] (ContractTerms a)
_X _ ct _ = Success ct

-- |The conditional term with c=1 is optional when any of the unconditional terms with c=0 is defined.
_X_I_1 :: [Bool] -> [Bool] -> ContractTerms a -> [String] -> [String] -> Validation [TermValidationError] (ContractTerms a)
_X_I_1 uncond cond ct@ContractTerms {..} uncondNames condNames
  | or uncond = Success ct
  | or cond = Failure [Required $ "The unconditional terms " ++ show uncondNames ++ " must be defined when any of " ++ show condNames ++ " are defined for contract type '" ++ show contractType ++ "'"]
  | otherwise = Success ct

-- |If the unconditional term with c=0 in the group is defined, then at least one of the conditional terms with c=2 must be defined.
_X_I_2 :: Maybe b -> [Bool] -> ContractTerms a -> String -> [String] -> Validation [TermValidationError] (ContractTerms a)
_X_I_2 (Just _) cond ct _ _ | or cond = Success ct
_X_I_2 (Just _) _ ContractTerms {..} uncondName condNames = Failure [Required $ "At least one of the conditional terms in group " ++ show condNames ++ " must be defined when " ++ uncondName ++ " is defined for contract type '" ++ show contractType ++ "'"]
_X_I_2 Nothing _ ct _ _ = Success ct

-- |At least one of the CAs with c=4 in this group has to be defined provided that CA IPCB of the group takes the value NTL
_X_I_4 :: [Bool] -> ContractTerms a -> [String] -> Validation [TermValidationError] (ContractTerms a)
_X_I_4 cond ct@ContractTerms {interestCalculationBase = Just IPCB_NTL, ..} condNames =
  if or cond
    then Success ct
    else Failure [Required $ "At least one of the conditional terms in group " ++ show condNames ++ " must be defined when interest calculation base is NTL for contract type '" ++ show contractType ++ "'"]
_X_I_4 _ ct _ = Success ct

-- |Non-nullable / required
_NN :: Maybe b -> ContractTerms a -> String -> Validation [TermValidationError] (ContractTerms a)
_NN (Just _) ct _ = Success ct
_NN Nothing ContractTerms{..} n = Failure [Required $ "Contract term '" ++ n ++ "' is required for contract type '" ++ show contractType ++ "'"]

-- |Not applicable
_NA :: Maybe a -> ContractTerms a -> String -> Validation [TermValidationError] (ContractTerms a)
_NA (Just _) ContractTerms{..} n = Failure [NotApplicable $ "Contract term '" ++ n ++ "' is not applicable for contract type '" ++ show contractType ++ "'"]
_NA Nothing ct _ = Success ct

-- |NN(I, 1, _) (If one is defined, all must be defined)
_NN_I_1 :: [Bool] -> ContractTerms a -> [String] -> Validation [TermValidationError] (ContractTerms a)
_NN_I_1 _cts ct@ContractTerms{..} ns
  | and _cts = Success ct
  | or _cts = Failure [Required $ "All contract terms in group " ++ show ns ++ " should be defined if one of them is defined for contract type '" ++ show contractType ++ "'"]
  | otherwise = Success ct

-- |Not nullable if CA IPCB of the group takes the value NTIED
_NN_I_3 :: Maybe b -> ContractTerms a -> [Char] -> Validation [TermValidationError] (ContractTerms a)
_NN_I_3 Nothing ContractTerms {interestCalculationBase = Just IPCB_NTIED} n = Failure [Required $ "Contract term " ++ n ++ " must be defined when interest calculation base is NTIED"]
_NN_I_3 _ ct _ = Success ct
