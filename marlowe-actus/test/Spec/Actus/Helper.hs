{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}

-- | = ACTUS Helper module for tests
--
module Spec.Actus.Helper
  ( toMarlowe
  )
where

import Actus.Domain.ContractTerms (ContractStructure (..), ContractTerms (..), Reference (..))
import Actus.Marlowe (ContractTermsMarlowe)
import Generator
import Language.Marlowe (Observation, Value)
import Spec.Actus.Haskell

toMarlowe :: TestContractTerms -> ContractTermsMarlowe
toMarlowe ct =
  ContractTerms
    { contractId = contractId ct,
      contractType = contractType ct,
      contractStructure = map trans (contractStructure ct),
      contractRole = contractRole ct,
      settlementCurrency = settlementCurrency ct,
      initialExchangeDate = initialExchangeDate ct,
      dayCountConvention = dayCountConvention ct,
      scheduleConfig = scheduleConfig ct,
      statusDate = statusDate ct,
      contractPerformance = contractPerformance ct,
      creditEventTypeCovered = creditEventTypeCovered ct,
      coverageOfCreditEnhancement = constant <$> coverageOfCreditEnhancement ct,
      guaranteedExposure = guaranteedExposure ct,
      cycleOfFee = cycleOfFee ct,
      cycleAnchorDateOfFee = cycleAnchorDateOfFee ct,
      feeAccrued = constant <$> feeAccrued ct,
      feeBasis = feeBasis ct,
      feeRate = constant <$> feeRate ct,
      cycleAnchorDateOfInterestPayment = cycleAnchorDateOfInterestPayment ct,
      cycleOfInterestPayment = cycleOfInterestPayment ct,
      accruedInterest = constant <$> accruedInterest ct,
      capitalizationEndDate = capitalizationEndDate ct,
      cycleAnchorDateOfInterestCalculationBase = cycleAnchorDateOfInterestCalculationBase ct,
      cycleOfInterestCalculationBase = cycleOfInterestCalculationBase ct,
      interestCalculationBase = interestCalculationBase ct,
      interestCalculationBaseA = constant <$> interestCalculationBaseA ct,
      nominalInterestRate = constant <$> nominalInterestRate ct,
      nominalInterestRate2 = constant <$> nominalInterestRate2 ct,
      interestScalingMultiplier = constant <$> interestScalingMultiplier ct,
      notionalPrincipal = constant <$> notionalPrincipal ct,
      premiumDiscountAtIED = constant <$> premiumDiscountAtIED ct,
      maturityDate = maturityDate ct,
      amortizationDate = amortizationDate ct,
      exerciseDate = exerciseDate ct,
      cycleAnchorDateOfPrincipalRedemption = cycleAnchorDateOfPrincipalRedemption ct,
      cycleOfPrincipalRedemption = cycleOfPrincipalRedemption ct,
      nextPrincipalRedemptionPayment = constant <$> nextPrincipalRedemptionPayment ct,
      purchaseDate = purchaseDate ct,
      priceAtPurchaseDate = constant <$> priceAtPurchaseDate ct,
      terminationDate = terminationDate ct,
      priceAtTerminationDate = constant <$> priceAtTerminationDate ct,
      quantity = constant <$> quantity ct,
      scalingIndexAtStatusDate = constant <$> scalingIndexAtStatusDate ct,
      cycleAnchorDateOfScalingIndex = cycleAnchorDateOfScalingIndex ct,
      cycleOfScalingIndex = cycleOfScalingIndex ct,
      scalingEffect = scalingEffect ct,
      scalingIndexAtContractDealDate = constant <$> scalingIndexAtContractDealDate ct,
      marketObjectCodeOfScalingIndex = marketObjectCodeOfScalingIndex ct,
      notionalScalingMultiplier = constant <$> notionalScalingMultiplier ct,
      cycleOfOptionality = cycleOfOptionality ct,
      cycleAnchorDateOfOptionality = cycleAnchorDateOfOptionality ct,
      optionType = optionType ct,
      optionStrike1 = constant <$> optionStrike1 ct,
      optionExerciseType = optionExerciseType ct,
      settlementPeriod = settlementPeriod ct,
      deliverySettlement = deliverySettlement ct,
      exerciseAmount = constant <$> exerciseAmount ct,
      futuresPrice = constant <$> futuresPrice ct,
      penaltyRate = constant <$> penaltyRate ct,
      penaltyType = penaltyType ct,
      prepaymentEffect = prepaymentEffect ct,
      cycleOfRateReset = cycleOfRateReset ct,
      cycleAnchorDateOfRateReset = cycleAnchorDateOfRateReset ct,
      nextResetRate = constant <$> nextResetRate ct,
      rateSpread = constant <$> rateSpread ct,
      rateMultiplier = constant <$> rateMultiplier ct,
      periodFloor = constant <$> periodFloor ct,
      periodCap = constant <$> periodCap ct,
      lifeCap = constant <$> lifeCap ct,
      lifeFloor = constant <$> lifeFloor ct,
      marketObjectCodeOfRateReset = marketObjectCodeOfRateReset ct,
      cycleOfDividend = cycleOfDividend ct,
      cycleAnchorDateOfDividend = cycleAnchorDateOfDividend ct,
      nextDividendPaymentAmount = constant <$> nextDividendPaymentAmount ct,
      enableSettlement = enableSettlement ct,
      constraints = constraints ct
    }
  where
    trans :: ContractStructure Double -> ContractStructure (Value Observation)
    trans cs =
      cs
        { reference = case reference cs of
            ReferenceId r    -> ReferenceId r
            ReferenceTerms t -> ReferenceTerms $ toMarlowe t
        }
