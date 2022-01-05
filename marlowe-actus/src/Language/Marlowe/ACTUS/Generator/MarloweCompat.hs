{-# LANGUAGE RecordWildCards #-}

{- This module provides compatibility to Marlowe DSL -}

module Language.Marlowe.ACTUS.Generator.MarloweCompat where

import Data.String (IsString (fromString))
import Data.Time (Day, LocalTime (..), UTCTime (UTCTime), timeOfDayToTime)
import Data.Time.Clock.System (SystemTime (MkSystemTime), utcToSystemTime)
import Language.Marlowe (Contract (Let), Observation, Value (Constant, UseValue), ValueId (ValueId))
import Language.Marlowe.ACTUS.Domain.ContractTerms
import Language.Marlowe.ACTUS.Domain.Ops (marloweFixedPoint)

useval :: String -> Integer -> Value Observation
useval name t = UseValue $ ValueId $ fromString $ name ++ "_" ++ show t

letval :: String -> Integer -> Value Observation -> Contract -> Contract
letval name t = Let $ ValueId $ fromString $ name ++ "_" ++ show t

letval' :: String -> Integer -> Maybe (Value Observation) -> Contract -> Contract
letval' name t (Just o) c = letval name t o c
letval' _ _ Nothing c     = c

toMarloweFixedPoint :: Double -> Integer
toMarloweFixedPoint = round <$> (fromIntegral marloweFixedPoint *)

constnt :: Double -> Value Observation
constnt = Constant . toMarloweFixedPoint

enum :: a -> a
enum = id

cardanoEpochStart :: Integer
cardanoEpochStart = 100

dayToSlotNumber :: Day -> Integer
dayToSlotNumber d =
  let (MkSystemTime secs _) = utcToSystemTime (UTCTime d 0)
   in fromIntegral secs - cardanoEpochStart

timeToSlotNumber :: LocalTime -> Integer
timeToSlotNumber LocalTime {..} =
  let (MkSystemTime secs _) = utcToSystemTime (UTCTime localDay (timeOfDayToTime localTimeOfDay))
   in fromIntegral secs - cardanoEpochStart

marloweDate :: Day -> Value Observation
marloweDate = Constant . fromInteger . dayToSlotNumber

marloweTime :: LocalTime -> Value Observation
marloweTime = Constant . fromInteger . timeToSlotNumber

toMarlowe :: ContractTerms -> ContractTermsMarlowe
toMarlowe ct =
  ContractTermsPoly
    { contractId = contractId ct,
      contractType = contractType ct,
      contractStructure = contractStructure ct,
      contractRole = contractRole ct,
      settlementCurrency = settlementCurrency ct,
      initialExchangeDate = initialExchangeDate ct,
      dayCountConvention = dayCountConvention ct,
      scheduleConfig = scheduleConfig ct,
      statusDate = statusDate ct,
      contractPerformance = contractPerformance ct,
      cycleOfFee = cycleOfFee ct,
      cycleAnchorDateOfFee = cycleAnchorDateOfFee ct,
      feeAccrued = constnt <$> feeAccrued ct,
      feeBasis = feeBasis ct,
      feeRate = constnt <$> feeRate ct,
      cycleAnchorDateOfInterestPayment = cycleAnchorDateOfInterestPayment ct,
      cycleOfInterestPayment = cycleOfInterestPayment ct,
      accruedInterest = constnt <$> accruedInterest ct,
      capitalizationEndDate = capitalizationEndDate ct,
      cycleAnchorDateOfInterestCalculationBase = cycleAnchorDateOfInterestCalculationBase ct,
      cycleOfInterestCalculationBase = cycleOfInterestCalculationBase ct,
      interestCalculationBase = interestCalculationBase ct,
      interestCalculationBaseA = constnt <$> interestCalculationBaseA ct,
      nominalInterestRate = constnt <$> nominalInterestRate ct,
      nominalInterestRate2 = constnt <$> nominalInterestRate2 ct,
      interestScalingMultiplier = constnt <$> interestScalingMultiplier ct,
      notionalPrincipal = constnt <$> notionalPrincipal ct,
      premiumDiscountAtIED = constnt <$> premiumDiscountAtIED ct,
      maturityDate = maturityDate ct,
      amortizationDate = amortizationDate ct,
      exerciseDate = exerciseDate ct,
      cycleAnchorDateOfPrincipalRedemption = cycleAnchorDateOfPrincipalRedemption ct,
      cycleOfPrincipalRedemption = cycleOfPrincipalRedemption ct,
      nextPrincipalRedemptionPayment = constnt <$> nextPrincipalRedemptionPayment ct,
      purchaseDate = purchaseDate ct,
      priceAtPurchaseDate = constnt <$> priceAtPurchaseDate ct,
      terminationDate = terminationDate ct,
      priceAtTerminationDate = constnt <$> priceAtTerminationDate ct,
      scalingIndexAtStatusDate = constnt <$> scalingIndexAtStatusDate ct,
      cycleAnchorDateOfScalingIndex = cycleAnchorDateOfScalingIndex ct,
      cycleOfScalingIndex = cycleOfScalingIndex ct,
      scalingEffect = scalingEffect ct,
      scalingIndexAtContractDealDate = constnt <$> scalingIndexAtContractDealDate ct,
      marketObjectCodeOfScalingIndex = marketObjectCodeOfScalingIndex ct,
      notionalScalingMultiplier = constnt <$> notionalScalingMultiplier ct,
      cycleOfOptionality = cycleOfOptionality ct,
      cycleAnchorDateOfOptionality = cycleAnchorDateOfOptionality ct,
      optionType = optionType ct,
      optionStrike1 = constnt <$> optionStrike1 ct,
      optionExerciseType = optionExerciseType ct,
      settlementPeriod = settlementPeriod ct,
      deliverySettlement = deliverySettlement ct,
      exerciseAmount = constnt <$> exerciseAmount ct,
      futuresPrice = constnt <$> futuresPrice ct,
      penaltyRate = constnt <$> penaltyRate ct,
      penaltyType = penaltyType ct,
      prepaymentEffect = prepaymentEffect ct,
      cycleOfRateReset = cycleOfRateReset ct,
      cycleAnchorDateOfRateReset = cycleAnchorDateOfRateReset ct,
      nextResetRate = constnt <$> nextResetRate ct,
      rateSpread = constnt <$> rateSpread ct,
      rateMultiplier = constnt <$> rateMultiplier ct,
      periodFloor = constnt <$> periodFloor ct,
      periodCap = constnt <$> periodCap ct,
      lifeCap = constnt <$> lifeCap ct,
      lifeFloor = constnt <$> lifeFloor ct,
      marketObjectCodeOfRateReset = marketObjectCodeOfRateReset ct,
      cycleOfDividend = cycleOfDividend ct,
      cycleAnchorDateOfDividend = cycleAnchorDateOfDividend ct,
      nextDividendPaymentAmount = constnt <$> nextDividendPaymentAmount ct,
      enableSettlement = enableSettlement ct,
      constraints = constraints ct
    }
