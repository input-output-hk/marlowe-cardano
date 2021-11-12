module Data.Actus.Terms where

import Data.Actus.Types as Types
import Data.DateTime (DateTime)

type Terms f a = (a -> a) -> f a

type AccruedInterest :: Terms Row Type
type AccruedInterest f = (accruedInterest :: f Number)

type AmortizationDate :: Terms Row Type
type AmortizationDate f = (amortizationDate :: f DateTime)

type CapitalizationEndDate :: Terms Row Type
type CapitalizationEndDate f = (capitalizationEndDate :: f DateTime)

type CollateralAmount :: Terms Row Type
type CollateralAmount f = (collateralAmount :: f Int)

type ContractId :: Terms Row Type
type ContractId f = (contractId :: f String)

type ContractPerformance :: Terms Row Type
type ContractPerformance f =
  (contractPerformance :: f Types.ContractPerformance)

type ContractRole :: Terms Row Type
type ContractRole f = (contractRole :: f Types.ContractRole)

type ContractStructure :: Terms Row Type
type ContractStructure f =
  (contractStructure :: f (Array Types.ContractStructure))

type CycleAnchorDateOfDividend :: Terms Row Type
type CycleAnchorDateOfDividend f = (cycleAnchorDateOfDividend :: f DateTime)

type CycleAnchorDateOfFee :: Terms Row Type
type CycleAnchorDateOfFee f = (cycleAnchorDateOfFee :: f DateTime)

type CycleAnchorDateOfInterestCalculationBase :: Terms Row Type
type CycleAnchorDateOfInterestCalculationBase f =
  ( cycleAnchorDateOfInterestCalculationBase :: f DateTime
  )

type CycleAnchorDateOfInterestPayment :: Terms Row Type
type CycleAnchorDateOfInterestPayment f =
  (cycleAnchorDateOfInterestPayment :: f DateTime)

type CycleAnchorDateOfOptionality :: Terms Row Type
type CycleAnchorDateOfOptionality f =
  (cycleAnchorDateOfOptionality :: f DateTime)

type CycleAnchorDateOfPrincipalRedemption :: Terms Row Type
type CycleAnchorDateOfPrincipalRedemption f =
  (cycleAnchorDateOfPrincipalRedemption :: f DateTime)

type CycleAnchorDateOfRateReset :: Terms Row Type
type CycleAnchorDateOfRateReset f = (cycleAnchorDateOfRateReset :: f DateTime)

type CycleAnchorDateOfScalingIndex :: Terms Row Type
type CycleAnchorDateOfScalingIndex f =
  (cycleAnchorDateOfScalingIndex :: f DateTime)

type CycleOfDividend :: Terms Row Type
type CycleOfDividend f = (cycleOfDividend :: f Types.Cycle)

type CycleOfFee :: Terms Row Type
type CycleOfFee f = (cycleOfFee :: f Types.Cycle)

type CycleOfInterestCalculationBase :: Terms Row Type
type CycleOfInterestCalculationBase f =
  (cycleOfInterestCalculationBase :: f Types.Cycle)

type CycleOfInterestPayment :: Terms Row Type
type CycleOfInterestPayment f = (cycleOfInterestPayment :: f Types.Cycle)

type CycleOfOptionality :: Terms Row Type
type CycleOfOptionality f = (cycleOfOptionality :: f Types.Cycle)

type CycleOfPrincipalRedemption :: Terms Row Type
type CycleOfPrincipalRedemption f =
  (cycleOfPrincipalRedemption :: f Types.Cycle)

type CycleOfRateReset :: Terms Row Type
type CycleOfRateReset f = (cycleOfRateReset :: f Types.Cycle)

type CycleOfScalingIndex :: Terms Row Type
type CycleOfScalingIndex f = (cycleOfScalingIndex :: f Types.Cycle)

type DayCountConvention :: Terms Row Type
type DayCountConvention f = (dayCountConvention :: f Types.DayCountConvention)

type DeliverySettlement :: Terms Row Type
type DeliverySettlement f = (deliverySettlement :: f Types.DeliverySettlement)

type ExerciseAmount :: Terms Row Type
type ExerciseAmount f = (exerciseAmount :: f Number)

type ExerciseDate :: Terms Row Type
type ExerciseDate f = (exerciseDate :: f DateTime)

type FeeAccrued :: Terms Row Type
type FeeAccrued f = (feeAccrued :: f Number)

type FeeBasis :: Terms Row Type
type FeeBasis f = (feeBasis :: f Types.FeeBasis)

type FeeRate :: Terms Row Type
type FeeRate f = (feeRate :: f Number)

type FuturesPrice :: Terms Row Type
type FuturesPrice f = (futuresPrice :: f Number)

type InitialExchangeDate :: Terms Row Type
type InitialExchangeDate f = (initialExchangeDate :: f DateTime)

type InterestCalculationBase :: Terms Row Type
type InterestCalculationBase f =
  (interestCalculationBase :: f Types.InterestCalculationBase)

type InterestCalculationBaseA :: Terms Row Type
type InterestCalculationBaseA f = (interestCalculationBaseA :: f Number)

type InterestScalingMultiplier :: Terms Row Type
type InterestScalingMultiplier f = (interestScalingMultiplier :: f Number)

type LifeCap :: Terms Row Type
type LifeCap f = (lifeCap :: f Number)

type LifeFloor :: Terms Row Type
type LifeFloor f = (lifeFloor :: f Number)

type MarketObjectCodeOfRateReset :: Terms Row Type
type MarketObjectCodeOfRateReset f = (marketObjectCodeOfRateReset :: f String)

type MarketObjectCodeOfScalingIndex :: Terms Row Type
type MarketObjectCodeOfScalingIndex f =
  (marketObjectCodeOfScalingIndex :: f String)

type MaturityDate :: Terms Row Type
type MaturityDate f = (maturityDate :: f DateTime)

type NextDividendPaymentAmount :: Terms Row Type
type NextDividendPaymentAmount f = (nextDividendPaymentAmount :: f Number)

type NextPrincipalRedemptionPayment :: Terms Row Type
type NextPrincipalRedemptionPayment f =
  (nextPrincipalRedemptionPayment :: f Number)

type NextResetRate :: Terms Row Type
type NextResetRate f = (nextResetRate :: f Number)

type NominalInterestRate :: Terms Row Type
type NominalInterestRate f = (nominalInterestRate :: f Number)

type NotionalPrincipal :: Terms Row Type
type NotionalPrincipal f = (notionalPrincipal :: f Number)

type NotionalScalingMultiplier :: Terms Row Type
type NotionalScalingMultiplier f = (notionalScalingMultiplier :: f Number)

type OptionExerciseType :: Terms Row Type
type OptionExerciseType f = (optionExerciseType :: f Types.OptionExerciseType)

type OptionStrike1 :: Terms Row Type
type OptionStrike1 f = (optionStrike1 :: f Number)

type OptionType :: Terms Row Type
type OptionType f = (optionType :: f Types.OptionType)

type PenaltyRate :: Terms Row Type
type PenaltyRate f = (penaltyRate :: f Number)

type PenaltyType :: Terms Row Type
type PenaltyType f = (penaltyType :: f Types.PenaltyType)

type PeriodCap :: Terms Row Type
type PeriodCap f = (periodCap :: f Number)

type PeriodFloor :: Terms Row Type
type PeriodFloor f = (periodFloor :: f Number)

type PremiumDiscountAtIED :: Terms Row Type
type PremiumDiscountAtIED f = (premiumDiscountAtIED :: f Number)

type PrepaymentEffect :: Terms Row Type
type PrepaymentEffect f = (prepaymentEffect :: f Types.PrepaymentEffect)

type PriceAtPurchaseDate :: Terms Row Type
type PriceAtPurchaseDate f = (priceAtPurchaseDate :: f Number)

type PriceAtTerminationDate :: Terms Row Type
type PriceAtTerminationDate f = (priceAtTerminationDate :: f Number)

type PurchaseDate :: Terms Row Type
type PurchaseDate f = (purchaseDate :: f DateTime)

type RateMultiplier :: Terms Row Type
type RateMultiplier f = (rateMultiplier :: f Number)

type RateSpread :: Terms Row Type
type RateSpread f = (rateSpread :: f Number)

type ScalingEffect :: Terms Row Type
type ScalingEffect f = (scalingEffect :: f Types.ScalingEffect)

type ScalingIndexAtContractDealDate :: Terms Row Type
type ScalingIndexAtContractDealDate f =
  (scalingIndexAtContractDealDate :: f Number)

type ScalingIndexAtStatusDate :: Terms Row Type
type ScalingIndexAtStatusDate f = (scalingIndexAtStatusDate :: f Number)

type SettlementCurrency :: Terms Row Type
type SettlementCurrency f = (settlementCurrency :: f String)

type SettlementPeriod :: Terms Row Type
type SettlementPeriod f = (settlementPeriod :: f Types.Cycle)

type StatusDate :: Terms Row Type
type StatusDate f = (statusDate :: f DateTime)

type TerminationDate :: Terms Row Type
type TerminationDate f = (terminationDate :: f DateTime)
