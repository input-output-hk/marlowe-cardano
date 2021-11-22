module Data.Actus.Terms where

import Data.Actus.Types as Types
import Data.DateTime (DateTime)

type Terms = Row Type -> Row Type

type AmortizationDate :: Terms
type AmortizationDate r = (amortizationDate :: DateTime | r)

type CollateralAmount :: Terms
type CollateralAmount r = (collateralAmount :: Int | r)

type ContractStructure :: Terms
type ContractStructure r =
  (contractStructure :: (Array Types.ContractStructure) | r)

type CycleAnchorDateOfDividend :: Terms
type CycleAnchorDateOfDividend r =
  (cycleAnchorDateOfDividend :: DateTime | r)

type CycleOfDividend :: Terms
type CycleOfDividend r = (cycleOfDividend :: Types.Cycle | r)

type DeliverySettlement :: Terms
type DeliverySettlement r =
  (deliverySettlement :: Types.DeliverySettlement | r)

type ExerciseAmount :: Terms
type ExerciseAmount r = (exerciseAmount :: Number | r)

type ExerciseDate :: Terms
type ExerciseDate r = (exerciseDate :: DateTime | r)

type FuturesPrice :: Terms
type FuturesPrice r = (futuresPrice :: Number | r)

type InterestScalingMultiplier :: Terms
type InterestScalingMultiplier r = (interestScalingMultiplier :: Number | r)

type MarketObjectCode :: Terms
type MarketObjectCode r = (marketObjectCode :: String | r)

type MarketObjectCodeOfScalingIndex :: Terms
type MarketObjectCodeOfScalingIndex r =
  (marketObjectCodeOfScalingIndex :: String | r)

type NextDividendPaymentAmount :: Terms
type NextDividendPaymentAmount r = (nextDividendPaymentAmount :: Number | r)

type NotionalScalingMultiplier :: Terms
type NotionalScalingMultiplier r = (notionalScalingMultiplier :: Number | r)

type OptionExerciseType :: Terms
type OptionExerciseType r =
  (optionExerciseType :: Types.OptionExerciseType | r)

type OptionStrike1 :: Terms
type OptionStrike1 r = (optionStrike1 :: Number | r)

type OptionType :: Terms
type OptionType r = (optionType :: Types.OptionType | r)

type ScalingEffect :: Terms
type ScalingEffect r = (scalingEffect :: Types.ScalingEffect | r)

type ScalingIndexAtContractDealDate :: Terms
type ScalingIndexAtContractDealDate r =
  (scalingIndexAtContractDealDate :: Number | r)

type ScalingIndexAtStatusDate :: Terms
type ScalingIndexAtStatusDate r = (scalingIndexAtStatusDate :: Number | r)

type SettlementCurrency :: Terms
type SettlementCurrency r = (settlementCurrency :: String | r)

type SettlementPeriod :: Terms
type SettlementPeriod r = (settlementPeriod :: Types.Cycle | r)
