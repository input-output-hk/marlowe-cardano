module Data.Actus.Types
  ( module ContractStructure
  , AnchoredCycle
  , BusinessDayConvention
  , Calendar
  , ContractEndEvent
  , ContractPerformance
  , ContractRole
  , Cycle
  , CycleStub
  , DayCountConvention
  , DeliverySettlement
  , EndOfMonthConvention
  , FeeBasis
  , InterestCalculationBase
  , OptionExerciseType
  , OptionType
  , PenaltyType
  , PrepaymentEffect
  , ScalingEffect
  ) where

import Data.Actus.ContractStructure
  ( ContractReference
  , ContractStructure
  , ReferenceRole
  , ReferenceType
  ) as ContractStructure
import Data.DateTime (DateTime)
import Data.Interval.Duration.Iso (IsoDuration)
import Data.These (These)

data Calendar
  = NoCalendar
  | MondayToFriday

data ContractRole
  = RealPositionAsset
  | RealPositionLiability
  | ReceiveFirstLegl
  | PayFirstLeg
  | ReceiveFix
  | PayFix
  | Buyer
  | Seller
  | CollateralPosition
  | CloseOutNetting
  | Underlying
  | UnderlyingPlus
  | UnderlyingMinus

data DayCountConvention
  = ActualActual
  | ActualThreeSixty
  | ActualThreeSixtyFive
  | ThirtyEThreeSixtyISDA
  | ThirtyEThreeSixty
  | TwentyEightEThreeThirtySix

data EndOfMonthConvention
  = SameDay
  | EndOfMonth

data BusinessDayConvention
  = NoShift
  | ShiftCalculateFollowing
  | ShiftCalculateModifiedFollowing
  | CalculateShiftFollowing
  | CalculateShiftModifiedFollowing
  | ShiftCalculatePreceding
  | ShiftCalculateModifiedPreceding
  | CalculateShiftPreceding
  | CalculateShiftModifiedPreceding

data ContractPerformance
  = Performant
  | Delayed
  | Delinquent
  | Default
  | Matured
  | Terminated

data FeeBasis
  = AbsoluteValue
  | NonimalValueOfTheUnderlying

data InterestCalculationBase
  = NotioalOutstanding
  | NotionalAtInitialExchange Number
  | NotionalLagged AnchoredCycle

data ScalingEffect
  = NoScaling
  | InterestIsScaled
  | PrincipalIsScaled
  | InterestAndPrincipalIsScaled

data PenaltyType
  = NoPenalty
  | FixedPenalty
  | RelativePenalty
  | InterestRateDifferential

data OptionType
  = Call
  | Put
  | CallPut

data OptionExerciseType
  = European
  | Bermudan
  | American

data DeliverySettlement
  = CashSettlement
  | PhysicalSettlement

data PrepaymentEffect
  = NoPrepayment
  | PrepaymentReducesRedemptionAmount
  | PrepaymentReducesMaturity

data CycleStub
  = ShortStub
  | LongStub

type Cycle =
  { duration :: IsoDuration
  , stub :: CycleStub
  }

type ContractEndEvent =
  { date :: DateTime
  , priceAtEnd :: Number
  }

type AnchoredCycle = These DateTime Cycle
