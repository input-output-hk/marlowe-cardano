module Data.Actus.Types
  ( module ContractStructure
  , ContractPerformance
  , ContractRole
  , Cycle
  , CycleStub
  , DayCountConvention
  , DeliverySettlement
  , FeeBasis
  , InterestCalculationBase
  , OptionExerciseType
  , OptionType
  , PenaltyType
  , PrepaymentEffect
  , ScalingEffect
  ) where

import Prelude

import Data.Actus.ContractStructure
  ( ContractReference
  , ContractStructure
  , ReferenceRole
  , ReferenceType
  ) as ContractStructure
import Data.Interval.Duration.Iso (IsoDuration)
import Data.Variant (Variant)

type ContractRole = Variant
  ( realPositionAsset :: Unit
  , realPositionLiability :: Unit
  , receiveFirstLegl :: Unit
  , payFirstLeg :: Unit
  , receiveFix :: Unit
  , payFix :: Unit
  , buyer :: Unit
  , seller :: Unit
  , collateralPosition :: Unit
  , closeOutNetting :: Unit
  , underlying :: Unit
  , underlyingPlus :: Unit
  , underlyingMinus :: Unit
  )

type DayCountConvention = Variant
  ( actualActual :: Unit
  , actualThreeSixty :: Unit
  , actualThreeSixtyFive :: Unit
  , thirtyEThreeSixtyISDA :: Unit
  , thirtyEThreeSixty :: Unit
  , twentyEightEThreeThirtySix :: Unit
  )

type EndOfMonthConvention = Variant
  ( sameDay :: Unit
  , endOfMonth :: Unit
  )

type BusinessDayConvention = Variant
  ( noShift :: Unit
  , shiftCalculateFollowing :: Unit
  , shiftCalculateModifiedFollowing :: Unit
  , calculateShiftFollowing :: Unit
  , calculateShiftModifiedFollowing :: Unit
  , shiftCalculatePreceding :: Unit
  , shiftCalculateModifiedPreceding :: Unit
  , calculateShiftPreceding :: Unit
  , calculateShiftModifiedPreceding :: Unit
  )

type Calendar = Variant
  ( noCalendar :: Unit
  , mondayToFriday :: Unit
  )

type ContractPerformance = Variant
  ( performant :: Unit
  , delayed :: Unit
  , delinquent :: Unit
  , default :: Unit
  , matured :: Unit
  , terminated :: Unit
  )

type FeeBasis = Variant
  ( absoluteValue :: Unit
  , nonimalValueOfTheUnderlying :: Unit
  )

type InterestCalculationBase = Variant
  ( notioalOutstanding :: Unit
  , notionalAtInitialExchange :: Unit
  , notionalLagged :: Unit
  )

type ScalingEffect = Variant
  ( noScaling :: Unit
  , interestIsScaled :: Unit
  , principalIsScaled :: Unit
  , interestAndPrincipalIsScaled :: Unit
  )

type PenaltyType = Variant
  ( noPenalty :: Unit
  , fixedPenalty :: Unit
  , relativePenalty :: Unit
  , interestRateDifferential :: Unit
  )

type OptionType = Variant
  ( call :: Unit
  , put :: Unit
  , callPut :: Unit
  )

type OptionExerciseType = Variant
  ( european :: Unit
  , bermudan :: Unit
  , american :: Unit
  )

type DeliverySettlement = Variant
  ( cashSettlement :: Unit
  , physicalSettlement :: Unit
  )

type PrepaymentEffect = Variant
  ( noPrepayment :: Unit
  , prepaymentReducesRedemptionAmount :: Unit
  , prepaymentReducesMaturity :: Unit
  )

type CycleStub = Variant
  ( shortStub :: Unit
  , longStub :: Unit
  )

type Cycle =
  { duration :: IsoDuration
  , stub :: CycleStub
  }
