module Data.Actus.Types
  ( module ContractStructure
  , AnchoredCycle
  , BusinessDayConvention(..)
  , Calendar
  , CalendarType(..)
  , Contract
  , ContractEndEvent
  , ContractPerformance(..)
  , ContractRole(..)
  , Cycle(..)
  , CycleStub(..)
  , DayCountConvention(..)
  , DeliverySettlement(..)
  , EndOfMonthConvention(..)
  , Fees
  , FeeBasis(..)
  , InterestCalculationBase(..)
  , LoanOptionality(..)
  , LoanRateReset(..)
  , OptionExerciseType(..)
  , OptionType(..)
  , PenaltyType(..)
  , PrepaymentEffect(..)
  , RateResetConstraints
  , ScalingEffect(..)
  , ScalingIndex
  , Settlement
  , mkCalender
  , mkFees
  , mkContractEndEvent
  , mkLoanOptionality
  , mkLoanRateReset
  , mkRateResetConstraints
  , mkScalingIndex
  , mkSettlement
  ) where

import Prelude

import Data.Actus.ContractStructure
  ( ContractReference
  , ContractStructure
  , ReferenceRole
  , ReferenceType
  ) as ContractStructure
import Data.DateTime (DateTime)
import Data.Interval.Duration.Iso (IsoDuration)
import Data.Map.Heterogeneous (HMap)
import Data.Map.Heterogeneous as HMap
import Data.Maybe (Maybe)
import Data.These (These)

type Contract r =
  { statusDate :: DateTime
  , contractRole :: ContractRole
  , contractId :: String
  | r
  }

data CalendarType
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

data Cycle = Cycle IsoDuration CycleStub

type ContractEndEvent =
  { date :: DateTime
  , priceAtEnd :: Number
  }

mkContractEndEvent :: DateTime -> Number -> ContractEndEvent
mkContractEndEvent date priceAtEnd =
  { date, priceAtEnd }

type AnchoredCycle = These DateTime Cycle

type ScalingIndex =
  { cycle :: AnchoredCycle
  , effect :: ScalingEffect
  , marketObjectCode :: String
  }

mkScalingIndex :: AnchoredCycle -> ScalingEffect -> String -> ScalingIndex
mkScalingIndex cycle effect marketObjectCode =
  { cycle, effect, marketObjectCode }

type Calendar = HMap
  ( calendar :: CalendarType
  , businessDayConvention :: BusinessDayConvention
  , endOfMonthConvention :: EndOfMonthConvention
  )

mkCalender
  :: Maybe CalendarType
  -> Maybe BusinessDayConvention
  -> Maybe EndOfMonthConvention
  -> Calendar
mkCalender calendarType businessDayConvention endOfMonthConvention =
  HMap.fromRecord
    { calendar: calendarType, businessDayConvention, endOfMonthConvention }

type Fees =
  { rate :: Number
  , basis :: FeeBasis
  , accrued :: Maybe Number
  , cycle :: AnchoredCycle
  }

mkFees :: Number -> FeeBasis -> Maybe Number -> AnchoredCycle -> Fees
mkFees rate basis accrued cycle = { rate, basis, accrued, cycle }

data LoanOptionality =
  LoanOptionality
    PrepaymentEffect
    ( HMap
        ( cycle :: AnchoredCycle
        , penaltyType :: PenaltyType
        , penaltyRate :: Number
        )
    )

mkLoanOptionality
  :: PrepaymentEffect
  -> Maybe AnchoredCycle
  -> Maybe PenaltyType
  -> Maybe Number
  -> LoanOptionality
mkLoanOptionality prepaymentEffect cycle penaltyType penaltyRate =
  LoanOptionality prepaymentEffect
    $ HMap.fromRecord { cycle, penaltyType, penaltyRate }

type RateResetConstraints =
  HMap
    ( lifeCap :: Number
    , lifeFloor :: Number
    , periodCap :: Number
    , periodFloor :: Number
    )

mkRateResetConstraints
  :: Maybe Number
  -> Maybe Number
  -> Maybe Number
  -> Maybe Number
  -> RateResetConstraints
mkRateResetConstraints lifeCap lifeFloor periodCap periodFloor =
  HMap.fromRecord
    { lifeCap
    , lifeFloor
    , periodCap
    , periodFloor
    }

type LoanRateReset =
  { cycle :: AnchoredCycle
  , rateSpread :: Number
  , marketObjectCode :: String
  , constraints :: RateResetConstraints
  , nextResetRate :: Maybe Number
  , rateMultiplier :: Maybe Number
  }

mkLoanRateReset
  :: AnchoredCycle
  -> Number
  -> String
  -> RateResetConstraints
  -> Maybe Number
  -> Maybe Number
  -> LoanRateReset
mkLoanRateReset
  cycle
  rateSpread
  marketObjectCode
  constraints
  nextResetRate
  rateMultiplier =
  { cycle
  , rateSpread
  , marketObjectCode
  , constraints
  , nextResetRate
  , rateMultiplier
  }

type Settlement =
  { exerciseAmount :: Number
  , exerciseDate :: DateTime
  , period :: Maybe IsoDuration
  , deliverySettlement :: Maybe DeliverySettlement
  }

mkSettlement
  :: Number
  -> DateTime
  -> Maybe IsoDuration
  -> Maybe DeliverySettlement
  -> Settlement
mkSettlement
  exerciseAmount
  exerciseDate
  period
  deliverySettlement =
  { exerciseAmount
  , exerciseDate
  , period
  , deliverySettlement
  }

