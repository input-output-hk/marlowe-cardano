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
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.DateTime (DateTime)
import Data.Enum (class BoundedEnum, class Enum)
import Data.Enum.Generic
  ( genericCardinality
  , genericFromEnum
  , genericPred
  , genericSucc
  , genericToEnum
  )
import Data.Generic.Rep (class Generic)
import Data.Interval.Duration.Iso (IsoDuration)
import Data.Map.Heterogeneous (HMap)
import Data.Map.Heterogeneous as HMap
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
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

derive instance eqCalendarType :: Eq CalendarType
derive instance ordCalendarType :: Ord CalendarType
derive instance genericCalendarType :: Generic CalendarType _
instance showCalendarType :: Show CalendarType where
  show = genericShow

instance enumCalendarType :: Enum CalendarType where
  succ = genericSucc
  pred = genericPred

instance boundedCalendarType :: Bounded CalendarType where
  top = genericTop
  bottom = genericBottom

instance boundedEnumCalendarType :: BoundedEnum CalendarType where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

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

derive instance eqContractRole :: Eq ContractRole
derive instance ordContractRole :: Ord ContractRole
derive instance genericContractRole :: Generic ContractRole _
instance showContractRole :: Show ContractRole where
  show = genericShow

instance enumContractRole :: Enum ContractRole where
  succ = genericSucc
  pred = genericPred

instance boundedContractRole :: Bounded ContractRole where
  top = genericTop
  bottom = genericBottom

instance boundedEnumContractRole :: BoundedEnum ContractRole where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

data DayCountConvention
  = ActualActual
  | ActualThreeSixty
  | ActualThreeSixtyFive
  | ThirtyEThreeSixtyISDA
  | ThirtyEThreeSixty
  | TwentyEightEThreeThirtySix

derive instance eqDayCountConvention :: Eq DayCountConvention
derive instance ordDayCountConvention :: Ord DayCountConvention
derive instance genericDayCountConvention :: Generic DayCountConvention _
instance showDayCountConvention :: Show DayCountConvention where
  show = genericShow

instance enumDayCountConvention :: Enum DayCountConvention where
  succ = genericSucc
  pred = genericPred

instance boundedDayCountConvention :: Bounded DayCountConvention where
  top = genericTop
  bottom = genericBottom

instance boundedEnumDayCountConvention :: BoundedEnum DayCountConvention where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

data EndOfMonthConvention
  = SameDay
  | EndOfMonth

derive instance eqEndOfMonthConvention :: Eq EndOfMonthConvention
derive instance ordEndOfMonthConvention :: Ord EndOfMonthConvention
derive instance genericEndOfMonthConvention :: Generic EndOfMonthConvention _
instance showEndOfMonthConvention :: Show EndOfMonthConvention where
  show = genericShow

instance enumEndOfMonthConvention :: Enum EndOfMonthConvention where
  succ = genericSucc
  pred = genericPred

instance boundedEndOfMonthConvention :: Bounded EndOfMonthConvention where
  top = genericTop
  bottom = genericBottom

instance boundedEnumEndOfMonthConvention :: BoundedEnum EndOfMonthConvention where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

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

derive instance eqBusinessDayConvention :: Eq BusinessDayConvention
derive instance ordBusinessDayConvention :: Ord BusinessDayConvention
derive instance genericBusinessDayConvention :: Generic BusinessDayConvention _
instance showBusinessDayConvention :: Show BusinessDayConvention where
  show = genericShow

instance enumBusinessDayConvention :: Enum BusinessDayConvention where
  succ = genericSucc
  pred = genericPred

instance boundedBusinessDayConvention :: Bounded BusinessDayConvention where
  top = genericTop
  bottom = genericBottom

instance boundedEnumBusinessDayConvention :: BoundedEnum BusinessDayConvention where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

data ContractPerformance
  = Performant
  | Delayed
  | Delinquent
  | Default
  | Matured
  | Terminated

derive instance eqContractPerformance :: Eq ContractPerformance
derive instance ordContractPerformance :: Ord ContractPerformance
derive instance genericContractPerformance :: Generic ContractPerformance _
instance showContractPerformance :: Show ContractPerformance where
  show = genericShow

instance enumContractPerformance :: Enum ContractPerformance where
  succ = genericSucc
  pred = genericPred

instance boundedContractPerformance :: Bounded ContractPerformance where
  top = genericTop
  bottom = genericBottom

instance boundedEnumContractPerformance :: BoundedEnum ContractPerformance where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

data FeeBasis
  = AbsoluteValue
  | NonimalValueOfTheUnderlying

derive instance eqFeeBasis :: Eq FeeBasis
derive instance ordFeeBasis :: Ord FeeBasis
derive instance genericFeeBasis :: Generic FeeBasis _
instance showFeeBasis :: Show FeeBasis where
  show = genericShow

instance enumFeeBasis :: Enum FeeBasis where
  succ = genericSucc
  pred = genericPred

instance boundedFeeBasis :: Bounded FeeBasis where
  top = genericTop
  bottom = genericBottom

instance boundedEnumFeeBasis :: BoundedEnum FeeBasis where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

data InterestCalculationBase
  = NotioalOutstanding
  | NotionalAtInitialExchange Number
  | NotionalLagged AnchoredCycle

derive instance eqInterestCalculationBase :: Eq InterestCalculationBase
derive instance ordInterestCalculationBase :: Ord InterestCalculationBase
derive instance genericInterestCalculationBase ::
  Generic InterestCalculationBase _

instance showInterestCalculationBase :: Show InterestCalculationBase where
  show = genericShow

data ScalingEffect
  = NoScaling
  | InterestIsScaled
  | PrincipalIsScaled
  | InterestAndPrincipalIsScaled

derive instance eqScalingEffect :: Eq ScalingEffect
derive instance ordScalingEffect :: Ord ScalingEffect
derive instance genericScalingEffect :: Generic ScalingEffect _
instance showScalingEffect :: Show ScalingEffect where
  show = genericShow

instance enumScalingEffect :: Enum ScalingEffect where
  succ = genericSucc
  pred = genericPred

instance boundedScalingEffect :: Bounded ScalingEffect where
  top = genericTop
  bottom = genericBottom

instance boundedEnumScalingEffect :: BoundedEnum ScalingEffect where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

data PenaltyType
  = NoPenalty
  | FixedPenalty
  | RelativePenalty
  | InterestRateDifferential

derive instance eqPenaltyType :: Eq PenaltyType
derive instance ordPenaltyType :: Ord PenaltyType
derive instance genericPenaltyType :: Generic PenaltyType _
instance showPenaltyType :: Show PenaltyType where
  show = genericShow

instance enumPenaltyType :: Enum PenaltyType where
  succ = genericSucc
  pred = genericPred

instance boundedPenaltyType :: Bounded PenaltyType where
  top = genericTop
  bottom = genericBottom

instance boundedEnumPenaltyType :: BoundedEnum PenaltyType where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

data OptionType
  = Call
  | Put
  | CallPut

derive instance eqOptionType :: Eq OptionType
derive instance ordOptionType :: Ord OptionType
derive instance genericOptionType :: Generic OptionType _
instance showOptionType :: Show OptionType where
  show = genericShow

instance enumOptionType :: Enum OptionType where
  succ = genericSucc
  pred = genericPred

instance boundedOptionType :: Bounded OptionType where
  top = genericTop
  bottom = genericBottom

instance boundedEnumOptionType :: BoundedEnum OptionType where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

data OptionExerciseType
  = European
  | Bermudan
  | American

derive instance eqOptionExerciseType :: Eq OptionExerciseType
derive instance ordOptionExerciseType :: Ord OptionExerciseType
derive instance genericOptionExerciseType :: Generic OptionExerciseType _
instance showOptionExerciseType :: Show OptionExerciseType where
  show = genericShow

instance enumOptionExerciseType :: Enum OptionExerciseType where
  succ = genericSucc
  pred = genericPred

instance boundedOptionExerciseType :: Bounded OptionExerciseType where
  top = genericTop
  bottom = genericBottom

instance boundedEnumOptionExerciseType :: BoundedEnum OptionExerciseType where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

data DeliverySettlement
  = CashSettlement
  | PhysicalSettlement

derive instance eqDeliverySettlement :: Eq DeliverySettlement
derive instance ordDeliverySettlement :: Ord DeliverySettlement
derive instance genericDeliverySettlement :: Generic DeliverySettlement _
instance showDeliverySettlement :: Show DeliverySettlement where
  show = genericShow

instance enumDeliverySettlement :: Enum DeliverySettlement where
  succ = genericSucc
  pred = genericPred

instance boundedDeliverySettlement :: Bounded DeliverySettlement where
  top = genericTop
  bottom = genericBottom

instance boundedEnumDeliverySettlement :: BoundedEnum DeliverySettlement where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

data PrepaymentEffect
  = NoPrepayment
  | PrepaymentReducesRedemptionAmount
  | PrepaymentReducesMaturity

derive instance eqPrepaymentEffect :: Eq PrepaymentEffect
derive instance ordPrepaymentEffect :: Ord PrepaymentEffect
derive instance genericPrepaymentEffect :: Generic PrepaymentEffect _
instance showPrepaymentEffect :: Show PrepaymentEffect where
  show = genericShow

instance enumPrepaymentEffect :: Enum PrepaymentEffect where
  succ = genericSucc
  pred = genericPred

instance boundedPrepaymentEffect :: Bounded PrepaymentEffect where
  top = genericTop
  bottom = genericBottom

instance boundedEnumPrepaymentEffect :: BoundedEnum PrepaymentEffect where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

data CycleStub
  = ShortStub
  | LongStub

derive instance eqCycleStub :: Eq CycleStub
derive instance ordCycleStub :: Ord CycleStub
derive instance genericCycleStub :: Generic CycleStub _
instance showCycleStub :: Show CycleStub where
  show = genericShow

instance enumCycleStub :: Enum CycleStub where
  succ = genericSucc
  pred = genericPred

instance boundedCycleStub :: Bounded CycleStub where
  top = genericTop
  bottom = genericBottom

instance boundedEnumCycleStub :: BoundedEnum CycleStub where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

data Cycle = Cycle IsoDuration CycleStub

derive instance eqCycle :: Eq Cycle
derive instance ordCycle :: Ord Cycle
derive instance genericCycle :: Generic Cycle _
instance showCycle :: Show Cycle where
  show = genericShow

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

derive instance eqLoanOptionality :: Eq LoanOptionality
derive instance genericLoanOptionality :: Generic LoanOptionality _
instance showLoanOptionality :: Show LoanOptionality where
  show = genericShow

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

