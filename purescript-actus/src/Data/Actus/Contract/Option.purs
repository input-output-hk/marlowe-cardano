module Data.Actus.Contract.Option where

import Data.Actus.Contract (Contract)
import Data.Actus.Types as Types
import Data.DateTime (DateTime)
import Data.Interval.Duration.Iso (IsoDuration)
import Data.Map.Heterogeneous (HMap)
import Data.Maybe (Maybe)

newtype Option = Option
  ( Contract
      ( calendar :: Calendar
      , contractPerformance :: Types.ContractPerformance
      , contractStructure :: Types.ContractStructure
      , notionalPrincipal :: NotionalPrincipal
      , optionality :: Optionality
      , settlement :: Maybe Settlement
      )
  )

type Calendar = HMap
  ( capitalizationEndDate :: Types.Calendar
  , businessDayConvention :: Types.BusinessDayConvention
  , endOfMonthConvention :: Types.EndOfMonthConvention
  )

type NotionalPrincipal =
  { maturityDate :: DateTime
  , purchase :: Maybe Types.ContractEndEvent
  , termination :: Maybe Types.ContractEndEvent
  }

type Optionality =
  { optionExerciseType :: Types.OptionExerciseType
  , optionExerciseEndDate :: DateTime
  , optionStrike1 :: Number
  , optionType :: Types.OptionType
  , cycle :: Maybe Types.AnchoredCycle
  }

type Settlement =
  { exerciseAmount :: Number
  , exerciseDate :: DateTime
  , period :: Maybe IsoDuration
  , deliverySettlement :: Maybe Types.DeliverySettlement
  }
