module Data.Actus.Contract.Future where

import Data.Actus.Contract (Contract)
import Data.Actus.Types as Types
import Data.DateTime (DateTime)
import Data.Interval.Duration.Iso (IsoDuration)
import Data.Map.Heterogeneous (HMap)
import Data.Maybe (Maybe)

newtype Future = Future
  ( Contract
      ( calendar :: Calendar
      , contractPerformance :: Types.ContractPerformance
      , contractStructure :: Types.ContractStructure
      , notionalPrincipal :: NotionalPrincipal
      , settlement :: Maybe Settlement
      , futuresPrice :: Number
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

type Settlement =
  { exerciseAmount :: Number
  , exerciseDate :: DateTime
  , period :: Maybe IsoDuration
  , deliverySettlement :: Maybe Types.DeliverySettlement
  }
