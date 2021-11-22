module Data.Actus.Contract.Future where

import Data.Actus.Types as Types
import Data.DateTime (DateTime)
import Data.Maybe (Maybe)

newtype Contract = Contract
  ( Types.Contract
      ( calendar :: Types.Calendar
      , contractPerformance :: Types.ContractPerformance
      , contractStructure :: Types.ContractStructure
      , futuresPrice :: Number
      , notionalPrincipal :: NotionalPrincipal
      , settlement :: Maybe Types.Settlement
      )
  )

mkContract
  :: DateTime
  -> Types.ContractRole
  -> String
  -> Types.Calendar
  -> Types.ContractPerformance
  -> Types.ContractStructure
  -> NotionalPrincipal
  -> Maybe Types.Settlement
  -> Number
  -> Contract
mkContract
  statusDate
  contractRole
  contractId
  calendar
  contractPerformance
  contractStructure
  notionalPrincipal
  settlement
  futuresPrice =
  Contract
    { statusDate
    , contractRole
    , contractId
    , calendar
    , contractPerformance
    , contractStructure
    , futuresPrice
    , notionalPrincipal
    , settlement
    }

type NotionalPrincipal =
  { maturityDate :: DateTime
  , purchase :: Maybe Types.ContractEndEvent
  , termination :: Maybe Types.ContractEndEvent
  }

mkNotionalPrincipal
  :: DateTime
  -> Maybe Types.ContractEndEvent
  -> Maybe Types.ContractEndEvent
  -> NotionalPrincipal
mkNotionalPrincipal maturityDate purchase termination =
  { maturityDate, purchase, termination }

