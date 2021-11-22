module Data.Actus.Contract.NegativeAmortizer where

import Data.Actus.Types as Types
import Data.DateTime (DateTime)
import Data.Maybe (Maybe)

newtype Contract = Contract
  ( Types.Contract
      ( calendar :: Types.Calendar
      , contractPerformance :: Types.ContractPerformance
      , fees :: Maybe Types.Fees
      , interest :: Interest
      , notionalPrincipal :: NotionalPrincipal
      , optionality :: Maybe Types.LoanOptionality
      , rateReset :: Maybe Types.LoanRateReset
      )
  )

mkContract
  :: DateTime
  -> Types.ContractRole
  -> String
  -> Types.Calendar
  -> Types.ContractPerformance
  -> Maybe Types.Fees
  -> Interest
  -> NotionalPrincipal
  -> Maybe Types.LoanOptionality
  -> Maybe Types.LoanRateReset
  -> Contract
mkContract
  statusDate
  contractRole
  contractId
  calendar
  contractPerformance
  fees
  interest
  notionalPrincipal
  optionality
  rateReset =
  Contract
    { statusDate
    , contractRole
    , contractId
    , calendar
    , contractPerformance
    , fees
    , interest
    , notionalPrincipal
    , optionality
    , rateReset
    }

type Interest =
  { accrued :: Number
  , capitalizationEndDate :: DateTime
  , nominalRate :: Number
  , dayCountConvention :: Types.DayCountConvention
  , paymentCycle :: Maybe Types.AnchoredCycle
  , calculationBase :: Types.InterestCalculationBase
  }

mkInterest
  :: Number
  -> DateTime
  -> Number
  -> Types.DayCountConvention
  -> Maybe Types.AnchoredCycle
  -> Types.InterestCalculationBase
  -> Interest
mkInterest
  accrued
  capitalizationEndDate
  nominalRate
  dayCountConvention
  paymentCycle
  calculationBase =
  { accrued
  , capitalizationEndDate
  , nominalRate
  , dayCountConvention
  , paymentCycle
  , calculationBase
  }

type NotionalPrincipal =
  { initialExchangeDate :: DateTime
  , premiumDiscountAtIED :: Maybe Number
  , maturityDate :: Maybe DateTime
  , notionalPrincipal :: Number
  , purchase :: Maybe Types.ContractEndEvent
  , termination :: Maybe Types.ContractEndEvent
  , redemptionCycle :: Maybe Types.AnchoredCycle
  , nextPayment :: Number
  , scalingIndex :: Maybe Types.ScalingIndex
  }

mkNotionalPrincipal
  :: DateTime
  -> Maybe Number
  -> Maybe DateTime
  -> Number
  -> Maybe Types.ContractEndEvent
  -> Maybe Types.ContractEndEvent
  -> Maybe Types.AnchoredCycle
  -> Number
  -> Maybe Types.ScalingIndex
  -> NotionalPrincipal
mkNotionalPrincipal
  initialExchangeDate
  premiumDiscountAtIED
  maturityDate
  notionalPrincipal
  purchase
  termination
  redemptionCycle
  nextPayment
  scalingIndex =
  { initialExchangeDate
  , premiumDiscountAtIED
  , maturityDate
  , notionalPrincipal
  , purchase
  , termination
  , redemptionCycle
  , nextPayment
  , scalingIndex
  }
