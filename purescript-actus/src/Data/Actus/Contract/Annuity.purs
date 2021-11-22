module Data.Actus.Contract.Annuity where

import Prelude

import Data.Actus.Types as Types
import Data.DateTime (DateTime)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)

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

derive instance eqContract :: Eq Contract
derive instance newtypeContract :: Newtype Contract _
derive instance genericContract :: Generic Contract _

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

type Fees =
  { rate :: Number
  , basis :: Types.FeeBasis
  , accrued :: Maybe Number
  , cycle :: Types.AnchoredCycle
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
  { ammortizationDate :: DateTime
  , initialExchangeDate :: DateTime
  , premiumDiscountAtIED :: Maybe Number
  , maturityDate :: Maybe DateTime
  , notionalPrincipal :: Number
  , purchase :: Maybe Types.ContractEndEvent
  , termination :: Maybe Types.ContractEndEvent
  , redemptionCycle :: Maybe Types.AnchoredCycle
  , nextPayment :: Maybe Number
  , scalingIndex :: Maybe Types.ScalingIndex
  }

mkNotionalPrincipal
  :: DateTime
  -> DateTime
  -> Maybe Number
  -> Maybe DateTime
  -> Number
  -> Maybe Types.ContractEndEvent
  -> Maybe Types.ContractEndEvent
  -> Maybe Types.AnchoredCycle
  -> Maybe Number
  -> Maybe Types.ScalingIndex
  -> NotionalPrincipal
mkNotionalPrincipal
  ammortizationDate
  initialExchangeDate
  premiumDiscountAtIED
  maturityDate
  notionalPrincipal
  purchase
  termination
  redemptionCycle
  nextPayment
  scalingIndex =
  { ammortizationDate
  , initialExchangeDate
  , premiumDiscountAtIED
  , maturityDate
  , notionalPrincipal
  , purchase
  , termination
  , redemptionCycle
  , nextPayment
  , scalingIndex
  }
