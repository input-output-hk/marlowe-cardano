module Data.Actus.Contract.Annuity where

import Data.Actus.Contract (Contract)
import Data.Actus.Types as Types
import Data.DateTime (DateTime)
import Data.Map.Heterogeneous (HMap)
import Data.Maybe (Maybe)

newtype Annuity = Annuity
  ( Contract
      ( calendar :: Calendar
      , contractPerformance :: Types.ContractPerformance
      , fees :: Maybe Fees
      , interest :: Interest
      , notionalPrincipal :: NotionalPrincipal
      , optionality :: Maybe Optionality
      , rateReset :: Maybe RateReset
      )
  )

type Calendar = HMap
  ( capitalizationEndDate :: Types.Calendar
  , businessDayConvention :: Types.BusinessDayConvention
  , endOfMonthConvention :: Types.EndOfMonthConvention
  )

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
  , scaling ::
      Maybe
        { cycle :: Types.AnchoredCycle
        , effect :: Types.ScalingEffect
        , marketObjectCodeOfIndex :: String
        }
  }

data Optionality =
  Optionality
    Types.PrepaymentEffect
    ( HMap
        ( cycle :: Maybe Types.AnchoredCycle
        , penaltyType :: Types.PenaltyType
        , penaltyRate :: Number
        )
    )

type RateReset =
  { cycle :: Types.AnchoredCycle
  , rateSpread :: Number
  , marketObjectCode :: String
  , constraints ::
      HMap
        ( lifeCap :: Number
        , lifeFloor :: Number
        , periodCap :: Number
        , periodFloor :: Number
        )
  , nextResetRate :: Maybe Number
  , rateMultiplier :: Maybe Number
  }
