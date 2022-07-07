{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
module Actus.Domain
  ( module Actus.Domain.BusinessEvents
  , module Actus.Domain.ContractState
  , module Actus.Domain.ContractTerms
  , module Actus.Domain.Schedule
  , RiskFactors (..)
  , CashFlow (..)
  , ActusOps (..)
  , ActusNum (..)
  , RoleSignOps (..)
  , ScheduleOps (..)
  , YearFractionOps (..)
  , marloweFixedPoint
  , setDefaultContractTermValues
  )
  where

import Actus.Domain.BusinessEvents
import Actus.Domain.ContractState
import Actus.Domain.ContractTerms
import Actus.Domain.Schedule
import Actus.Utility.YearFraction (yearFraction)
import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Time (LocalTime)
import GHC.Generics (Generic)

{-| Risk factor observer
-}
data RiskFactors a = RiskFactors
    { o_rf_CURS :: a
    , o_rf_RRMO :: a
    , o_rf_SCMO :: a
    , pp_payoff :: a
    , xd_payoff :: a
    , dv_payoff :: a
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

{-| Cash flows
-}
data CashFlow a = CashFlow
  { tick               :: Integer,
    cashContractId     :: String,
    cashParty          :: String,
    cashCounterParty   :: String,
    cashPaymentDay     :: LocalTime,
    cashCalculationDay :: LocalTime,
    cashEvent          :: EventType,
    amount             :: a,
    notional           :: a,
    cashCurrency       :: String
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

marloweFixedPoint :: Integer
marloweFixedPoint = 1000000

class ActusOps a where
    _min  :: a -> a -> a
    _max  :: a -> a -> a
    _abs  :: a -> a
    _zero :: a
    _one  :: a
    _fromInteger :: Integer -> a
    _negate :: a -> a

class Eq a => ActusNum a where
    (+) :: a -> a -> a
    (-) :: a -> a -> a
    (*) :: a -> a -> a
    (/) :: a -> a -> a

infixl 7  *, /
infixl 6  +, -

class YearFractionOps b where
    _y :: DCC -> LocalTime -> LocalTime -> Maybe LocalTime -> b

class ScheduleOps b where
    _ceiling :: b -> Integer

class (ActusNum a, ActusOps a) => RoleSignOps a where
    _r :: CR -> a
    _r CR_RPA = _one
    _r CR_RPL = _negate _one
    _r CR_CLO = _one
    _r CR_CNO = _one
    _r CR_COL = _one
    _r CR_LG  = _one
    _r CR_ST  = _negate _one
    _r CR_BUY = _one
    _r CR_SEL = _negate _one
    _r CR_RFL = _one
    _r CR_PFL = _negate _one
    _r CR_RF  = _one
    _r CR_PF  = _negate _one

-- == Default instance (Double)

setDefaultContractTermValues :: ContractTerms Double -> ContractTerms Double
setDefaultContractTermValues ct@ContractTerms {..} =
  ct
    { scheduleConfig =
        scheduleConfig
          { endOfMonthConvention = applyDefault EOMC_SD (endOfMonthConvention scheduleConfig),
            businessDayConvention = applyDefault BDC_NULL (businessDayConvention scheduleConfig),
            calendar = applyDefault CLDR_NC (calendar scheduleConfig)
          },
      contractPerformance            = applyDefault PRF_PF contractPerformance,
      interestCalculationBase        = applyDefault IPCB_NT interestCalculationBase,
      premiumDiscountAtIED           = applyDefault 0.0 premiumDiscountAtIED,
      scalingEffect                  = applyDefault SE_OOO scalingEffect,
      penaltyRate                    = applyDefault 0.0 penaltyRate,
      penaltyType                    = applyDefault PYTP_O penaltyType,
      prepaymentEffect               = applyDefault PPEF_N prepaymentEffect,
      rateSpread                     = applyDefault 0.0 rateSpread,
      rateMultiplier                 = applyDefault 1.0 rateMultiplier,
      feeAccrued                     = applyDefault 0.0 feeAccrued,
      feeRate                        = applyDefault 0.0 feeRate,
      accruedInterest                = applyDefault 0.0 accruedInterest,
      nominalInterestRate            = applyDefault 0.0 nominalInterestRate,
      priceAtPurchaseDate            = applyDefault 0.0 priceAtPurchaseDate,
      priceAtTerminationDate         = applyDefault 0.0 priceAtTerminationDate,
      scalingIndexAtContractDealDate = applyDefault 0.0 scalingIndexAtContractDealDate,
      periodFloor                    = applyDefault (- infinity) periodFloor,
      periodCap                      = applyDefault infinity periodCap,
      lifeCap                        = applyDefault infinity lifeCap,
      lifeFloor                      = applyDefault (- infinity) lifeFloor,
      interestCalculationBaseA       = applyDefault 0.0 interestCalculationBaseA,

      -- see ContractModel.java
      cycleAnchorDateOfInterestPayment = cycleAnchorDateOfInterestPayment <|> ((guard $ contractType == CLM) >> initialExchangeDate)
    }
  where
    infinity :: Double
    infinity = 1 Prelude./ 0 :: Double

    applyDefault :: a -> Maybe a -> Maybe a
    applyDefault v o = o <|> Just v


instance RoleSignOps Double

instance ActusOps Double where
    _min  = min
    _max  = max
    _abs  = abs
    _zero = 0.0
    _one  = 1.0
    _fromInteger = fromInteger
    _negate = negate

instance ActusNum Double where
    a + b       = a Prelude.+ b
    a - b       = a Prelude.- b
    a * b       = a Prelude.* b
    a / b       = a Prelude./ b

instance YearFractionOps Double where
    _y = yearFraction

instance ScheduleOps Double where
    _ceiling = ceiling
