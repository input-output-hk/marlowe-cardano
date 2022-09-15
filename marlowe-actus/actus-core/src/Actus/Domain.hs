{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Actus.Domain
  ( module Actus.Domain.BusinessEvents
  , module Actus.Domain.ContractState
  , module Actus.Domain.ContractTerms
  , module Actus.Domain.Schedule
  , ActusFrac(..)
  , ActusOps(..)
  , CashFlow(..)
  , RiskFactors(..)
  , setDefaultContractTermValues
  , sign
  ) where

import Actus.Domain.BusinessEvents
import Actus.Domain.ContractState (ContractState(..))
import Actus.Domain.ContractTerms
import Actus.Domain.Schedule
import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Time (LocalTime)
import GHC.Generics (Generic)

class (Fractional a, ActusOps a) => ActusFrac a where
  _ceiling :: a -> Integer

class ActusOps a where
  _min :: a -> a -> a
  _max :: a -> a -> a

instance ActusOps Double where
  _min = min
  _max = max

instance ActusFrac Double where
  _ceiling = ceiling

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

sign :: Num a => CR -> a
sign CR_RPA = 1
sign CR_RPL = negate 1
sign CR_CLO = 1
sign CR_CNO = 1
sign CR_COL = 1
sign CR_LG  = 1
sign CR_ST  = negate 1
sign CR_BUY = 1
sign CR_SEL = negate 1
sign CR_RFL = 1
sign CR_PFL = negate 1
sign CR_RF  = 1
sign CR_PF  = negate 1

-- == Default instance (Double)

applyDefaults :: ContractStructure Double -> ContractStructure Double
applyDefaults cs = case reference cs of
  ReferenceTerms rt -> cs { reference = ReferenceTerms $ setDefaultContractTermValues rt }
  ReferenceId _     -> cs

setDefaultContractTermValues :: ContractTerms Double -> ContractTerms Double
setDefaultContractTermValues ct@ContractTerms {..} =
  ct
    { scheduleConfig =
        scheduleConfig
          { endOfMonthConvention = applyDefault EOMC_SD (endOfMonthConvention scheduleConfig),
            businessDayConvention = applyDefault BDC_NULL (businessDayConvention scheduleConfig),
            calendar = applyDefault CLDR_NC (calendar scheduleConfig)
          },
      contractStructure              = map applyDefaults contractStructure,
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
    infinity = 1 / 0 :: Double

    applyDefault :: a -> Maybe a -> Maybe a
    applyDefault v o = o <|> Just v
