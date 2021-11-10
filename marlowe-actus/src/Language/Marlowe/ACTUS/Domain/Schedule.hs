{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
module Language.Marlowe.ACTUS.Domain.Schedule where

import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Time (LocalTime)
import GHC.Generics (Generic)
import Language.Marlowe.ACTUS.Domain.BusinessEvents (EventType)

data ShiftedDay = ShiftedDay
  { paymentDay     :: LocalTime,
    calculationDay :: LocalTime
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

type ShiftedSchedule = [ShiftedDay]

data CashFlow = CashFlow
  { tick               :: Integer,
    cashContractId     :: String,
    cashParty          :: String,
    cashCounterParty   :: String,
    cashPaymentDay     :: LocalTime,
    cashCalculationDay :: LocalTime,
    cashEvent          :: EventType,
    amount             :: Double,
    notional           :: Double,
    currency           :: String
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
