{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
module Actus.Domain.Schedule where

import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Time (LocalTime)
import GHC.Generics (Generic)

data ShiftedDay = ShiftedDay
  { paymentDay     :: LocalTime,
    calculationDay :: LocalTime
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

mkShiftedDay :: LocalTime -> ShiftedDay
mkShiftedDay d = ShiftedDay d d

type ShiftedSchedule = [ShiftedDay]
