{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Marlowe.Contracts.UTC.CouponBond
  where

import Data.Time (Day, addDays)
import Data.Time.Calendar (addGregorianMonthsClip, addGregorianYearsClip)
import Data.Time.Clock (UTCTime(..))
import Language.Marlowe.Extended.V1
import Marlowe.Contracts.Common
import Marlowe.Contracts.UTC.Common
import Prelude hiding (cycle)

-- |A coupon bond is a debt security that periodically pays interest.
couponBond ::
     Party    -- ^ Investor
  -> Party    -- ^ Issuer
  -> UTCTime  -- ^ Initial Fixing
  -> UTCTime  -- ^ Maturity
  -> Cycle    -- ^ Cycle
  -> Value    -- ^ Discounted value
  -> Value    -- ^ Coupon
  -> Value    -- ^ Face value
  -> Token    -- ^ Token
  -> Contract -- ^ Continuation
  -> Contract -- ^ Coupon Bond Contract
couponBond investor issuer fixing maturity cycle discounted coupon face token continuation =
  transfer investor issuer (token, discounted) (toTimeout fixing) Close $
    foldr
      (\timeout -> transfer issuer investor (token, coupon) (toTimeout timeout) Close)
      (transfer issuer investor (token, face) (toTimeout maturity) Close continuation)
      schedule
  where
    schedule :: [UTCTime]
    schedule = tail $ generateSchedule cycle fixing maturity

-- == Time handling

generateSchedule :: Cycle -> UTCTime -> UTCTime -> [UTCTime]
generateSchedule Cycle {..} anchorDate endDate =
  let go :: UTCTime -> Integer -> [UTCTime] -> [UTCTime]
      go current k acc =
        if current >= endDate || n == 0
          then acc ++ [current]
          else
            let current' = shiftDate anchorDate (k * n) period
             in go current' (k + 1) (acc ++ [current])
   in go anchorDate 1 []

shiftDate :: UTCTime -> Integer -> Period -> UTCTime
shiftDate UTCTime {..} n p =
  UTCTime
    { utctDay = shift n p utctDay,
      utctDayTime = utctDayTime
    }
  where
    shift :: Integer -> Period -> Day -> Day
    shift m Day      = addDays m
    shift m Week     = addDays (m * 7)
    shift m Month    = addGregorianMonthsClip m
    shift m Quarter  = addGregorianMonthsClip (m * 3)
    shift m HalfYear = addGregorianMonthsClip (m * 6)
    shift m Year     = addGregorianYearsClip m

data Cycle = Cycle
  { n       :: Integer,
    period  :: Period,
    include :: Bool
  }

data Period
  = Day
  | Week
  | Month
  | Quarter
  | HalfYear
  | Year
