{-# LANGUAGE RecordWildCards #-}

module Language.Marlowe.ACTUS.Utility.DateShift
  ( applyBDC
  , applyEOMC
  , applyBDCWithCfg
  , getPreceedingBusinessDay
  , getFollowingBusinessDay
  , moveToEndOfMonth
  , shiftDate
  )
where

import           Data.Time                                   (LocalTime (..), addDays, toGregorian)
import           Data.Time.Calendar                          (addGregorianMonthsClip, addGregorianYearsClip,
                                                              fromGregorian, gregorianMonthLength)
import           Data.Time.Calendar.WeekDate                 (toWeekDate)
import           Language.Marlowe.ACTUS.Domain.ContractTerms (BDC (..), Calendar (..), Cycle (..), EOMC (..),
                                                              Period (..), ScheduleConfig (..))
import           Language.Marlowe.ACTUS.Domain.Schedule      (ShiftedDay (..))

{- Business Day Convention -}

applyBDCWithCfg :: ScheduleConfig -> LocalTime -> ShiftedDay
applyBDCWithCfg
  ScheduleConfig
    { businessDayConvention = Just bdc,
      calendar = Just cal
    }
  d = applyBDC bdc cal d
applyBDCWithCfg _ date = ShiftedDay {paymentDay = date, calculationDay = date}

applyBDC :: BDC -> Calendar -> LocalTime -> ShiftedDay
applyBDC BDC_NULL _ date =
  ShiftedDay { paymentDay = date, calculationDay = date }

applyBDC BDC_SCF cal date = ShiftedDay
  { paymentDay     = getFollowingBusinessDay date cal
  , calculationDay = getFollowingBusinessDay date cal
  }

applyBDC BDC_SCMF cal date = ShiftedDay
  { paymentDay     = shiftModifiedFollowing date cal
  , calculationDay = shiftModifiedFollowing date cal
  }

applyBDC BDC_CSF cal date = ShiftedDay
  { paymentDay     = getFollowingBusinessDay date cal
  , calculationDay = date
  }

applyBDC BDC_CSMF cal date = ShiftedDay
  { paymentDay     = shiftModifiedFollowing date cal
  , calculationDay = date
  }

applyBDC BDC_SCP cal date = ShiftedDay
  { paymentDay     = getPreceedingBusinessDay date cal
  , calculationDay = getPreceedingBusinessDay date cal
  }

applyBDC BDC_SCMP cal date = ShiftedDay
  { paymentDay     = shiftModifiedPreceeding date cal
  , calculationDay = shiftModifiedPreceeding date cal
  }

applyBDC BDC_CSP cal date = ShiftedDay
  { paymentDay     = getPreceedingBusinessDay date cal
  , calculationDay = date
  }

applyBDC BDC_CSMP cal date = ShiftedDay
  { paymentDay     = shiftModifiedPreceeding date cal
  , calculationDay = date
  }

shiftModifiedFollowing :: LocalTime -> Calendar -> LocalTime
shiftModifiedFollowing lt@LocalTime {..} cal =
  let (_, month, _) = toGregorian localDay
      st@LocalTime {localDay = stLocalDay} = getFollowingBusinessDay lt cal
      (_, shiftedMonth, _) = toGregorian stLocalDay
   in if month == shiftedMonth then st else getPreceedingBusinessDay lt cal

shiftModifiedPreceeding :: LocalTime -> Calendar -> LocalTime
shiftModifiedPreceeding lt@LocalTime {..} cal =
  let (_, month, _) = toGregorian localDay
      st@LocalTime {localDay = stLocalDay} = getPreceedingBusinessDay lt cal
      (_, shiftedMonth, _) = toGregorian stLocalDay
   in if month == shiftedMonth then st else getFollowingBusinessDay lt cal

getFollowingBusinessDay :: LocalTime -> Calendar -> LocalTime
getFollowingBusinessDay LocalTime {..} CLDR_MF =
  let day = case toWeekDate localDay of
        (_, _, 6) -> addDays 2 localDay
        (_, _, 7) -> addDays 1 localDay
        _         -> localDay
   in LocalTime {localDay = day, localTimeOfDay = localTimeOfDay}
getFollowingBusinessDay lt _ = lt

getPreceedingBusinessDay :: LocalTime -> Calendar -> LocalTime
getPreceedingBusinessDay LocalTime {..} CLDR_MF =
  let day = case toWeekDate localDay of
        (_, _, 6) -> addDays (-1) localDay
        (_, _, 7) -> addDays (-2) localDay
        _         -> localDay
   in LocalTime {localDay = day, localTimeOfDay = localTimeOfDay}
getPreceedingBusinessDay lt _ = lt

shiftDate :: LocalTime -> Integer -> Period -> LocalTime
shiftDate LocalTime {..} n p = case p of
  P_D -> LocalTime {localDay = addDays n localDay, localTimeOfDay = localTimeOfDay}
  P_W -> LocalTime {localDay = addDays (n * 7) localDay, localTimeOfDay = localTimeOfDay}
  P_M -> LocalTime {localDay = addGregorianMonthsClip n localDay, localTimeOfDay = localTimeOfDay}
  P_Q -> LocalTime {localDay = addGregorianMonthsClip (n * 3) localDay, localTimeOfDay = localTimeOfDay}
  P_H -> LocalTime {localDay = addGregorianMonthsClip (n * 6) localDay, localTimeOfDay = localTimeOfDay}
  P_Y -> LocalTime {localDay = addGregorianYearsClip n localDay, localTimeOfDay = localTimeOfDay}

{- End of Month Convention -}
applyEOMC :: LocalTime -> Cycle -> EOMC -> LocalTime -> LocalTime
applyEOMC s Cycle {..} endOfMonthConvention date
  | isLastDayOfMonthWithLessThan31Days s
    && p /= P_D
    && p /= P_W
    && endOfMonthConvention == EOMC_EOM
  = moveToEndOfMonth date
  | otherwise
  = date

isLastDayOfMonthWithLessThan31Days :: LocalTime -> Bool
isLastDayOfMonthWithLessThan31Days LocalTime {..} =
  let (year, month, day) = toGregorian localDay
      isLastDay = gregorianMonthLength year month == day
   in day < 31 && isLastDay

moveToEndOfMonth :: LocalTime -> LocalTime
moveToEndOfMonth LocalTime {..} =
  let (year, month, _) = toGregorian localDay
      monthLength = gregorianMonthLength year month
   in LocalTime
        { localDay = fromGregorian year month monthLength,
          localTimeOfDay = localTimeOfDay
        }
