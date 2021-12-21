{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Language.Marlowe.ACTUS.Utility.ScheduleGenerator
  ( generateRecurrentSchedule
  , (<+>)
  , (<->)
  , sup
  , inf
  )
where

import qualified Data.List                                   as L (delete, init, last, length)
import           Data.Time                                   (LocalTime (..))
import           Language.Marlowe.ACTUS.Domain.ContractTerms (Cycle (..), ScheduleConfig (..), Stub (..))
import           Language.Marlowe.ACTUS.Domain.Schedule      (ShiftedSchedule, mkShiftedDay)
import           Language.Marlowe.ACTUS.Utility.DateShift    (applyBDC, applyEOMC, shiftDate)

maximumMaybe :: Ord a => [a] -> Maybe a
maximumMaybe [] = Nothing
maximumMaybe xs = Just $ maximum xs

minimumMaybe :: Ord a => [a] -> Maybe a
minimumMaybe [] = Nothing
minimumMaybe xs = Just $ minimum xs

inf :: (Ord a) => [a] -> a -> Maybe a
inf set threshold =
  minimumMaybe [t | t <- set, t > threshold]

sup :: (Ord a) => [a] -> a -> Maybe a
sup set threshold =
  maximumMaybe [t | t <- set, t < threshold]

correction :: Cycle -> LocalTime -> LocalTime -> [LocalTime] -> [LocalTime]
correction
  Cycle
    { ..
    }
  anchorDate
  endDate
  schedule =
    let lastDate = L.last schedule
        schedule' = L.init schedule
        scheduleSize = L.length schedule'
        schedule'' =
          if not includeEndDay && endDate == anchorDate
            then L.delete anchorDate schedule'
            else schedule'
     in if stub == LongStub && L.length schedule'' > 2 && endDate /= lastDate
          then L.delete (schedule'' !! (scheduleSize - 1)) schedule''
          else schedule''

addEndDay :: Bool -> LocalTime -> ShiftedSchedule -> ShiftedSchedule
addEndDay True endDate schedule = schedule ++ [mkShiftedDay endDate]
addEndDay _ _ schedule          = schedule

generateRecurrentSchedule' :: Cycle -> LocalTime -> LocalTime -> [LocalTime]
generateRecurrentSchedule' Cycle {..} anchorDate endDate =
  let go :: LocalTime -> Integer -> [LocalTime] -> [LocalTime]
      go current k acc =
        if current >= endDate || n == 0
          then acc ++ [current]
          else
            let current' = shiftDate anchorDate (k * n) p
             in go current' (k + 1) (acc ++ [current])
   in go anchorDate 1 []

generateRecurrentSchedule :: LocalTime -> Cycle -> LocalTime -> ScheduleConfig -> ShiftedSchedule
generateRecurrentSchedule
  anchorDate
  cycle
  endDate
  ScheduleConfig
    { endOfMonthConvention = Just eomc,
      calendar = Just cal,
      businessDayConvention = Just bdc
    } =
    let s = generateRecurrentSchedule' cycle anchorDate endDate
        c = correction cycle anchorDate endDate s
     in addEndDay (includeEndDay cycle) endDate $ fmap (applyBDC bdc cal . applyEOMC anchorDate cycle eomc) c
generateRecurrentSchedule _ _ _ _ = []

(<+>) :: LocalTime -> Cycle -> LocalTime
(<+>) date cycle = shiftDate date (n cycle) (p cycle)

(<->) :: LocalTime -> Cycle -> LocalTime
(<->) date cycle = shiftDate date (-n cycle) (p cycle)
