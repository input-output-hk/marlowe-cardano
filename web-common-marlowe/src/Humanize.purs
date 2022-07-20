module Humanize
  ( utcToLocal
  , localToUtc
  , humanizeDuration
  , formatDate
  , formatDate'
  , formatTime
  , formatTime'
  , formatPOSIXTime
  , humanizeInterval
  , humanizeValue
  , humanizeOffset
  ) where

import Prologue

import Data.BigInt.Argonaut (BigInt)
import Data.BigInt.Argonaut as BigInt
import Data.DateTime (DateTime, adjust)
import Data.DateTime.Instant (toDateTime)
import Data.Formatter.DateTime (FormatterCommand(..), format) as DateTime
import Data.Formatter.Number (Formatter(..), format) as Number
import Data.Int (floor, round)
import Data.Int (toNumber) as Int
import Data.List as List
import Data.Maybe (fromMaybe)
import Data.Newtype (over, unwrap)
import Data.Ord (abs)
import Data.Time.Duration (Minutes(..), Seconds(..))
import Data.Tuple.Nested ((/\))
import Language.Marlowe.Core.V1.Semantics.Types (TimeInterval(..), Token(..))
import Plutus.V1.Ledger.Time (POSIXTime)
import Plutus.V1.Ledger.Time as POSIXTime

humanizeDuration :: Seconds -> String
humanizeDuration (Seconds seconds)
  | seconds <= 0.0 = "Timed out"
  | seconds <= 60.0 = show (floor seconds) <> "sec left"
  | seconds <= (60.0 * 60.0) =
      let
        min = floor (seconds / 60.0)

        sec = floor $ seconds - (Int.toNumber min * 60.0)
      in
        show min <> "min " <> show sec <> "s left"
  | seconds < (60.0 * 60.0 * 24.0) =
      let
        hours = floor (seconds / 60.0 / 60.0)

        min = round $ (seconds - (Int.toNumber hours * 60.0 * 60.0)) / 60.0
      in
        show hours <> "hr " <> show min <> "m left"
  | otherwise =
      let
        days = floor (seconds / 60.0 / 60.0 / 24.0)
      in
        if (days == 1) then
          "1 day left"
        else
          show days <> "days left"

humanizeInterval :: Minutes -> TimeInterval -> String
humanizeInterval tzOffset (TimeInterval from to) = humanize
  (formatPOSIXTime tzOffset from)
  (formatPOSIXTime tzOffset to)
  where
  humanize ((fromDate /\ fromTime)) ((toDate /\ toTime))
    | fromDate == toDate && fromTime == toTime = "on " <> fromDate <> " at " <>
        fromTime
    | fromDate == toDate = "on " <> fromDate <> " between " <> fromTime
        <> " and "
        <> toTime
    | otherwise = "between " <> fromDate <> " " <> fromTime <> " and " <> toDate
        <> " "
        <> toTime

formatPOSIXTime :: Minutes -> POSIXTime -> (Tuple String String)
formatPOSIXTime tzOffset time =
  let
    localDateTime = fromMaybe (toDateTime $ unwrap time)
      $ POSIXTime.toLocalDateTime tzOffset time
  in
    formatDate' localDateTime /\ formatTime' localDateTime

-- Adjusts a DateTime via an offset (that can be obtained using timezoneOffset)
-- The `adjust` function can overflow, if that happens (it shouldn't) we resolve to
-- the original value
-- TODO: SCP-3833 Add type safety to timezone conversions
utcToLocal :: Minutes -> DateTime -> DateTime
utcToLocal tzOffset dt =
  fromMaybe dt (adjust (over Minutes negate tzOffset :: Minutes) dt)

localToUtc :: Minutes -> DateTime -> DateTime
localToUtc tzOffset dt =
  fromMaybe dt (adjust tzOffset dt)

minutesFormatter :: Number.Formatter
minutesFormatter =
  Number.Formatter
    { sign: false
    , before: 2
    , comma: false
    , after: 0
    , abbreviations: false
    }

formatMinutes :: Int -> String
formatMinutes = Number.format minutesFormatter <<< Int.toNumber

-- This functions provides a human readable string for the Browser offset,
-- which can be obtained using Effect.Now (getTimezoneOffset)
humanizeOffset :: Minutes -> String
humanizeOffset (Minutes 0.0) = "GMT"

humanizeOffset (Minutes min) = "GMT" <> (if min > zero then "-" else "+") <>
  offsetString
  where
  min' = floor $ abs min

  hours = min' / 60

  offsetString =
    if min' `mod` 60 == zero then
      show hours
    else
      show hours <> ":" <> formatMinutes (min' - hours * 60)

formatDate :: Minutes -> DateTime -> String
formatDate tzOffset dt = formatDate' $ utcToLocal tzOffset dt

formatDate' :: DateTime -> String
formatDate' =
  DateTime.format
    $ List.fromFoldable
        [ DateTime.DayOfMonth
        , DateTime.Placeholder " "
        , DateTime.MonthShort
        , DateTime.Placeholder " "
        , DateTime.YearFull
        ]

formatTime :: Minutes -> DateTime -> String
formatTime tzOffset dt = formatTime' $ utcToLocal tzOffset dt

formatTime' :: DateTime -> String
formatTime' =
  DateTime.format
    $ List.fromFoldable
        [ DateTime.Hours24
        , DateTime.Placeholder ":"
        , DateTime.MinutesTwoDigits
        ]

oneMillion :: BigInt
oneMillion = BigInt.fromInt 1_000_000

humanizeValue :: Token -> BigInt -> String
-- TODO: use a different currencyFormatter with no decimal places when they're all zero
humanizeValue (Token "" "") value
  | value `mod` oneMillion == zero =
      "₳ " <> Number.format (numberFormatter 0) (toAda value)
humanizeValue (Token "" "") value =
  "₳ " <> Number.format (numberFormatter 6) (toAda value)
humanizeValue (Token "" "dollar") value = "$ " <> Number.format
  (numberFormatter 2)
  (BigInt.toNumber value / 100.0)

humanizeValue (Token _ name) value =
  Number.format (numberFormatter 0) (BigInt.toNumber value) <> " " <> name

toAda :: BigInt -> Number
toAda lovelace = (BigInt.toNumber lovelace) / 1_000_000.0

numberFormatter :: Int -> Number.Formatter
numberFormatter decimals =
  Number.Formatter
    { sign: false
    , before: 0
    , comma: true
    , after: decimals
    , abbreviations: false
    }
