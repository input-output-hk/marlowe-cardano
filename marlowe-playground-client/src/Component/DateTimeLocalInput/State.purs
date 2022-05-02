module Component.DateTimeLocalInput.State
  ( component
  ) where

import Prologue

import Component.DateTimeLocalInput.Types
  ( Action(..)
  , Component
  , DSL
  , Message(..)
  )
import Component.DateTimeLocalInput.View (render)
import Data.Array (intercalate)
import Data.Array.NonEmpty (toArray)
import Data.Compactable (compact)
import Data.Date (exactDate)
import Data.Date as Date
import Data.DateTime (DateTime(..))
import Data.DateTime as DateTime
import Data.Either (hush)
import Data.Enum (fromEnum, toEnum)
import Data.Foldable (for_)
import Data.Int as Int
import Data.Maybe (fromMaybe)
import Data.String (length, take)
import Data.String.Regex (match, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Time (Time(..))
import Data.Time as Time
import Halogen as H

component
  :: forall query m
   . Component query m
component = H.mkComponent
  { initialState: deriveState
  , render
  , eval:
      H.mkEval
        ( H.defaultEval
            { handleAction = handleAction
            }
        )
  }
  where
  deriveState input =
    { value: showNormalizedDateTime input.value input.trimSeconds
    , classList: input.classList
    , trimSeconds: input.trimSeconds
    }

handleAction
  :: forall m
   . Action
  -> DSL m Unit
handleAction (ChangeValue newValue) = do
  let
    mParsed = parseInput newValue
  for_ mParsed \parsedValue -> do
    -- H.modify_ (_ { value = parsedValue })
    H.raise $ ValueChanged parsedValue

parseInput :: String -> Maybe DateTime
parseInput value = do
  dateTimeRegex <- mDateTimeRegex
  found <- match dateTimeRegex value
  case compact $ toArray found of
    [ _, year, month, day, hour, minute, second ] -> buildOutput
      year
      month
      day
      hour
      minute
      second
    [ _, year, month, day, hour, minute ] -> buildOutput
      year
      month
      day
      hour
      minute
      ":00"
    _ -> Nothing
  where
  mDateTimeRegex = hush $ regex
    """(\d{4})-(\d{2})-(\d{2})T(\d{2}):(\d{2})(:(\d{2}))?"""
    noFlags

  buildOutput year month day hour minute second =
    do
      year' <- toEnum =<< Int.fromString year
      month' <- toEnum =<< Int.fromString month
      day' <- toEnum =<< Int.fromString day
      date <- exactDate year' month' day'
      hour' <- toEnum =<< Int.fromString hour
      minute' <- toEnum =<< Int.fromString minute
      -- The first character in the second array is ":" because
      -- of how the regexp grouping works. If the matching is empty we
      -- use 0 seconds
      second' <- toEnum $ fromMaybe 0 $ Int.fromString $ take 1 second
      -- The DateTime local does not handle milliseconds.
      ms' <- toEnum 0
      let
        time = Time hour' minute' second' ms'
      pure $ DateTime date time

pad2 :: Int -> String
pad2 n =
  let
    str = show n
  in
    if length str == 1 then "0" <> str else str

-- The datetime-local component uses a normalized string, which can work with or without
-- seconds.
-- YYYY-MM-DDThh:mm(:ss)
-- https://developer.mozilla.org/en-US/docs/Web/HTML/Date_and_time_formats#local_date_and_time_strings
showNormalizedDateTime :: DateTime -> Boolean -> String
showNormalizedDateTime value trimSeconds =
  let
    date = DateTime.date value
    year = fromEnum $ Date.year date
    month = fromEnum $ Date.month date
    day = fromEnum $ Date.day date
    dateStr = intercalate "-" [ show year, pad2 month, pad2 day ]
    time = DateTime.time value
    hour = fromEnum $ Time.hour time
    minute = fromEnum $ Time.minute time
    seconds = fromEnum $ Time.second time
    timeStr = intercalate ":" $ compact
      [ Just (pad2 hour)
      , Just (pad2 minute)
      , if trimSeconds then Nothing else Just (pad2 seconds)
      ]
  in
    dateStr <> "T" <> timeStr
