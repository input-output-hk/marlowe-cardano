-- FIXME: Rename to DateTimeLocalInput
module Component.InstantInput where

import Prologue

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
import Debug (spy)
import Halogen as H
import Halogen.Css (classNames)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks

type Input =
  { classList :: Array String
  -- ^ Optional classes to style the component
  , value :: DateTime
  -- ^ Initial value
  , trimSeconds :: Boolean
  -- ^ Wether the component should use a component with seconds
  --   or discard them.
  }

type Output = DateTime

type Tokens q s = Hooks.ComponentTokens q s Output

-- FIXME: Rename to DateTimeLocalInput
instantInput :: forall a m. H.Component a Input Output m
instantInput =
  Hooks.component \(tokens :: Tokens _ _) input ->
    Hooks.do
      -- The state of our component is a String representation of the date, as it
      -- is used by the datetime-local component (e.g. '2018-06-12T19:30').
      -- This component does not know anything about the users TimeZone. Conversions
      -- are expected at the usage level.
      -- https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/datetime-local#value
      Tuple value valueId <- Hooks.useState $
        showNormalizedDateTime
          input.value
          input.trimSeconds
      -- For every render in which the value is different.
      Hooks.captures { value } Hooks.useTickEffect do
        let
          mParsed = parseInput value
        for_ mParsed $ Hooks.raise $ spy "instantInput outputToken"
          tokens.outputToken
        -- We don't need to free any resources
        pure Nothing
      Hooks.pure do
        HH.div
          [ classNames
              ( [ "bg-gray-light"
                , "flex"
                , "items-center"
                , "border-solid"
                , "border"
                , "rounded-sm"
                , "overflow-hidden"
                , "box-border"
                , "focus-within:ring-1"
                , "focus-within:ring-black"
                ]
                  <> input.classList
              )
          ]
          [ HH.input
              [ classNames
                  [ "flex-1"
                  , "px-1"
                  , "box-border"
                  , "self-stretch"
                  , "border-0"
                  , "outline-none"
                  ]
              , HE.onValueInput $ Hooks.put valueId
              , HP.type_ HP.InputDatetimeLocal
              , HP.value value
              ]
          ]
  where
  mDateTimeRegex = hush $ regex
    """(\d{4})-(\d{2})-(\d{2})T(\d{2}):(\d{2})(:(\d{2}))?"""
    noFlags

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
