module Marlowe.Time where

import Prologue

import Data.DateTime (adjust)
import Data.DateTime.Instant (Instant, fromDateTime, instant, toDateTime)
import Data.Int as Int
import Data.Maybe (fromJust, fromMaybe)
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Partial.Unsafe (unsafePartial)

secondsSinceShelley :: Int -> Instant
secondsSinceShelley seconds = fromDateTime
  $ fromMaybe (if seconds < 0 then bottom else top)
  $ adjust (Seconds $ Int.toNumber seconds)
  $ toDateTime
  $ shelleyEpoch

shelleyEpoch :: Instant
shelleyEpoch =
  let
    -- 2020-07-29 21:44:51 UTC expressed as unix epoch
    epoch = Milliseconds 1596059091000.0
  in
    unsafePartial $ fromJust $ instant $ epoch

unixEpoch :: Instant
unixEpoch = unsafeInstantFromInt 0

unsafeInstantFromInt :: Int -> Instant
unsafeInstantFromInt ms =
  unsafePartial $ fromJust $ instant $ Milliseconds $ Int.toNumber ms
