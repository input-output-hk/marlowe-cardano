module Marlowe.Contracts.UTC.Common where

import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Language.Marlowe (POSIXTime (..))
import Language.Marlowe.Core.V1.Semantics.Types

-- | Convert UTCTime to Timeout
toTimeout :: UTCTime -> Timeout
toTimeout = POSIXTime . (* 1000) . floor . utcTimeToPOSIXSeconds
