module Marlowe.Contracts.UTC.Common
  where

import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Language.Marlowe.Extended.V1

-- |Convert UTCTime to Timeout
toTimeout :: UTCTime -> Timeout
toTimeout = POSIXTime . floor . utcTimeToPOSIXSeconds
