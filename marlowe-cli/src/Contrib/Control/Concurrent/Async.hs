module Contrib.Control.Concurrent.Async
  where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Data.Time.Units (Millisecond, TimeUnit(toMicroseconds))

-- | Race two `IO a` actions.
altIO :: IO a -> IO a -> IO a
altIO a b = either id id <$> race a b

timeoutIO :: Millisecond -> IO a -> IO (Maybe a)
timeoutIO timeout io = altIO (threadDelay (fromInteger . toMicroseconds $ timeout) >> pure Nothing) (Just <$> io)

