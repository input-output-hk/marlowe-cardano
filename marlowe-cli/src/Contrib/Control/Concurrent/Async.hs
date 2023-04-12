module Contrib.Control.Concurrent.Async
  where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Data.Time.Units (Microsecond)

-- | Race two `IO a` actions.
altIO :: IO a -> IO a -> IO a
altIO a b = either id id <$> race a b

timeoutIO :: Microsecond -> IO a -> IO (Maybe a)
timeoutIO timeout io = altIO (threadDelay (fromIntegral timeout) >> pure Nothing) (Just <$> io)

