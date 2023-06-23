module Contrib.Control.Concurrent.Async where

import Contrib.Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Data.Time.Units (TimeUnit)

-- | Race two `IO a` actions.
altIO :: IO a -> IO a -> IO a
altIO a b = either id id <$> race a b

timeoutIO :: (TimeUnit t) => t -> IO a -> IO (Maybe a)
timeoutIO timeout io = altIO (threadDelay timeout >> pure Nothing) (Just <$> io)
