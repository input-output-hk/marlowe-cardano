module Contrib.UnliftIO.Control.Concurrent where

import Data.Time.Units (TimeUnit, toMicroseconds)
import UnliftIO (MonadIO)
import UnliftIO.Concurrent (threadDelay)

threadDelayBy :: (MonadIO m) => (TimeUnit a) => a -> m ()
threadDelayBy = threadDelay . fromInteger . toMicroseconds
