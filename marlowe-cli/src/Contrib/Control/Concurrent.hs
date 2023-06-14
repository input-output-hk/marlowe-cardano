module Contrib.Control.Concurrent where

import qualified Control.Concurrent as C
import Data.Time.Units (TimeUnit(toMicroseconds))

threadDelay :: TimeUnit a => a -> IO ()
threadDelay delay = C.threadDelay $ fromInteger $ toMicroseconds delay
