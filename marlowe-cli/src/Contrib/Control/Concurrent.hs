module Contrib.Control.Concurrent where

import Control.Concurrent qualified as C
import Data.Time.Units (TimeUnit (toMicroseconds))

threadDelay :: (TimeUnit a) => a -> IO ()
threadDelay delay = C.threadDelay $ fromInteger $ toMicroseconds delay
