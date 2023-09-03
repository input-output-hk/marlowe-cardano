module Contrib.Data.Time.Units where

import Data.Time (NominalDiffTime)
import Data.Time.Clock (secondsToNominalDiffTime)
import Data.Time.Units (TimeUnit (..))

toNominalDiffTime :: (TimeUnit a) => a -> NominalDiffTime
toNominalDiffTime a = do
  let us = toMicroseconds a
      us' = fromInteger us
  secondsToNominalDiffTime $ us' / 10 ^ (6 :: Integer)
