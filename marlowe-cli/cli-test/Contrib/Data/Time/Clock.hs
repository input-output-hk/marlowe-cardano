module Contrib.Data.Time.Clock where

import Data.Time (NominalDiffTime)

nominalDiffTimeToMilliseconds :: NominalDiffTime -> Integer
nominalDiffTimeToMilliseconds ndt = round $ 1000 * ndt
