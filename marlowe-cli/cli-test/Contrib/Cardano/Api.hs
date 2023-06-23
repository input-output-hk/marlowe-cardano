module Contrib.Cardano.Api where

import Cardano.Api (Lovelace (Lovelace))

lovelaceToInt :: Lovelace -> Int
lovelaceToInt (Lovelace l) = fromInteger l

lovelaceFromInt :: Int -> Lovelace
lovelaceFromInt = Lovelace . toInteger
