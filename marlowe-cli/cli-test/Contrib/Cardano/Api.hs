module Contrib.Cardano.Api (lovelaceToInt, lovelaceFromInt) where

import Cardano.Api.Ledger (Coin (Coin))

lovelaceToInt :: Coin -> Int
lovelaceToInt (Coin l) = fromInteger l

lovelaceFromInt :: Int -> Coin
lovelaceFromInt = Coin . toInteger
