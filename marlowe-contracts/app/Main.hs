{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Main
where

import qualified Language.Marlowe.Object.Archive as A
import qualified Language.Marlowe.Object.Types as O
import Marlowe.Contracts.UTC.Futures (future)

w1Pk, w2Pk :: O.Party
w1Pk = O.Role "party1"
w2Pk = O.Role "party2"

futureBundle :: O.ObjectBundle
futureBundle =
  future
    w1Pk
    w2Pk
    (O.Constant 80_000_000) -- 80 ADA
    (O.Constant 8_000_000) -- 8 ADA
    (read "2024-03-31 08:00:00.000000 UTC")
    [ read "2024-09-01 08:30:00.000000 UTC"
    , read "2024-09-02 08:30:00.000000 UTC"
    , read "2024-09-03 08:30:00.000000 UTC"
    , read "2024-09-04 08:30:00.000000 UTC"
    , read "2024-09-05 08:30:00.000000 UTC"
    , read "2024-09-06 08:30:00.000000 UTC"
    , read "2024-09-07 08:30:00.000000 UTC"
    , read "2024-09-08 08:30:00.000000 UTC"
    , read "2024-09-09 08:30:00.000000 UTC"
    , read "2024-09-10 08:30:00.000000 UTC"
    , read "2024-09-11 08:30:00.000000 UTC"
    , read "2024-09-12 08:30:00.000000 UTC"
    ] -- margin calls
    (read "2025-03-31 09:00:00.000000 UTC")

main :: IO ()
main = A.packArchive "futureBundle.zip" (O.Label "initialMarginDeposit") $ write futureBundle

write :: O.ObjectBundle -> (O.LabelledObject -> IO ()) -> IO ()
write bundle f = mapM_ f (O.getObjects bundle)
