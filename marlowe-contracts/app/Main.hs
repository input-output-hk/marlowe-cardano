{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Main
where

import Control.Monad.IO.Class (MonadIO (..))
import Language.Marlowe.Core.V1.Semantics.Types
import Language.Marlowe.Runtime.Client
import Marlowe.Contracts.UTC.Futures (future)

w1Pk, w2Pk :: Party
w1Pk = Role "party1"
w2Pk = Role "party2"

futureContract :: Contract
futureContract =
  future
    w1Pk
    w2Pk
    (Constant 80_000_000) -- 80 ADA
    (Constant 8_000_000) -- 8 ADA
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
main = connectToMarloweRuntime "localhost" 32856 do
  hashes <- loadContract futureContract
  _ <- liftIO $ print (show hashes)
  return ()
