{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Main
where

import Control.Monad.IO.Class (MonadIO (..))
import qualified Language.Marlowe.Core.V1.Semantics.Types as Core
import qualified Language.Marlowe.Extended.V1 as Extended
import Language.Marlowe.Runtime.Client
import Marlowe.Contracts.UTC.Futures (future)

w1Pk, w2Pk :: Extended.Party
w1Pk = Extended.Role "party1"
w2Pk = Extended.Role "party2"

futContract :: Extended.Contract
futContract =
  future
    w1Pk
    w2Pk
    (Extended.Constant 80_000_000) -- 80 ADA
    (Extended.Constant 8_000_000) -- 8 ADA
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

futCore :: Maybe Core.Contract
futCore = Extended.toCore futContract

main :: IO ()
main = connectToMarloweRuntime "localhost" 32844 do
  futureContract <- liftIO $ case futCore of
    Nothing -> ioError $ userError "error generating contract"
    Just c -> return c
  hashes <- loadContract futureContract
  _ <- liftIO $ print (show hashes)
  return ()
