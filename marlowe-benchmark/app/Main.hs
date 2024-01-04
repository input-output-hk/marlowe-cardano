{-# LANGUAGE LambdaCase #-}

-- | Execute Benchmarks.
module Main (
  -- * Entry point
  main,
) where

import Data.Aeson (eitherDecodeFileStrict')
import Data.Default (def)
import Language.Marlowe.Runtime.Benchmark (measure)
import Language.Marlowe.Runtime.Client (connectToMarloweRuntime)
import System.Environment (getArgs)

-- | Execute the benchmarks.
main :: IO ()
main =
  do
    (host, port, config) <-
      getArgs >>= \case
        [] -> pure ("localhost", 3700, def)
        [h] -> pure (h, 3700, def)
        [h, p] -> pure (h, read p, def)
        [h, p, c] ->
          eitherDecodeFileStrict' c >>= \case
            Right c' -> pure (h, read p, c')
            Left e -> error e
        _ -> error "USAGE: marlowe-benchmark [host [port [configfile]]]"
    connectToMarloweRuntime host port $ measure config
