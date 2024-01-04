module Main (
  main,
) where

import Language.Marlowe.Runtime.Benchmark (BenchmarkConfig (..), measure)
import Language.Marlowe.Runtime.Client (connectToMarloweRuntime)

main :: IO ()
main =
  connectToMarloweRuntime "localhost" 13700
    . measure
    $ BenchmarkConfig
      { headerSyncParallelism = 3
      , maxContracts = 100
      , syncParallelism = 3
      , syncBatchSize = 20
      , queryParallelism = 3
      , queryBatchSize = 1
      , queryPageSize = 50
      }
