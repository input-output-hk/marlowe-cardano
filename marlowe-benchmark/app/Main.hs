module Main (
  main,
) where

import Language.Marlowe.Runtime.Benchmark (measure)
import Language.Marlowe.Runtime.Client (connectToMarloweRuntime)

main :: IO ()
main =
  connectToMarloweRuntime "localhost" 13700 $
    measure
      3 -- HeaderSync parallelism
      100 -- maximum number of contracts
      3 -- Sync parallelism
      20 -- Sync batch size
