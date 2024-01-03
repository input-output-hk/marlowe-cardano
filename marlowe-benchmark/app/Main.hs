module Main (
  main,
) where

import Language.Marlowe.Runtime.Benchmark (measure)
import Language.Marlowe.Runtime.Client (connectToMarloweRuntime)

main :: IO ()
main = connectToMarloweRuntime "localhost" 13700 measure
