module Main (
  main
) where


import           Language.Marlowe.CLI (mainCLI)
import           Paths_marlowe        (version)


main :: IO ()
main = mainCLI version
