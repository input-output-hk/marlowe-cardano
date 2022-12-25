



module Main
  ( main
  ) where


import Sofr


main :: IO ()
main =
  do
    env <- nyfrbEnv
    rate <- fetchSofrBasisPoints env
    print rate
