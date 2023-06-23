module Contrib.Data.List.Random where

import System.Random

combinationWithRepetitions :: forall a. Int -> [a] -> IO [a]
combinationWithRepetitions len elems = do
  let draw :: StdGen -> ([a], StdGen)
      draw gen = do
        let indices = take len $ randomRs (0, length elems - 1) gen
            combination = map (elems !!) indices
            newGen = snd $ genWord32 gen
        (combination, newGen)
  gen <- getStdGen
  let (str, newGen) = draw gen
  setStdGen newGen
  return str
