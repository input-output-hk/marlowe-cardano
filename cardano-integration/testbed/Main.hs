{-# LANGUAGE BlockArguments #-}

module Main
  where

import Control.Concurrent (threadDelay)
import Data.Functor (void)
import Test.Integration.Cardano (withLocalTestnet)

main :: IO ()
main = withLocalTestnet \testnet -> do
  putStrLn "Testnet started, press enter to exit"
  void getLine
