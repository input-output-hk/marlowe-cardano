{-# LANGUAGE BlockArguments #-}

module Main
  where

import Control.Concurrent (threadDelay)
import Data.Functor (void)
import Test.Integration.Cardano (withLocalTestnet)
import Test.Integration.Marlowe (withLocalMarloweRuntime)

main :: IO ()
main = withLocalTestnet $ withLocalMarloweRuntime \runtime -> do
  putStrLn "Local Marlowe Runtime started, press enter to exit"
  void getLine
