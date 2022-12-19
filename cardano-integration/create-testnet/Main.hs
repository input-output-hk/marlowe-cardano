{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module Main
  where

import Control.Concurrent (threadDelay)
import Data.Functor (void)
import Test.Integration.Cardano (LocalTestnet(..), withLocalTestnet)
import Test.Integration.Workspace (Workspace(workspaceDir))

main :: IO ()
main = withLocalTestnet \LocalTestnet{..} -> do
  putStrLn $ "Workspace dir: " <> workspaceDir workspace
  putStrLn $ "Testnet magic: " <> show testnetMagic
  putStrLn "Testnet started, press enter to exit"
  void getLine
