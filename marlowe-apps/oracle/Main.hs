

{-# LANGUAGE OverloadedStrings #-}


module Main
  ( main
  ) where


import Network.HTTP.Client.TLS (newTlsManager)
import Network.Oracle (makeOracle, readOracle)
import System.Environment (getArgs)

main :: IO ()
main =
  do
    manager <- newTlsManager
    oracleEnv <- makeOracle manager
    print =<< mapM (readOracle oracleEnv . read) =<< getArgs
