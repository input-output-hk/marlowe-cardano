

{-# LANGUAGE OverloadedStrings #-}


module Main
  ( main
  ) where


import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (newTChanIO, readTChan)
import Control.Monad (forever, void)
import Language.Marlowe.Runtime.App.Run
import Language.Marlowe.Runtime.App.Stream
import Network.HTTP.Client.TLS (newTlsManager)
import Network.Oracle (makeOracle, readOracle)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)


main :: IO ()
main =
  let
    mode = "stream" :: String
  in
    case mode of
      "oracle" -> mainOracle
      "stream" -> mainStream
      _        -> pure ()


mainStream :: IO ()
mainStream =
  do
    config <- read <$> readFile "preprod.config"
    channel <- newTChanIO
    void . forkIO
      $ either (hPutStrLn stderr) pure
      =<< runClientWithConfig config (streamAllContracts channel)
    forever $ threadDelay 100000 >> atomically (readTChan channel) >>= print


mainOracle :: IO ()
mainOracle =
  do
    manager <- newTlsManager
    oracleEnv <- makeOracle manager
    print =<< mapM (readOracle oracleEnv . read) =<< getArgs
