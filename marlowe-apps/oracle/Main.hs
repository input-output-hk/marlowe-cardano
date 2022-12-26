

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}


module Main
  ( main
  ) where


import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, readTChan)
import Control.Monad (forever, void)
import Language.Marlowe.Runtime.App.Run
import Language.Marlowe.Runtime.App.Stream
import Language.Marlowe.Runtime.Core.Api (MarloweVersionTag(V1))
import Network.HTTP.Client.TLS (newTlsManager)
import Network.Oracle (makeOracle, readOracle)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

import qualified Data.Aeson as A (encode)
import qualified Data.ByteString.Lazy.Char8 as LBS8 (putStrLn)


main :: IO ()
main =
  let
    mode = "steps" :: String
  in
    case mode of
      "oracle" -> mainOracle
      "ids"    -> mainStreamContractIds
      "steps"  -> mainStreamContractSteps
      _        -> pure ()


mainStreamContractIds :: IO ()
mainStreamContractIds =
  do
    config <- read <$> readFile "preprod.config"
    channel <- newTChanIO
    void . forkIO
      $ either (hPutStrLn stderr) pure
      =<< runClientWithConfig config (streamAllContractIds channel)
    forever $ threadDelay 100000 >> atomically (readTChan channel) >>= print


mainStreamContractSteps :: IO ()
mainStreamContractSteps =
  do
    config <- read <$> readFile "preprod.config"
    channel <- newTChanIO :: IO (TChan (ContractStream 'V1))
    void . forkIO
      . runClientWithConfig config
      $ streamAllContractSteps "11e07e45f099e47d8729efc13622256ef88b9201ab0ea24b7395f3b26129ccca#1" channel
    forever $ threadDelay 100000 >> atomically (readTChan channel) >>= LBS8.putStrLn . A.encode


mainOracle :: IO ()
mainOracle =
  do
    manager <- newTlsManager
    oracleEnv <- makeOracle manager
    print =<< mapM (readOracle oracleEnv . read) =<< getArgs
