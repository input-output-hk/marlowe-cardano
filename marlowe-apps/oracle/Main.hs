

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}


module Main
  ( main
  ) where


import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, readTChan)
import Control.Monad (forever, void)
import Data.String (fromString)
import Language.Marlowe.Core.V1.Semantics.Types (Party(Address))
import Language.Marlowe.Core.V1.Semantics.Types.Address (deserialiseAddress)
import Language.Marlowe.Runtime.App.Run
import Language.Marlowe.Runtime.App.Stream
import Language.Marlowe.Runtime.ChainSync.Api (Address(unAddress), fromBech32)
import Language.Marlowe.Runtime.Core.Api (MarloweVersionTag(V1))
import Network.HTTP.Client.TLS (newTlsManager)
import Network.Oracle (makeOracle, readOracle)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

import qualified Cardano.Api as C (AsType(AsPaymentExtendedKey, AsSigningKey), readFileTextEnvelope)
import qualified Data.Aeson as A (encode)
import qualified Data.ByteString.Lazy.Char8 as LBS8 (putStrLn)
import qualified Data.Text as T (pack)


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
    [configFile, _symbol, addressBech32, keyFile, contractId] <- getArgs
    config <- read <$> readFile configFile
    Just address <- pure . fromBech32 $ T.pack addressBech32
    Just _party <- pure $ uncurry Address <$> deserialiseAddress (unAddress address)
    Right _key <- C.AsSigningKey C.AsPaymentExtendedKey `C.readFileTextEnvelope` keyFile
    channel <- newTChanIO :: IO (TChan (ContractStream 'V1))
    void . forkIO
      . runClientWithConfig config
      $ streamAllContractSteps (fromString contractId) channel
    forever $ threadDelay 100000 >> atomically (readTChan channel) >>= LBS8.putStrLn . A.encode


mainOracle :: IO ()
mainOracle =
  do
    manager <- newTlsManager
    oracleEnv <- makeOracle manager
    print =<< mapM (readOracle oracleEnv . read) =<< getArgs
