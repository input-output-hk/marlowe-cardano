

{-# LANGUAGE NumericUnderscores #-}


module Main
  ( main
  ) where


import Language.Marlowe.Core.V1.Semantics.Types (Party(Address))
import Language.Marlowe.Core.V1.Semantics.Types.Address (deserialiseAddress)
import Language.Marlowe.Oracle.Process
import Language.Marlowe.Runtime.ChainSync.Api (Address(unAddress), fromBech32)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.Oracle (makeOracle)
import System.Environment (getArgs)

import qualified Cardano.Api as C (AsType(AsPaymentExtendedKey, AsSigningKey), readFileTextEnvelope)
import qualified Data.Text as T (pack)


main :: IO ()
main =
  do
    manager <- newTlsManager
    oracleEnv <- makeOracle manager
    let pollingFrequency = 5_000_000
    [configFile, addressBech32, keyFile] <- getArgs
    config <- read <$> readFile configFile
    Just address <- pure . fromBech32 $ T.pack addressBech32
    Just party <- pure $ uncurry Address <$> deserialiseAddress (unAddress address)
    Right key <- C.AsSigningKey C.AsPaymentExtendedKey `C.readFileTextEnvelope` keyFile
    discoveryChannel <- runDiscovery config pollingFrequency
    detectionChannel <- runDetection config pollingFrequency party discoveryChannel
    runOracle oracleEnv config (4 * pollingFrequency) address key party detectionChannel discoveryChannel
