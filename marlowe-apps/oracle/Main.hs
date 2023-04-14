{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}


module Main
  ( main
  ) where


import Language.Marlowe.Core.V1.Semantics.Types (Party(Address))
import Language.Marlowe.Core.V1.Semantics.Types.Address (deserialiseAddress)
import Language.Marlowe.Oracle.Process (runDetection, runOracle)
import Language.Marlowe.Runtime.App.Channel (RequeueFrequency(RequeueFrequency), runDiscovery')
import Language.Marlowe.Runtime.App.Parser (addressParser, getConfigParser)
import Language.Marlowe.Runtime.App.Types (Config, PollingFrequency(PollingFrequency))
import Language.Marlowe.Runtime.ChainSync.Api (Address(unAddress))
import Network.HTTP.Client.TLS (newTlsManager)
import Network.Oracle (makeOracle)
import Observe.Event.Render.JSON (DefaultRenderSelectorJSON(defaultRenderSelectorJSON))
import Observe.Event.Render.JSON.Handle (simpleJsonStderrBackend)

import qualified Cardano.Api as C (AsType(AsPaymentExtendedKey, AsSigningKey), readFileTextEnvelope)
import Data.Time.Units (Second)
import qualified Options.Applicative as O


main :: IO ()
main =
  do
    Command{..} <- O.execParser =<< commandParser
    let
      pollingFrequency' = PollingFrequency pollingFrequency
      requeueFrequency' = RequeueFrequency requeueFrequency
    key <-
      C.readFileTextEnvelope (C.AsSigningKey C.AsPaymentExtendedKey) keyFile
        >>= \case
          Right key'   -> pure key'
          Left message -> error $ show message
    Just party <- pure $ uncurry Address <$> deserialiseAddress (unAddress address)
    eventBackend <- simpleJsonStderrBackend defaultRenderSelectorJSON
    manager <- newTlsManager
    oracleEnv <- makeOracle manager
    discoveryChannel <- runDiscovery' eventBackend config pollingFrequency' False
    detectionChannel <- runDetection party eventBackend config pollingFrequency' discoveryChannel
    runOracle oracleEnv config address key party eventBackend requeueFrequency' False detectionChannel discoveryChannel


data Command =
  Command
  {
    config :: Config
  , pollingFrequency :: Second
  , requeueFrequency :: Second
  , address :: Address
  , keyFile :: FilePath
  }
    deriving (Show)


commandParser :: IO (O.ParserInfo Command)
commandParser =
  do
    configParser <- getConfigParser
    let
      commandOptions =
        Command
          <$> configParser
          <*> fmap fromInteger (O.option O.auto (O.long "polling" <> O.value 5 <> O.metavar "SECONDS" <> O.help "The polling frequency for waiting on Marlowe Runtime."))
          <*> fmap fromInteger (O.option O.auto (O.long "requeue" <> O.value 20 <> O.metavar "SECONDS" <> O.help "The requeuing frequency for reviewing the progress of contracts on Marlowe Runtime."))
          <*> O.argument addressParser (O.metavar "ADDRESS" <> O.help "The Bech32 address of the oracle.")
          <*> O.strArgument (O.metavar "KEYFILE" <> O.help "The extended payment signing key file for the oracle.")
    pure
      $ O.info
        (O.helper {- <*> O.versionOption -} <*> commandOptions)
        (
          O.fullDesc
            <> O.progDesc "This command-line tool watches the blockchain for Marlowe contracts to which it can contribute oracle input, and then it submits an oracle `IChoice` to the contract when it is ready for that input."
            <> O.header "marlowe-oracle : run an oracle for Marlowe contracts"
        )
