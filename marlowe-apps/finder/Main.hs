{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Main
  ( main
  ) where


import Data.Text (Text)
import Language.Marlowe.Runtime.App.Parser (getConfigParser)
import Language.Marlowe.Runtime.App.Stream (ContractStream(..), TChanEOF)
import Language.Marlowe.Runtime.App.Types (Config)
import Language.Marlowe.Runtime.Core.Api (ContractId, MarloweVersionTag(V1))
import Observe.Event (EventBackend, addField)
import Observe.Event.Dynamic (DynamicEventSelector(..))
import Observe.Event.Render.JSON (DefaultRenderSelectorJSON(defaultRenderSelectorJSON))
import Observe.Event.Render.JSON.Handle (simpleJsonStderrBackend)
import Observe.Event.Syntax ((≔))

import qualified Language.Marlowe.Runtime.App.Channel as App
  (LastSeen(..), runContractAction, runDetection, runDiscovery')
import qualified Options.Applicative as O


runDetection
  :: EventBackend IO r DynamicEventSelector
  -> Config
  -> Int
  -> TChanEOF ContractId
  -> IO (TChanEOF (ContractStream 'V1))
runDetection = App.runDetection $ const True


runFinder
  :: EventBackend IO r DynamicEventSelector
  -> Int
  -> Bool
  -> TChanEOF (ContractStream 'V1)
  -> TChanEOF ContractId
  -> IO ()
runFinder eventBackend =
  App.runContractAction "FinderProcess" eventBackend
    $ \event App.LastSeen{..} ->
      addField event $ ("transactionId" :: Text) ≔ lastTxId


main :: IO ()
main =
  do
    Command{..} <- O.execParser =<< commandParser
    eventBackend <- simpleJsonStderrBackend defaultRenderSelectorJSON
    discoveryChannel <- App.runDiscovery' eventBackend config pollingFrequency endOnWait
    detectionChannel <- runDetection eventBackend config pollingFrequency discoveryChannel
    runFinder eventBackend requeueFrequency endOnWait detectionChannel discoveryChannel


data Command =
  Command
  {
    config :: Config
  , pollingFrequency :: Int
  , requeueFrequency :: Int
  , endOnWait :: Bool
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
          <*> O.option O.auto (O.long "polling" <> O.value 5_000_000 <> O.metavar "SECONDS" <> O.help "The polling frequency for waiting on Marlowe Runtime.")
          <*> O.option O.auto (O.long "requeue" <> O.value 20_000_000 <> O.metavar "SECONDS" <> O.help "The requeuing frequency for reviewing the progress of contracts on Marlowe Runtime.")
          <*> O.flag False True (O.long "end-at-tip" <> O.help "Stop the process when the tip of all contracts has been reached.")
    pure
      $ O.info
        (O.helper {- <*> O.versionOption -} <*> commandOptions)
        (
          O.fullDesc
            <> O.progDesc "This command-line tool watches the blockchain for Marlowe contracts for active Marlowe contracts."
            <> O.header "marlowe-finder : find active Marlowe contracts"
        )
