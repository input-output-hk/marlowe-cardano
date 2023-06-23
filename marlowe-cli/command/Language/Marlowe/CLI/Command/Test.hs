-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Contract-testing commands in the Marlowe CLI tool.
--
-----------------------------------------------------------------------------


{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}


module Language.Marlowe.CLI.Command.Test
  ( -- * Marlowe CLI Commands
    TestCommand
  , mkParseTestCommand
  , runTestCommand
  ) where

import Cardano.Api (IsShelleyBasedEra, NetworkId)
import Control.Monad.Except (MonadError, MonadIO)
import Data.Maybe (fromMaybe)
import Language.Marlowe.CLI.Command.Parse (parseAddress, parseNetworkId)
import Language.Marlowe.CLI.Test (runTestSuite)
import Language.Marlowe.CLI.Test.ExecutionMode (ExecutionMode(OnChainMode, SimulationMode))
import Language.Marlowe.CLI.Test.Types
  (ConcurrentRunners(ConcurrentRunners), ReportingStrategy(..), RuntimeConfig(RuntimeConfig), TestSuite(TestSuite))
import Language.Marlowe.CLI.Types (CliEnv, CliError, askEra)

import Control.Monad.Reader.Class (MonadReader)
import Data.Time.Units (Second)
import Language.Marlowe.Runtime.CLI.Option (CliOption, optParserWithEnvDefault, port)
import qualified Language.Marlowe.Runtime.CLI.Option as Runtime.CLI.Option
import Network.Socket (PortNumber)
import Options.Applicative (OptionFields)
import qualified Options.Applicative as O
import Options.Applicative.Types (Parser(BindP))


-- | Marlowe CLI commands and options for testing contracts.
type TestCommand era = TestSuite era FilePath


-- | Run a contract-testing command.
runTestCommand :: IsShelleyBasedEra era
               => MonadError CliError m
               => MonadIO m
               => MonadReader (CliEnv era) m
               => TestCommand era  -- ^ The command.
               -> m ()         -- ^ Action for running the command.
runTestCommand cmd = do
  era <- askEra
  runTestSuite era cmd


executionModeParser :: O.Parser ExecutionMode
executionModeParser = fmap (fromMaybe (OnChainMode (120 :: Second))) simulationModeOpt


simulationModeOpt :: O.Parser (Maybe ExecutionMode)
simulationModeOpt = O.optional (O.flag' SimulationMode  (O.long "simulation-mode" <> O.help "Run test suite in simulation mode by ignoring the transaction submission timeout"))


-- | Parser for the "testSuite" options.
mkParseTestCommand :: IsShelleyBasedEra era
               => O.Mod O.OptionFields NetworkId
               -> O.Mod O.OptionFields FilePath
               -> IO (O.Parser (TestCommand era))
mkParseTestCommand network socket = do
  let
    chainSeekSyncPort :: CliOption OptionFields PortNumber
    chainSeekSyncPort = port "chain-seek-sync" "CHAIN_SEEK_SYNC" 3715 "The port number of the chain-seek server's synchronization API."

    chainSeekCmdPort :: CliOption OptionFields PortNumber
    chainSeekCmdPort = port "chain-seek-cmd" "CHAIN_SEEK_CMD" 3720 "The port number of the chain-seek server's command API."

  runtimePortParser <- optParserWithEnvDefault Runtime.CLI.Option.runtimePort
  runtimeHostParser <- optParserWithEnvDefault Runtime.CLI.Option.runtimeHost
  chainSeekSyncPortParser <- optParserWithEnvDefault chainSeekSyncPort
  chainSeekCmdPortParser <- optParserWithEnvDefault chainSeekCmdPort

  let
    runtimeConfigParser = RuntimeConfig
      <$> runtimeHostParser
      <*> runtimePortParser
      <*> chainSeekSyncPortParser
      <*> chainSeekCmdPortParser

    reportingStrategyParser = do
      let
        streamJsonOpt = O.optional (O.flag' ()  (O.long "stream-json" <> O.help "Stream result json objects to the stdout as they are produced. All the other debug logs are outputed to stderr."))
        writeToJsonFile = O.optional (O.strOption (O.long "write-to-json-file" <> O.metavar "FILE" <> O.help "Write result result json objects list to a report file."))
      BindP ((,) <$> streamJsonOpt <*> writeToJsonFile) \case
        (Nothing, Nothing) -> pure Nothing
        (Just (), Nothing) -> pure $ Just StreamJSON
        (Nothing, Just file) -> pure $ Just $ WriteJSONFile file
        (Just (), Just file) -> pure $ Just $ StreamAndWriteJSONFile file

  pure $ TestSuite
    <$> parseNetworkId network
    <*> O.strOption              (O.long "socket-path"      <> O.metavar "SOCKET_FILE"  <> socket   <> O.help "Location of the cardano-node socket file. Defaults to the CARDANO_NODE_SOCKET_PATH environment variable's value.")
    <*> O.strOption              (O.long "faucet-skey-file" <> O.metavar "FAUCET_SIGNING_KEY_FILE"  <> O.help "The file containing the signing key for the faucet."                                                             )
    <*> O.option parseAddress    (O.long "faucet-address"   <> O.metavar "FAUCET_ADDRESS"           <> O.help "The address of the faucet."                                                                                      )
    <*> executionModeParser
    <*> (O.some . O.strArgument) (                           O.metavar "TEST_FILE"                  <> O.help "JSON file containing a test case."                                                                               )
    <*> runtimeConfigParser
    <*> do
      let
        parser = ConcurrentRunners <$> O.auto
        defaultValue = ConcurrentRunners 3
      O.option parser            (O.value defaultValue <> O.long "concurrent-runners" <> O.metavar "INTEGER" <> O.help "The number of concurrent test runners."                                                                         )
    <*> reportingStrategyParser
    <*> O.option O.auto          (O.value 3            <> O.long "max-retries"      <> O.metavar "INTEGER"                  <> O.help "The maximum number of retries for each test case."                                                              )
