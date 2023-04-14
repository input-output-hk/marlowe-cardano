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


{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
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
import Language.Marlowe.CLI.Test (runTests)
import Language.Marlowe.CLI.Test.ExecutionMode (ExecutionMode(OnChainMode, SimulationMode))
import Language.Marlowe.CLI.Test.Types (RuntimeConfig(RuntimeConfig), TestSuite(TestSuite))
import Language.Marlowe.CLI.Types (CliEnv, CliError, askEra)

import Control.Monad.Reader.Class (MonadReader)
import Data.Time.Units (Second)
import Language.Marlowe.Runtime.CLI.Option (CliOption, optParserWithEnvDefault, port)
import qualified Language.Marlowe.Runtime.CLI.Option as Runtime.CLI.Option
import Network.Socket (PortNumber)
import Options.Applicative (OptionFields)
import qualified Options.Applicative as O


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
  runTests era cmd


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
  pure $ TestSuite
    <$> parseNetworkId network
    <*> O.strOption              (O.long "socket-path"    <> O.metavar "SOCKET_FILE"  <> socket  <> O.help "Location of the cardano-node socket file. Defaults to the CARDANO_NODE_SOCKET_PATH environment variable's value.")
    <*> O.strOption              (O.long "faucet-key"     <> O.metavar "SIGNING_FILE"            <> O.help "The file containing the signing key for the faucet."                                                             )
    <*> O.option parseAddress    (O.long "faucet-address" <> O.metavar "ADDRESS"                 <> O.help "The address of the faucet."                                                                                      )
    <*> executionModeParser
    <*> (O.some . O.strArgument) (                           O.metavar "TEST_FILE"               <> O.help "JSON file containing a test case."                                                                               )
    <*> runtimeConfigParser
