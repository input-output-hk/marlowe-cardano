-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-----------------------------------------------------------------------------
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Contract-testing commands in the Marlowe CLI tool.
module Language.Marlowe.CLI.Command.Test (
  -- * Marlowe CLI Commands
  TestCommand,
  mkParseTestCommand,
  runTestCommand,
) where

import Cardano.Api (BabbageEraOnwards, IsShelleyBasedEra, NetworkId)
import Control.Monad.Except (MonadError, MonadIO)
import Data.Maybe (fromMaybe)
import Language.Marlowe.CLI.Command.Parse (parseAddress, parseNetworkId)
import Language.Marlowe.CLI.Test (runTestSuite)
import Language.Marlowe.CLI.Test.ExecutionMode (UseExecutionMode (..))
import Language.Marlowe.CLI.Test.Types (
  MaxConcurrentRunners (MaxConcurrentRunners),
  ReportingStrategy (..),
  RuntimeConfig (RuntimeConfig),
  TestSuite (TestSuite),
 )
import Language.Marlowe.CLI.Types (CliEnv, CliError, askEra)

import Control.Monad.Reader.Class (MonadReader)
import Data.Time.Units (Second)
import Language.Marlowe.Runtime.CLI.Option (optParserWithEnvDefault)
import Language.Marlowe.Runtime.CLI.Option qualified as Runtime.CLI.Option
import Options.Applicative qualified as O
import Options.Applicative.Types (Parser (BindP))

-- | Marlowe CLI commands and options for testing contracts.
type TestCommand era = TestSuite era FilePath

-- | Run a contract-testing command.
runTestCommand
  :: (IsShelleyBasedEra era)
  => (MonadError CliError m)
  => (MonadIO m)
  => (MonadReader (CliEnv era) m)
  => TestCommand era
  -- ^ The command.
  -> m ()
  -- ^ Action for running the command.
runTestCommand cmd = do
  era <- askEra
  runTestSuite era cmd

txBuildupContextParser :: O.Parser UseExecutionMode
txBuildupContextParser = fmap (fromMaybe (UseOnChainMode (120 :: Second))) simulationModeOpt

simulationModeOpt :: O.Parser (Maybe UseExecutionMode)
simulationModeOpt =
  O.optional
    ( O.flag'
        UseSimulationMode
        (O.long "simulation-mode" <> O.help "Run test suite in simulation mode by ignoring the transaction submission timeout")
    )

-- | Parser for the "testSuite" options.
mkParseTestCommand
  :: BabbageEraOnwards era
  -> O.Mod O.OptionFields NetworkId
  -> O.Mod O.OptionFields FilePath
  -> IO (O.Parser (TestCommand era))
mkParseTestCommand era network socket = do
  runtimePortParser <- optParserWithEnvDefault Runtime.CLI.Option.runtimePort
  runtimeHostParser <- optParserWithEnvDefault Runtime.CLI.Option.runtimeHost

  let runtimeConfigParser =
        RuntimeConfig
          <$> runtimeHostParser
          <*> runtimePortParser

      reportingStrategyParser = do
        let streamJsonOpt =
              O.optional
                ( O.flag'
                    ()
                    ( O.long "stream-json"
                        <> O.help "Stream result json objects to the stdout as they are produced. All the other debug logs are output to stderr."
                    )
                )
            writeToJsonFile =
              O.optional
                ( O.strOption
                    (O.long "write-to-json-file" <> O.metavar "FILE" <> O.help "Write result result json objects list to a report file.")
                )
        BindP ((,) <$> streamJsonOpt <*> writeToJsonFile) \case
          (Nothing, Nothing) -> pure Nothing
          (Just (), Nothing) -> pure $ Just StreamJSON
          (Nothing, Just file) -> pure $ Just $ WriteJSONFile file
          (Just (), Just file) -> pure $ Just $ StreamAndWriteJSONFile file

  pure $
    TestSuite
      <$> parseNetworkId network
      <*> O.strOption
        ( O.long "socket-path"
            <> O.metavar "SOCKET_FILE"
            <> socket
            <> O.help
              "Location of the cardano-node socket file. Defaults to the CARDANO_NODE_SOCKET_PATH environment variable's value."
        )
      <*> O.strOption
        ( O.long "faucet-skey-file"
            <> O.metavar "FAUCET_SIGNING_KEY_FILE"
            <> O.help "The file containing the signing key for the faucet."
        )
      <*> O.option
        (parseAddress era)
        ( O.long "faucet-address" <> O.metavar "FAUCET_ADDRESS" <> O.help "The address of the faucet."
        )
      <*> txBuildupContextParser
      <*> (O.some . O.strArgument)
        ( O.metavar "TEST_FILE" <> O.help "JSON file containing a test case."
        )
      <*> runtimeConfigParser
      <*> do
        let parser = MaxConcurrentRunners <$> O.auto
            defaultValue = MaxConcurrentRunners 3
        O.option
          parser
          ( O.value defaultValue
              <> O.long "max-concurrent-runners"
              <> O.metavar "INTEGER"
              <> O.help "The number of concurrent test runners."
          )
      <*> reportingStrategyParser
      <*> O.option
        O.auto
        ( O.value 3
            <> O.long "max-retries"
            <> O.metavar "INTEGER"
            <> O.help "The maximum number of retries for each test case."
        )
