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
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

-- | Commands for the Marlowe CLI tool.
module Language.Marlowe.CLI.Command (
  -- * Entry Point
  runCLI,

  -- * Marlowe CLI Commands
  Command (..),
  mkCommandParser,
  runCommand,
) where

import Cardano.Api (
  BabbageEra,
  BabbageEraOnwards (..),
  ConwayEra,
  IsShelleyBasedEra,
  NetworkId,
  babbageEraOnwardsToShelleyBasedEra,
  shelleyBasedEraConstraints,
 )
import Control.Monad.Except (MonadError, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (Foldable (fold), asum)
import Language.Marlowe.CLI.Command.Contract (ContractCommand, parseContractCommand, runContractCommand)
import Language.Marlowe.CLI.Command.Format (FormatCommand, parseFormatCommand, runFormatCommand)
import Language.Marlowe.CLI.Command.Input (InputCommand, parseInputCommand, runInputCommand)
import Language.Marlowe.CLI.Command.Role (RoleCommand, parseRoleCommand, runRoleCommand)
import Language.Marlowe.CLI.Command.Run (RunCommand, parseRunCommand, runRunCommand)
import Language.Marlowe.CLI.Command.Template (
  OutputFiles (..),
  TemplateCommand,
  parseTemplateCommand,
  parseTemplateCommandOutputFiles,
  runTemplateCommand,
 )
import Language.Marlowe.CLI.Command.Test (TestCommand, mkParseTestCommand, runTestCommand)
import Language.Marlowe.CLI.Command.Transaction (TransactionCommand, parseTransactionCommand, runTransactionCommand)
import Language.Marlowe.CLI.Command.Util (UtilCommand, parseUtilCommand, runUtilCommand)
import Language.Marlowe.CLI.IO (getNetworkMagic, getNodeSocketPath)
import Language.Marlowe.CLI.Types (CliEnv (..), CliError (..))
import System.Exit (exitFailure)
import System.IO (BufferMode (LineBuffering), hPutStrLn, hSetBuffering, stderr, stdout)

import Control.Monad.Reader (runReaderT)
import Options.Applicative qualified as O
import Options.Applicative.Types qualified as O

-- | Marlowe CLI commands and options.
data Command era
  = -- | Contract-related commands.
    RunCommand (RunCommand era)
  | -- | Export-related commands.
    ContractCommand ContractCommand
  | -- | Input-related commands.
    InputCommand InputCommand
  | -- | Role-related commands.
    RoleCommand RoleCommand
  | -- | Template-related commands.
    TemplateCommand TemplateCommand OutputFiles
  | -- | Transaction-related commands.
    TransactionCommand (TransactionCommand era)
  | -- | Miscellaneous commands.
    UtilCommand (UtilCommand era)
  | -- | Test-related commands.
    TestCommand (TestCommand era)
  | -- | Format-related commands.
    FormatCommand FormatCommand

data SomeCommand = forall era. SomeCommand (BabbageEraOnwards era) (Command era)

data SomeEra = forall era. SomeEra (BabbageEraOnwards era)

-- | Main entry point for Marlowe CLI tool.
runCLI
  :: String
  -- ^ The version of the tool.
  -> IO ()
  -- ^ Action to run the tool.
runCLI version =
  do
    networkId <- maybe mempty O.value <$> liftIO getNetworkMagic
    socketPath <- maybe mempty O.value <$> liftIO getNodeSocketPath
    commandParser <- mkCommandParser networkId socketPath version
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    SomeCommand era command <-
      O.customExecParser O.defaultPrefs{O.prefShowHelpOnEmpty = True} commandParser
    result <- runExceptT $ shelleyBasedEraConstraints (babbageEraOnwardsToShelleyBasedEra era) $ runCommand era command
    case result of
      Right () -> return ()
      Left message -> do
        hPutStrLn stderr $ unCliError message
        exitFailure

-- | Run a CLI command.
runCommand
  :: (IsShelleyBasedEra era)
  => (MonadError CliError m)
  => (MonadIO m)
  => BabbageEraOnwards era
  -> Command era
  -- ^ The command.
  -> m ()
  -- ^ Action to run the command.
runCommand era cmd = flip runReaderT CliEnv{..} case cmd of
  RunCommand command -> runRunCommand command
  ContractCommand command -> runContractCommand command
  TestCommand command -> runTestCommand command
  InputCommand command -> runInputCommand command
  RoleCommand command -> runRoleCommand command
  TemplateCommand command outputFiles -> runTemplateCommand command outputFiles
  TransactionCommand command -> runTransactionCommand era command
  UtilCommand command -> runUtilCommand command
  FormatCommand command -> runFormatCommand command

-- | Command parseCommand for the tool version.
mkCommandParser
  :: O.Mod O.OptionFields NetworkId
  -- ^ The default network ID.
  -> O.Mod O.OptionFields FilePath
  -- ^ The default node socket path.
  -> String
  -- ^ The tool version.
  -> IO (O.ParserInfo SomeCommand)
  -- ^ The command parseCommand.
mkCommandParser networkId socketPath version = do
  someCommandParser <- mkSomeCommandParser
  pure $
    O.info
      ( O.helper
          <*> versionOption version
          <*> someCommandParser
      )
      ( O.fullDesc
          <> O.progDesc "Utilities for Marlowe."
          <> O.header "marlowe-cli : a command-line tool for Marlowe contracts"
      )
  where
    eraOption :: O.Parser SomeEra
    eraOption =
      asum
        [ O.flag'
            (SomeEra BabbageEraOnwardsBabbage)
            ( O.long "babbage-era"
                <> O.help "Specify the Babbage era (default)"
            )
        , O.flag'
            (SomeEra BabbageEraOnwardsConway)
            ( O.long "conway-era"
                <> O.help "Specify the Conway era"
            )
        , pure (SomeEra BabbageEraOnwardsBabbage)
        ]
    mkSomeCommandParser :: IO (O.Parser SomeCommand)
    mkSomeCommandParser = do
      let parseTestCommand :: BabbageEraOnwards era -> IO (O.Parser (TestCommand era))
          parseTestCommand era = mkParseTestCommand era networkId socketPath
      -- FIXME: Is is possible to avoid this duplication?
      -- It seems to be hard to mix `BindP` and `IO`.
      testCommandParsers <- (,) <$> parseTestCommand BabbageEraOnwardsBabbage <*> parseTestCommand BabbageEraOnwardsConway
      pure $ O.BindP eraOption (mkSomeCommandParser' testCommandParsers)

    mkSomeCommandParser'
      :: (O.Parser (TestCommand BabbageEra), O.Parser (TestCommand ConwayEra))
      -> SomeEra
      -> O.Parser SomeCommand
    mkSomeCommandParser' (testCommandParser, _) (SomeEra BabbageEraOnwardsBabbage) = do
      SomeCommand BabbageEraOnwardsBabbage <$> commandParser BabbageEraOnwardsBabbage testCommandParser
    mkSomeCommandParser' (_, testCommandParser) (SomeEra BabbageEraOnwardsConway) = do
      SomeCommand BabbageEraOnwardsConway <$> commandParser BabbageEraOnwardsConway testCommandParser

    commandParser :: BabbageEraOnwards era -> O.Parser (TestCommand era) -> O.Parser (Command era)
    commandParser era testCommandParser =
      asum
        [ O.hsubparser $
            fold
              [ O.commandGroup "High-level commands:"
              , O.command "run" $
                  O.info (RunCommand <$> parseRunCommand era networkId socketPath) $
                    O.progDesc "Run a contract."
              , O.command "template" $
                  O.info (TemplateCommand <$> parseTemplateCommand <*> parseTemplateCommandOutputFiles) $
                    O.progDesc "Create a contract from a template."
              , O.command "test" $
                  O.info (TestCommand <$> testCommandParser) $
                    O.progDesc "Run test scenario described using yaml based DSL."
              , O.command "format" $
                  O.info (FormatCommand <$> parseFormatCommand) $
                    O.progDesc "Convert between formats and pretty-print Marlowe contracts."
              ]
        , O.hsubparser $
            fold
              [ O.commandGroup "Low-level commands:"
              , O.command "contract" $
                  O.info (ContractCommand <$> parseContractCommand networkId) $
                    O.progDesc "Export contract address, validator, datum, or redeemer."
              , O.command "input" $
                  O.info (InputCommand <$> parseInputCommand) $
                    O.progDesc "Create inputs to a contract."
              , O.command "role" $
                  O.info (RoleCommand <$> parseRoleCommand networkId) $
                    O.progDesc "Export role address, validator, datum, or redeemer."
              , O.command "transaction" $
                  O.info (TransactionCommand <$> parseTransactionCommand era networkId socketPath) $
                    O.progDesc "Create and submit transactions."
              , O.command "util" $
                  O.info (UtilCommand <$> parseUtilCommand era networkId socketPath) $
                    O.progDesc "Miscellaneous utilities."
              ]
        ]

-- | Option parseCommand for the tool version.
versionOption
  :: String
  -- ^ The tool version.
  -> O.Parser (a -> a)
  -- ^ The option parseCommand.
versionOption version =
  O.infoOption
    ("marlowe-cli " <> version)
    (O.long "version" <> O.help "Show version.")
