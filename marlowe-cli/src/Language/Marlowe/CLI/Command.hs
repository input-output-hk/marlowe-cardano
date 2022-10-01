-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Commands for the Marlowe CLI tool.
--
-----------------------------------------------------------------------------


{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}


module Language.Marlowe.CLI.Command
  ( -- * Entry Point
    runCLI
    -- * Marlowe CLI Commands
  , Command(..)
  , parseCommand
  , runCommand
  ) where


import Cardano.Api (IsShelleyBasedEra, NetworkId, ScriptDataSupportedInEra(..))
import Control.Monad.Except (MonadError, MonadIO, liftIO, runExceptT)
import Data.Foldable (Foldable(fold), asum)
import Language.Marlowe.CLI.Command.Contract (ContractCommand, parseContractCommand, runContractCommand)
import Language.Marlowe.CLI.Command.Input (InputCommand, parseInputCommand, runInputCommand)
import Language.Marlowe.CLI.Command.Role (RoleCommand, parseRoleCommand, runRoleCommand)
import Language.Marlowe.CLI.Command.Run (RunCommand, parseRunCommand, runRunCommand)
import Language.Marlowe.CLI.Command.Template
  (OutputFiles(..), TemplateCommand, parseTemplateCommand, parseTemplateCommandOutputFiles, runTemplateCommand)
import Language.Marlowe.CLI.Command.Test (TestCommand, parseTestCommand, runTestCommand)
import Language.Marlowe.CLI.Command.Transaction (TransactionCommand, parseTransactionCommand, runTransactionCommand)
import Language.Marlowe.CLI.Command.Util (UtilCommand, parseUtilCommand, runUtilCommand)
import Language.Marlowe.CLI.IO (getNetworkMagic, getNodeSocketPath)
import Language.Marlowe.CLI.Types (CliEnv(..), CliError(..), withShelleyBasedEra)
import System.Exit (exitFailure)
import System.IO (BufferMode(LineBuffering), hPutStrLn, hSetBuffering, stderr, stdout)

import Control.Applicative ((<|>))
import Control.Monad.Reader (runReaderT)
import qualified Options.Applicative as O


-- | Marlowe CLI commands and options.
data Command era =
    -- | Contract-related commands.
    RunCommand (RunCommand era)
    -- | Export-related commands.
  | ContractCommand ContractCommand
    -- | Input-related commands.
  | InputCommand InputCommand
    -- | Role-related commands.
  | RoleCommand RoleCommand
    -- | Template-related commands.
  | TemplateCommand TemplateCommand OutputFiles
    -- | Transaction-related commands.
  | TransactionCommand (TransactionCommand era)
    -- | Miscellaneous commands.
  | UtilCommand (UtilCommand era)
    -- | Test-related commands.
  | TestCommand (TestCommand era)

data SomeCommand = forall era. SomeCommand (ScriptDataSupportedInEra era) (Command era)

-- | Main entry point for Marlowe CLI tool.
runCLI :: String  -- ^ The version of the tool.
       -> IO ()   -- ^ Action to run the tool.
runCLI version =
  do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    networkId <- maybe mempty O.value <$> liftIO getNetworkMagic
    socketPath <- maybe mempty O.value <$> liftIO getNodeSocketPath
    SomeCommand era command <- O.execParser $ parseCommand networkId socketPath version
    result <- runExceptT $ withShelleyBasedEra era $ runCommand era command
    case result of
      Right ()      -> return ()
      Left  message -> do
                         hPutStrLn stderr $ unCliError message
                         exitFailure


-- | Run a CLI command.
runCommand :: IsShelleyBasedEra era
           => MonadError CliError m
           => MonadIO m
           => ScriptDataSupportedInEra era
           -> Command era  -- ^ The command.
           -> m ()     -- ^ Action to run the command.
runCommand era cmd = flip runReaderT CliEnv{..} case cmd of
  RunCommand command                  -> runRunCommand command
  ContractCommand command             -> runContractCommand command
  TestCommand command                 -> runTestCommand command
  InputCommand command                -> runInputCommand command
  RoleCommand command                 -> runRoleCommand command
  TemplateCommand command outputFiles -> runTemplateCommand command outputFiles
  TransactionCommand command          -> runTransactionCommand command
  UtilCommand command                 -> runUtilCommand command

-- | Command parseCommand for the tool version.
parseCommand :: O.Mod O.OptionFields NetworkId  -- ^ The default network ID.
             -> O.Mod O.OptionFields FilePath   -- ^ The default node socket path.
             -> String                          -- ^ The tool version.
             -> O.ParserInfo SomeCommand        -- ^ The command parseCommand.
parseCommand networkId socketPath version =
  O.info
    (
          O.helper
      <*> versionOption version
      <*> (noEraParser <|> alonzoParser <|> babbageParser)
    )
    (
         O.fullDesc
      <> O.progDesc "Utilities for Marlowe."
      <> O.header "marlowe-cli : a command-line tool for Marlowe contracts"
    )
  where
    alonzoParser = SomeCommand <$> alonzoFlagParser <*> commandParser
    babbageParser = SomeCommand <$> babbageFlagParser <*> commandParser
    noEraParser = SomeCommand ScriptDataInBabbageEra <$> commandParser
    alonzoFlagParser = O.flag ScriptDataInAlonzoEra ScriptDataInAlonzoEra $ mconcat
      [ O.long "alonzo-era"
      , O.help "Read and write Alonzo transactions"
      ]
    babbageFlagParser = O.flag' ScriptDataInBabbageEra $ mconcat
      [ O.long "babbage-era"
      , O.help "Read and write Babbage transactions"
      ]
    commandParser :: IsShelleyBasedEra era => O.Parser (Command era)
    commandParser = asum
      [
        O.hsubparser $ fold
          [ O.commandGroup "High-level commands:"
          , O.command "run"         $ O.info (RunCommand      <$> parseRunCommand networkId socketPath )                     $ O.progDesc "Run a contract."
          , O.command "template"    $ O.info (TemplateCommand <$> parseTemplateCommand <*> parseTemplateCommandOutputFiles)  $ O.progDesc "Create a contract from a template."
          , O.command "test"        $ O.info (TestCommand     <$> parseTestCommand networkId socketPath)                     $ O.progDesc "Test contracts."
          ]
      , O.hsubparser $ fold
          [ O.commandGroup "Low-level commands:"
          , O.command "contract"    $ O.info (ContractCommand    <$> parseContractCommand networkId              ) $ O.progDesc "Export contract address, validator, datum, or redeemer."
          , O.command "input"       $ O.info (InputCommand       <$> parseInputCommand                           ) $ O.progDesc "Create inputs to a contract."
          , O.command "role"        $ O.info (RoleCommand        <$> parseRoleCommand networkId                  ) $ O.progDesc "Export role address, validator, datum, or redeemer."
          , O.command "transaction" $ O.info (TransactionCommand <$> parseTransactionCommand networkId socketPath) $ O.progDesc "Create and submit transactions."
          , O.command "util"        $ O.info (UtilCommand        <$> parseUtilCommand networkId socketPath       ) $ O.progDesc "Miscellaneous utilities."
          ]
      ]


-- | Option parseCommand for the tool version.
versionOption :: String            -- ^ The tool version.
              -> O.Parser (a -> a) -- ^ The option parseCommand.
versionOption version =
  O.infoOption
    ("marlowe-cli " <> version <> " «mainnet»")
    (O.long "version" <> O.help "Show version.")
