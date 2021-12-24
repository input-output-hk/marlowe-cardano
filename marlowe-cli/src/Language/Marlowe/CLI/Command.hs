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


{-# LANGUAGE FlexibleContexts #-}


module Language.Marlowe.CLI.Command (
-- * Entry Point
  runCLI
-- * Marlowe CLI Commands
, Command(..)
, parseCommand
, runCommand
) where


import           Control.Monad.Except                     (MonadError, MonadIO, runExceptT)
import           Language.Marlowe.CLI.Command.Contract    (ContractCommand, parseContractCommand, runContractCommand)
import           Language.Marlowe.CLI.Command.Export      (ExportCommand, parseExportCommand, runExportCommand)
import           Language.Marlowe.CLI.Command.Input       (InputCommand, parseInputCommand, runInputCommand)
import           Language.Marlowe.CLI.Command.Role        (RoleCommand, parseRoleCommand, runRoleCommand)
import           Language.Marlowe.CLI.Command.Template    (TemplateCommand, parseTemplateCommand, runTemplateCommand)
import           Language.Marlowe.CLI.Command.Transaction (TransactionCommand, parseTransactionCommand,
                                                           runTransactionCommand)
import           Language.Marlowe.CLI.Command.Util        (UtilCommand, parseUtilCommand, runUtilCommand)
import           Language.Marlowe.CLI.Types               (CliError (..))
import           System.Exit                              (exitFailure)
import           System.IO                                (hPutStrLn, stderr)

import qualified Options.Applicative                      as O


-- | Marlowe CLI commands and options.
data Command =
    -- | Contract-related commands.
    ContractCommand ContractCommand
    -- | Export-related commands.
  | ExportCommand ExportCommand
    -- | Input-related commands.
  | InputCommand InputCommand
    -- | Role-related commands.
  | RoleCommand RoleCommand
    -- | Template-related commands.
  | TemplateCommand TemplateCommand
    -- | Transaction-related commands.
  | TransactionCommand TransactionCommand
    -- | Miscellaneous commands.
  | UtilCommand UtilCommand


-- | Main entry point for Marlowe CLI tool.
runCLI :: String  -- ^ The version of the tool.
       -> IO ()   -- ^ Action to run the tool.
runCLI version =
  do
    command <- O.execParser $ parseCommand version
    result <- runExceptT $ runCommand command
    case result of
      Right ()      -> return ()
      Left  message -> do
                         hPutStrLn stderr $ unCliError message
                         exitFailure


-- | Run a CLI command.
runCommand :: MonadError CliError m
           => MonadIO m
           => Command  -- ^ The command.
           -> m ()     -- ^ Action to run the command.
runCommand (ContractCommand    command) = runContractCommand    command
runCommand (ExportCommand      command) = runExportCommand      command
runCommand (InputCommand       command) = runInputCommand       command
runCommand (RoleCommand        command) = runRoleCommand        command
runCommand (TemplateCommand    command) = runTemplateCommand    command
runCommand (TransactionCommand command) = runTransactionCommand command
runCommand (UtilCommand        command) = runUtilCommand        command


-- | Command parseCommand for the tool version.
parseCommand :: String                -- ^ The tool version.
             -> O.ParserInfo Command  -- ^ The command parseCommand.
parseCommand version =
  O.info
    (
          O.helper
      <*> versionOption version
      <*> O.hsubparser
          (
               O.command "contract"    (O.info (ExportCommand      <$> parseExportCommand     ) $ O.progDesc "Export contract address, validator, datum, or redeemer. [low-level]")
            <> O.command "input"       (O.info (InputCommand       <$> parseInputCommand      ) $ O.progDesc "Create inputs to a contract."                                       )
            <> O.command "role"        (O.info (RoleCommand        <$> parseRoleCommand       ) $ O.progDesc "Export role address, validator, datum, or redeemer. [low-level]"    )
            <> O.command "run"         (O.info (ContractCommand    <$> parseContractCommand   ) $ O.progDesc "Run a contract."                                                    )
            <> O.command "template"    (O.info (TemplateCommand    <$> parseTemplateCommand   ) $ O.progDesc "Create a contract from a template."                                 )
            <> O.command "transaction" (O.info (TransactionCommand <$> parseTransactionCommand) $ O.progDesc "Create and submit transactions. [low-level]"                        )
            <> O.command "util"        (O.info (UtilCommand        <$> parseUtilCommand       ) $ O.progDesc "Miscellaneous utilities. [low-level]"                               )
          )
    )
    (
         O.fullDesc
      <> O.progDesc "Utilities for Marlowe."
      <> O.header "marlow-cli : a command-line tool for Marlowe contracts"
    )


-- | Option parseCommand for the tool versoin.
versionOption :: String            -- ^ The tool version.
              -> O.Parser (a -> a) -- ^ The option parseCommand.
versionOption version =
  O.infoOption
    ("marlowe-cli " <> version)
    (O.long "version" <> O.help "Show version.")
