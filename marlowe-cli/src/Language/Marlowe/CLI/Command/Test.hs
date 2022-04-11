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


{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}


module Language.Marlowe.CLI.Command.Test (
-- * Marlowe CLI Commands
  TestCommand
, parseTestCommand
, runTestCommand
) where


import Control.Monad.Except (MonadError, MonadIO)
import Language.Marlowe.CLI.Command.Parse (parseAddressAny, parseNetworkId, parseUrl)
import Language.Marlowe.CLI.Test (runTests)
import Language.Marlowe.CLI.Test.Types (MarloweTests (..))
import Language.Marlowe.CLI.Types (CliError)

import qualified Options.Applicative as O


-- | Marlowe CLI commands and options for testing contracts.
type TestCommand = MarloweTests FilePath


-- | Run a contract-testing command.
runTestCommand :: MonadError CliError m
               => MonadIO m
               => TestCommand  -- ^ The command.
               -> m ()         -- ^ Action for running the command.
runTestCommand = runTests


-- | Parser for test commands.
parseTestCommand :: O.Parser TestCommand
parseTestCommand =
  O.hsubparser
    $ O.commandGroup "Commands for testing contracts:"
--  <> _scriptsCommand
    <> pabsCommand


-- | Parser for the "scripts" command.
_scriptsCommand :: O.Mod O.CommandFields TestCommand
_scriptsCommand =
  O.command "scripts"
    $ O.info scriptsOptions
    $ O.progDesc "Test Marlowe scripts on-chain."


-- | Parser for the "scripts" options.
scriptsOptions :: O.Parser TestCommand
scriptsOptions =
  ScriptTests
    <$> (O.optional . O.option parseNetworkId) (O.long "testnet-magic"  <> O.metavar "INTEGER"                               <> O.help "Network magic, or omit for mainnet."                    )
    <*> O.strOption                            (O.long "socket-path"    <> O.metavar "SOCKET_FILE"                           <> O.help "Location of the cardano-node socket file."              )
    <*> O.option O.auto                        (O.long "slot-length"    <> O.metavar "INTEGER"      <> O.value 1000          <> O.help "The slot length, in milliseconds."                      )
    <*> O.option O.auto                        (O.long "slot-offset"    <> O.metavar "INTEGER"      <> O.value 1591566291000 <> O.help "The effective POSIX time of slot zero, in milliseconds.")
    <*> O.strOption                            (O.long "faucet-key"     <> O.metavar "SIGNING_FILE"                          <> O.help "The file containing the signing key for the faucet."    )
    <*> O.option parseAddressAny               (O.long "faucet-address" <> O.metavar "ADDRESS"                               <> O.help "The address of the faucet."                             )
    <*> O.option parseAddressAny               (O.long "burn-address"   <> O.metavar "ADDRESS"                               <> O.help "Burn address for discarding used tokens."               )
    <*> (O.some . O.strArgument)               (                           O.metavar "TEST_FILE"                             <> O.help "JSON file containing a test case."                      )


-- | Parser for the "contracts" command.
pabsCommand :: O.Mod O.CommandFields TestCommand
pabsCommand =
  O.command "contracts"
    . O.info pabsOptions
    $ O.progDesc "Test Marlowe contracts using the Marlowe PAB."


-- | Parser for the "contracts" options.
pabsOptions :: O.Parser TestCommand
pabsOptions =
  PabTests
    <$> (O.optional . O.option parseNetworkId) (O.long "testnet-magic"  <> O.metavar "INTEGER"      <> O.help "Network magic, or omit for mainnet."                )
    <*> O.strOption                            (O.long "socket-path"    <> O.metavar "SOCKET_FILE"  <> O.help "Location of the cardano-node socket file."          )
    <*> O.option parseUrl                      (O.long "wallet-url"     <> O.metavar "URL"          <> O.help "URL for Cardano Wallet."                            )
    <*> O.option parseUrl                      (O.long "pab-url"        <> O.metavar "URL"          <> O.help "URL for the Marlowe PAB."                           )
    <*> O.strOption                            (O.long "faucet-key"     <> O.metavar "SIGNING_FILE" <> O.help "The file containing the signing key for the faucet.")
    <*> O.option parseAddressAny               (O.long "faucet-address" <> O.metavar "ADDRESS"      <> O.help "The address of the faucet."                         )
    <*> O.option parseAddressAny               (O.long "burn-address"   <> O.metavar "ADDRESS"      <> O.help "Burn address for discarding used tokens."           )
    <*> O.strOption                            (O.long "passphrase"     <> O.metavar "PASSWORD"     <> O.help "The passphrase used for the Marlowe PAB."           )
    <*> (O.some . O.strArgument)               (                           O.metavar "TEST_FILE"    <> O.help "JSON file containing a test case."                  )
