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


import Cardano.Api (NetworkId)
import Control.Monad.Except (MonadError, MonadIO)
import Language.Marlowe.CLI.Command.Parse (parseAddressAny, parseNetworkId, parseUrl)
import Language.Marlowe.CLI.Test (runTests)
import Language.Marlowe.CLI.Test.Types (MarloweTests (PabTests, ScriptTests))
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
parseTestCommand :: NetworkId -> FilePath -> O.Parser TestCommand
parseTestCommand network socket=
  O.hsubparser
    $ O.commandGroup "Commands for testing contracts:"
--  <> _scriptsCommand network socket
    <> pabsCommand network socket


-- | Parser for the "scripts" command.
_scriptsCommand :: NetworkId -> FilePath -> O.Mod O.CommandFields TestCommand
_scriptsCommand network socket =
  O.command "scripts"
    $ O.info (scriptsOptions network socket)
    $ O.progDesc "Test Marlowe scripts on-chain."


-- | Parser for the "scripts" options.
scriptsOptions :: NetworkId -> FilePath -> O.Parser TestCommand
scriptsOptions network socket =
  ScriptTests
    <$> (O.optional . O.option parseNetworkId) (O.long "testnet-magic"  <> O.metavar "INTEGER"      <> O.value network <> O.help "Network magic. Defaults to the CARDANO_TESTNET_MAGIC environment variable's value."                              )
    <*> O.strOption                            (O.long "socket-path"    <> O.metavar "SOCKET_FILE"  <> O.value socket  <> O.help "Location of the cardano-node socket file. Defaults to the CARDANO_NODE_SOCKET_PATH environment variable's value.")
    <*> O.strOption                            (O.long "faucet-key"     <> O.metavar "SIGNING_FILE"                    <> O.help "The file containing the signing key for the faucet."                                                             )
    <*> O.option parseAddressAny               (O.long "faucet-address" <> O.metavar "ADDRESS"                         <> O.help "The address of the faucet."                                                                                      )
    <*> O.option parseAddressAny               (O.long "burn-address"   <> O.metavar "ADDRESS"                         <> O.help "Burn address for discarding used tokens."                                                                        )
    <*> (O.some . O.strArgument)               (                           O.metavar "TEST_FILE"                       <> O.help "JSON file containing a test case."                                                                               )


-- | Parser for the "contracts" command.
pabsCommand :: NetworkId -> FilePath -> O.Mod O.CommandFields TestCommand
pabsCommand network socket =
  O.command "contracts"
    . O.info (pabsOptions network socket)
    $ O.progDesc "Test Marlowe contracts using the Marlowe PAB."


-- | Parser for the "contracts" options.
pabsOptions :: NetworkId -> FilePath -> O.Parser TestCommand
pabsOptions network socket =
  PabTests
    <$> (O.optional . O.option parseNetworkId) (O.long "testnet-magic"  <> O.metavar "INTEGER"      <> O.value network <> O.help "Network magic. Defaults to the CARDANO_TESTNET_MAGIC environment variable's value."                              )
    <*> O.strOption                            (O.long "socket-path"    <> O.metavar "SOCKET_FILE"  <> O.value socket  <> O.help "Location of the cardano-node socket file. Defaults to the CARDANO_NODE_SOCKET_PATH environment variable's value.")
    <*> O.option parseUrl                      (O.long "wallet-url"     <> O.metavar "URL"                             <> O.help "URL for Cardano Wallet."                                                                                         )
    <*> O.option parseUrl                      (O.long "pab-url"        <> O.metavar "URL"                             <> O.help "URL for the Marlowe PAB."                                                                                        )
    <*> O.strOption                            (O.long "faucet-key"     <> O.metavar "SIGNING_FILE"                    <> O.help "The file containing the signing key for the faucet."                                                             )
    <*> O.option parseAddressAny               (O.long "faucet-address" <> O.metavar "ADDRESS"                         <> O.help "The address of the faucet."                                                                                      )
    <*> O.option parseAddressAny               (O.long "burn-address"   <> O.metavar "ADDRESS"                         <> O.help "Burn address for discarding used tokens."                                                                        )
    <*> O.strOption                            (O.long "passphrase"     <> O.metavar "PASSWORD"                        <> O.help "The passphrase used for the Marlowe PAB."                                                                        )
    <*> (O.some . O.strArgument)               (                           O.metavar "TEST_FILE"                       <> O.help "JSON file containing a test case."                                                                               )
