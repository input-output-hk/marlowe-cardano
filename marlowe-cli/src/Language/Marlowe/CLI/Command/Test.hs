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


import Cardano.Api (IsShelleyBasedEra, NetworkId)
import Control.Monad.Except (MonadError, MonadIO)
import Language.Marlowe.CLI.Command.Parse (parseAddress, parseNetworkId, protocolVersionOpt)
import Language.Marlowe.CLI.Test (runTests)
import Language.Marlowe.CLI.Test.Types (MarloweTests (ScriptTests))
import Language.Marlowe.CLI.Types (CliEnv, CliError, askEra)

import Control.Monad.Reader.Class (MonadReader)
import qualified Options.Applicative as O


-- | Marlowe CLI commands and options for testing contracts.
type TestCommand era = MarloweTests era FilePath


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


-- | Parser for test commands.
parseTestCommand :: IsShelleyBasedEra era => O.Mod O.OptionFields NetworkId
                 -> O.Mod O.OptionFields FilePath
                 -> O.Parser (TestCommand era)
parseTestCommand network socket =
  O.hsubparser
    $ O.commandGroup "Commands for testing contracts:"
    <> scriptsCommand network socket


-- | Parser for the "scripts" command.
scriptsCommand :: IsShelleyBasedEra era => O.Mod O.OptionFields NetworkId
                -> O.Mod O.OptionFields FilePath
                -> O.Mod O.CommandFields (TestCommand era)
scriptsCommand network socket =
  O.command "scripts"
    $ O.info (scriptsOptions network socket)
    $ O.progDesc "Test Marlowe scripts on-chain."


-- | Parser for the "scripts" options.
scriptsOptions :: IsShelleyBasedEra era
               => O.Mod O.OptionFields NetworkId
               -> O.Mod O.OptionFields FilePath
               -> O.Parser (TestCommand era)
scriptsOptions network socket =
  ScriptTests
    <$> O.option parseNetworkId  (O.long "testnet-magic"  <> O.metavar "INTEGER"      <> network <> O.help "Network magic. Defaults to the CARDANO_TESTNET_MAGIC environment variable's value."                              )
    <*> O.strOption              (O.long "socket-path"    <> O.metavar "SOCKET_FILE"  <> socket  <> O.help "Location of the cardano-node socket file. Defaults to the CARDANO_NODE_SOCKET_PATH environment variable's value.")
    <*> O.strOption              (O.long "faucet-key"     <> O.metavar "SIGNING_FILE"            <> O.help "The file containing the signing key for the faucet."                                                             )
    <*> O.option parseAddress    (O.long "faucet-address" <> O.metavar "ADDRESS"                 <> O.help "The address of the faucet."                                                                                      )
    <*> O.option parseAddress    (O.long "burn-address"   <> O.metavar "ADDRESS"                 <> O.help "Burn address for discarding used tokens."                                                                        )
    <*> protocolVersionOpt

    <*> (O.some . O.strArgument) (                           O.metavar "TEST_FILE"               <> O.help "JSON file containing a test case."                                                                               )
