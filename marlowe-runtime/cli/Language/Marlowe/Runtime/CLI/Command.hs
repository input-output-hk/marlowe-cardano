{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.CLI.Command
  where

import Control.Concurrent.STM (STM)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Foldable (asum)
import Language.Marlowe.Protocol.Sync.Client (marloweSyncClientPeer)
import Language.Marlowe.Runtime.CLI.Command.Apply
  ( ApplyCommand
  , advanceCommandParser
  , applyCommandParser
  , chooseCommandParser
  , depositCommandParser
  , notifyCommandParser
  , runApplyCommand
  )
import Language.Marlowe.Runtime.CLI.Command.Create (CreateCommand, createCommandParser, runCreateCommand)
import Language.Marlowe.Runtime.CLI.Command.Log (LogCommand, logCommandParser, runLogCommand)
import Language.Marlowe.Runtime.CLI.Command.Submit (SubmitCommand, runSubmitCommand, submitCommandParser)
import Language.Marlowe.Runtime.CLI.Command.Tx (TxCommand)
import Language.Marlowe.Runtime.CLI.Command.Withdraw (WithdrawCommand, runWithdrawCommand, withdrawCommandParser)
import Language.Marlowe.Runtime.CLI.Env (Env(..))
import Language.Marlowe.Runtime.CLI.Monad (CLI, runCLI)
import Language.Marlowe.Runtime.CLI.Option (optParserWithEnvDefault)
import qualified Language.Marlowe.Runtime.CLI.Option as O
import Network.Protocol.Driver (SomeConnector(..), tcpClient)
import Network.Protocol.Job.Client (jobClientPeer)
import Network.Socket (HostName, PortNumber)
import Options.Applicative

-- | Top-level options for running a command in the Marlowe Runtime CLI.
data Options = Options
  { historyHost :: !HostName
  , historySyncPort :: !PortNumber
  , txHost :: !HostName
  , txCommandPort :: !PortNumber
  , cmd :: !Command
  }

-- | A command for the Marlowe Runtime CLI.
data Command
  = Apply (TxCommand ApplyCommand)
  | Create (TxCommand CreateCommand)
  | Log LogCommand
  | Submit SubmitCommand
  | Withdraw (TxCommand WithdrawCommand)

-- | Read options from the environment and the command line.
getOptions :: IO Options
getOptions = do
  historyHostParser <- optParserWithEnvDefault O.historyHost
  historySyncPortParser <- optParserWithEnvDefault O.historySyncPort
  txHostParser <- optParserWithEnvDefault O.txHost
  txCommandPortParser <- optParserWithEnvDefault O.txCommandPort
  let
    commandParser = asum
      [ hsubparser $ mconcat
          [ commandGroup "Contract history commands"
          , command "log" $ Log <$> logCommandParser
          ]
      , hsubparser $ mconcat
          [ commandGroup "Contract transaction commands"
          , command "apply" $ Apply <$> applyCommandParser
          , command "advance" $ Apply <$> advanceCommandParser
          , command "deposit" $ Apply <$> depositCommandParser
          , command "choose" $ Apply <$> chooseCommandParser
          , command "notify" $ Apply <$> notifyCommandParser
          , command "create" $ Create <$> createCommandParser
          , command "withdraw" $ Withdraw <$> withdrawCommandParser
          ]
      , hsubparser $ mconcat
          [ commandGroup "Low level commands"
          , command "submit" $ Submit <$> submitCommandParser
          ]
      ]
    parser = Options
      <$> historyHostParser
      <*> historySyncPortParser
      <*> txHostParser
      <*> txCommandPortParser
      <*> commandParser
    infoMod = mconcat
      [ fullDesc
      , progDesc "Command line interface for managing Marlowe smart contracts on Cardano."
      ]
  execParser $ info (helper <*> parser) infoMod

-- | Run a command.
runCommand :: Command -> CLI ()
runCommand = \case
  Apply cmd -> runApplyCommand cmd
  Create cmd -> runCreateCommand cmd
  Log cmd -> runLogCommand cmd
  Submit cmd -> runSubmitCommand cmd
  Withdraw cmd -> runWithdrawCommand cmd

-- | Interpret a CLI action in IO using the provided options.
runCLIWithOptions :: STM () -> Options -> CLI a -> IO a
runCLIWithOptions sigInt Options{..} cli = runReaderT (runCLI cli) Env
  { marloweSyncConnector = SomeConnector $ tcpClient historyHost historySyncPort marloweSyncClientPeer
  , txJobConnector = SomeConnector $ tcpClient txHost txCommandPort jobClientPeer
  , sigInt
  }
