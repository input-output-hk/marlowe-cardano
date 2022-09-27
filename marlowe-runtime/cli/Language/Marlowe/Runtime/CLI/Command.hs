module Language.Marlowe.Runtime.CLI.Command
  where

import Control.Concurrent.STM (STM)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Foldable (asum)
import Language.Marlowe.Protocol.Sync.Client (marloweSyncClientPeer)
import Language.Marlowe.Protocol.Sync.Codec (codecMarloweSync)
import Language.Marlowe.Runtime.CLI.Command.Add (AddCommand, addCommandParser, runAddCommand)
import Language.Marlowe.Runtime.CLI.Command.Apply
  (ApplyCommand, applyCommandParser, chooseCommandParser, depositCommandParser, notifyCommandParser, runApplyCommand)
import Language.Marlowe.Runtime.CLI.Command.Create (CreateCommand, createCommandParser, runCreateCommand)
import Language.Marlowe.Runtime.CLI.Command.Log (LogCommand, logCommandParser, runLogCommand)
import Language.Marlowe.Runtime.CLI.Command.Ls (LsCommand, lsCommandParser, runLsCommand)
import Language.Marlowe.Runtime.CLI.Command.Rm (RmCommand, rmCommandParser, runRmCommand)
import Language.Marlowe.Runtime.CLI.Command.Submit (SubmitCommand, runSubmitCommand, submitCommandParser)
import Language.Marlowe.Runtime.CLI.Command.Withdraw (WithdrawCommand, runWithdrawCommand, withdrawCommandParser)
import Language.Marlowe.Runtime.CLI.Env (Env(..), runClientPeerOverSocket)
import Language.Marlowe.Runtime.CLI.Monad (CLI, runCLI)
import Language.Marlowe.Runtime.CLI.Option (optParserWithEnvDefault)
import qualified Language.Marlowe.Runtime.CLI.Option as O
import Network.Protocol.Job.Client (jobClientPeer)
import Network.Protocol.Job.Codec (codecJob)
import Network.Protocol.Query.Client (queryClientPeer)
import Network.Protocol.Query.Codec (codecQuery)
import Network.Socket (HostName, PortNumber, SocketType(..), addrSocketType, defaultHints, getAddrInfo)
import Options.Applicative

-- | Top-level options for running a command in the Marlowe Runtime CLI.
data Options = Options
  { historyHost :: !HostName
  , historyCommandPort :: !PortNumber
  , historyQueryPort :: !PortNumber
  , historySyncPort :: !PortNumber
  , txHost :: !HostName
  , txCommandPort :: !PortNumber
  , cmd :: !Command
  }

-- | A command for the Marlowe Runtime CLI.
data Command
  = Add AddCommand
  | Apply ApplyCommand
  | Create CreateCommand
  | Log LogCommand
  | Ls LsCommand
  | Rm RmCommand
  | Submit SubmitCommand
  | Withdraw WithdrawCommand

-- | Read options from the environment and the command line.
getOptions :: IO Options
getOptions = do
  historyHostParser <- optParserWithEnvDefault O.historyHost
  historyCommandPortParser <- optParserWithEnvDefault O.historyCommandPort
  historyQueryPortParser <- optParserWithEnvDefault O.historyQueryPort
  historySyncPortParser <- optParserWithEnvDefault O.historySyncPort
  txHostParser <- optParserWithEnvDefault O.txHost
  txCommandPortParser <- optParserWithEnvDefault O.txCommandPort
  let
    commandParser = asum
      [ hsubparser $ mconcat
          [ commandGroup "Contract history commands"
          , command "add" $ Add <$> addCommandParser
          , command "log" $ Log <$> logCommandParser
          , command "ls" $ Ls <$> lsCommandParser
          , command "rm" $ Rm <$> rmCommandParser
          ]
      , hsubparser $ mconcat
          [ commandGroup "Contract transaction commands"
          , command "apply" $ Apply <$> applyCommandParser
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
      <*> historyCommandPortParser
      <*> historyQueryPortParser
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
  Add cmd -> runAddCommand cmd
  Apply cmd -> runApplyCommand cmd
  Create cmd -> runCreateCommand cmd
  Log cmd -> runLogCommand cmd
  Ls cmd -> runLsCommand cmd
  Rm cmd -> runRmCommand cmd
  Submit cmd -> runSubmitCommand cmd
  Withdraw cmd -> runWithdrawCommand cmd

-- | Interpret a CLI action in IO using the provided options.
runCLIWithOptions :: STM () -> Options -> CLI a -> IO a
runCLIWithOptions sigInt Options{..} cli = do
  historyJobAddr <-  resolve historyHost historyCommandPort
  historyQueryAddr <- resolve historyHost historyQueryPort
  historySyncAddr <- resolve historyHost historySyncPort
  txJobAddr <- resolve txHost txCommandPort
  runReaderT (runCLI cli) Env
    { envRunHistoryJobClient = runClientPeerOverSocket historyJobAddr codecJob jobClientPeer
    , envRunHistoryQueryClient = runClientPeerOverSocket historyQueryAddr codecQuery queryClientPeer
    , envRunHistorySyncClient = runClientPeerOverSocket historySyncAddr codecMarloweSync marloweSyncClientPeer
    , envRunTxJobClient = runClientPeerOverSocket txJobAddr codecJob jobClientPeer
    , sigInt
    }
  where
    resolve host port =
      head <$> getAddrInfo (Just defaultHints { addrSocketType = Stream }) (Just host) (Just $ show port)
