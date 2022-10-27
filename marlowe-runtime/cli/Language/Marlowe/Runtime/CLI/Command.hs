{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
module Language.Marlowe.Runtime.CLI.Command
  where

import qualified Colog
import Control.Concurrent.STM (STM)
import Control.Exception (Exception, SomeException, catch, throw)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (runReaderT)
import qualified Data.ByteString.Lazy as LB
import Data.Foldable (asum)
import qualified Data.Text as T
import Language.Marlowe.Protocol.Sync.Client (marloweSyncClientPeer)
import Language.Marlowe.Protocol.Sync.Codec (codecMarloweSync)
import Language.Marlowe.Runtime.CLI.Command.Add (AddCommand, addCommandParser, runAddCommand)
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
import Language.Marlowe.Runtime.CLI.Command.Ls (LsCommand, lsCommandParser, runLsCommand)
import Language.Marlowe.Runtime.CLI.Command.Rm (RmCommand, rmCommandParser, runRmCommand)
import Language.Marlowe.Runtime.CLI.Command.Submit (SubmitCommand, runSubmitCommand, submitCommandParser)
import Language.Marlowe.Runtime.CLI.Command.Tx (TxCommand)
import Language.Marlowe.Runtime.CLI.Command.Withdraw (WithdrawCommand, runWithdrawCommand, withdrawCommandParser)
import Language.Marlowe.Runtime.CLI.Env (Env(..), RunClient, runClientPeerOverSocket)
import Language.Marlowe.Runtime.CLI.Monad
  (CLI, logDebug, logError, runCLI, runDiscoveryQueryClient, runHistoryJobClient, runHistoryQueryClient, runTxJobClient)
import Language.Marlowe.Runtime.CLI.Option (optParserWithEnvDefault)
import qualified Language.Marlowe.Runtime.CLI.Option as O
import Language.Marlowe.Runtime.CLI.Option.Colog (Verbosity(LogLevel), logActionParser)
import qualified Language.Marlowe.Runtime.Discovery.Api as Discovery
import qualified Language.Marlowe.Runtime.History.Api as History
import Language.Marlowe.Runtime.Logging.Colog (logErrorM)
import Language.Marlowe.Runtime.Transaction.Api (marloweTxCommandSchema)
import Network.Protocol.Job.Client (jobClientPeer)
import qualified Network.Protocol.Job.Client as Job
import Network.Protocol.Job.Codec (codecJob)
import Network.Protocol.Query.Client (queryClientPeer)
import qualified Network.Protocol.Query.Client as Query
import Network.Protocol.Query.Codec (codecQuery)
import Network.Socket
  (AddrInfo(addrAddress), HostName, PortNumber, SocketType(..), addrSocketType, defaultHints, getAddrInfo)
import Network.TypedProtocol (Peer, PeerRole(AsClient))
import Network.TypedProtocol.Codec (Codec)
import Options.Applicative
import System.Exit (exitFailure)

-- | Top-level options for running a command in the Marlowe Runtime CLI.
data Options = Options
  { historyHost :: !HostName
  , historyCommandPort :: !PortNumber
  , historyQueryPort :: !PortNumber
  , historySyncPort :: !PortNumber
  , discoveryHost :: !HostName
  , discoveryQueryPort :: !PortNumber
  , txHost :: !HostName
  , txCommandPort :: !PortNumber
  , cmd :: !Command
  , logAction :: !(Colog.LogAction IO Colog.Message)
  }

-- | A command for the Marlowe Runtime CLI.
data Command
  = Add AddCommand
  | Apply (TxCommand ApplyCommand)
  | Create (TxCommand CreateCommand)
  | Log LogCommand
  | Ls LsCommand
  | Rm RmCommand
  | Submit SubmitCommand
  | Withdraw (TxCommand WithdrawCommand)

-- | Read options from the environment and the command line.
getOptions :: IO Options
getOptions = do
  historyHostParser <- optParserWithEnvDefault O.historyHost
  historyCommandPortParser <- optParserWithEnvDefault O.historyCommandPort
  historyQueryPortParser <- optParserWithEnvDefault O.historyQueryPort
  historySyncPortParser <- optParserWithEnvDefault O.historySyncPort
  discoveryHostParser <- optParserWithEnvDefault O.discoveryHost
  discoveryQueryPortParser <- optParserWithEnvDefault O.discoveryQueryPort
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
      <*> historyCommandPortParser
      <*> historyQueryPortParser
      <*> historySyncPortParser
      <*> discoveryHostParser
      <*> discoveryQueryPortParser
      <*> txHostParser
      <*> txCommandPortParser
      <*> commandParser
      <*> logActionParser (LogLevel Colog.Error)
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
  discoveryQueryAddr <- resolve discoveryHost discoveryQueryPort
  txJobAddr <- resolve txHost txCommandPort

  let
    onHandshake name (Left _) = do
      logError . T.pack $ name <> " server handshake failure"
      liftIO exitFailure
    onHandshake name _ = do
      logDebug . T.pack $ name <> " server handshake succeeded"
      pure ()

    cli' = do
      runHistoryJobClient (Job.doHandshake History.commandSchema) >>= onHandshake "History Job"
      runHistoryQueryClient (Query.doHandshake History.querySchema) >>= onHandshake "History Query"
      -- FIXME: add handshake to marlowe protocol
      -- runHistorySyncClient (ChainSync.doHandshake ChainSync.marloweChainSync) >>= onHandshake "History sycn"
      runTxJobClient (Job.doHandshake marloweTxCommandSchema) >>= onHandshake "Marlowe Tx Job"
      runDiscoveryQueryClient (Query.doHandshake Discovery.querySchema) >>= onHandshake "Marlowe Discovery"
      cli

  runReaderT (runCLI cli') Env
    { envRunHistoryJobClient = runClientPeerOverSocket' "History Job" historyJobAddr codecJob jobClientPeer
    , envRunHistoryQueryClient = runClientPeerOverSocket' "History Query" historyQueryAddr codecQuery queryClientPeer
    , envRunHistorySyncClient = runClientPeerOverSocket' "History Sync" historySyncAddr codecMarloweSync marloweSyncClientPeer
    , envRunTxJobClient = runClientPeerOverSocket' "Marlowe Tx Job" txJobAddr codecJob jobClientPeer
    , envRunDiscoveryQueryClient = runClientPeerOverSocket' "Marlowe Discovery" discoveryQueryAddr codecQuery queryClientPeer
    , logAction = logAction
    , sigInt
    }
  where
    resolve host port =
      head <$> getAddrInfo (Just defaultHints { addrSocketType = Stream }) (Just host) (Just $ show port)

    runClientPeerOverSocket'
      :: Exception ex
      => String -- ^ Client failure stderr extra message
      -> AddrInfo -- ^ Socket address to connect to
      -> Codec protocol ex IO LB.ByteString -- ^ A codec for the protocol
      -> (forall a. client IO a -> Peer protocol 'AsClient st IO a) -- ^ Interpret the client as a protocol peer
      -> RunClient IO client
    runClientPeerOverSocket' name addr codec clientToPeer client = do
      let
        run = runClientPeerOverSocket addr codec clientToPeer client
      run `catch` \(err :: SomeException)-> do
        logErrorM logAction . T.pack $ (name <> " client (server at: " <> show (addrAddress addr) <> ") failure: " <> show err)
        -- hPutStrLn stderr (name <> " client (server at: " <> show (addrAddress addr) <> ") failure: " <> show err)
        throw err
