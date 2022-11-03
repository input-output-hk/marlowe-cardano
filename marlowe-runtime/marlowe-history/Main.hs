{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Main
  where

import Colog (logDebug)
import qualified Colog
import Control.Concurrent.STM (atomically)
import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Data.Void (Void)
import GHC.Stack (HasCallStack)
import Language.Marlowe.Protocol.Sync.Codec (codecMarloweSync)
import Language.Marlowe.Protocol.Sync.Server (marloweSyncServerPeer)
import Language.Marlowe.Runtime.CLI.Option.Colog (Verbosity(LogLevel), logActionParser)
import Language.Marlowe.Runtime.ChainSync.Api
  (ChainSyncQuery(..), RuntimeChainSeekClient, WithGenesis(..), runtimeChainSeekCodec)
import qualified Language.Marlowe.Runtime.ChainSync.Api as ChainSync
import Language.Marlowe.Runtime.History (History(..), HistoryDependencies(..), mkHistory)
import Language.Marlowe.Runtime.History.Api (historyJobCodec, historyQueryCodec)
import Language.Marlowe.Runtime.History.JobServer (RunJobServer(RunJobServer))
import Language.Marlowe.Runtime.History.QueryServer (RunQueryServer(RunQueryServer))
import Language.Marlowe.Runtime.History.Store (hoistHistoryQueries)
import Language.Marlowe.Runtime.History.Store.Memory (mkHistoryQueriesInMemory)
import Language.Marlowe.Runtime.History.SyncServer (RunSyncServer(..))
import Language.Marlowe.Runtime.Logging.Colog.LogIO (LogIO, dieLogIO, runLogIO, throwLogIO)
import Language.Marlowe.Runtime.Logging.Colog.LogIO.Network
  (ProtocolName(ProtocolName), withClientSocket, withServerSocket)
import Network.Channel (hoistChannel, socketAsChannel)
import Network.Protocol.ChainSeek.Client (chainSeekClientPeer)
import Network.Protocol.Driver (hoistDriver, mkDriver)
import Network.Protocol.Job.Server (jobServerPeer)
import Network.Protocol.Query.Client (liftQuery', queryClientPeer)
import Network.Protocol.Query.Codec (codecQuery)
import Network.Protocol.Query.Server (queryServerPeer)
import Network.Protocol.SchemaVersion (SchemaVersion)
import Network.Socket
  ( AddrInfo(..)
  , AddrInfoFlag(..)
  , HostName
  , PortNumber
  , SockAddr
  , SocketType(..)
  , accept
  , defaultHints
  , getAddrInfo
  , withSocketsDo
  )
import Network.TypedProtocol (runPeerWithDriver, startDState)
import Options.Applicative
  ( auto
  , execParser
  , fullDesc
  , header
  , help
  , helper
  , info
  , long
  , metavar
  , option
  , progDesc
  , short
  , showDefault
  , strOption
  , value
  )

main :: IO ()
main = run =<< getOptions

clientHints :: AddrInfo
clientHints = defaultHints { addrSocketType = Stream }

run :: HasCallStack => Options -> IO ()
run Options{logAction=mainLogAction,..} = withSocketsDo do
  jobAddr <- resolve commandPort
  queryAddr <- resolve queryPort
  syncAddr <- resolve syncPort
  Colog.withBackgroundLogger Colog.defCapacity mainLogAction \logAction ->
    runLogIO logAction $ withServerSocket (ProtocolName "History Job") jobAddr \jobSocket ->
      withServerSocket (ProtocolName "History Query") queryAddr \querySocket ->
        withServerSocket (ProtocolName "History Sync") syncAddr \syncSocket -> do
          slotConfig <- queryChainSync GetSlotConfig
          securityParameter <- queryChainSync GetSecurityParameter
          let
            connectToChainSeek :: forall a. RuntimeChainSeekClient LogIO a -> LogIO a
            connectToChainSeek client = do
              chainSeekAddr <- liftIO $ head <$> getAddrInfo (Just clientHints) (Just chainSeekHost) (Just $ show chainSeekPort)
              withClientSocket (ProtocolName "Chain Sync") chainSeekAddr \chainSeekSocket -> do
                let driver = hoistDriver liftIO $ mkDriver throwIO runtimeChainSeekCodec $ socketAsChannel chainSeekSocket
                let peer = chainSeekClientPeer Genesis client
                fst <$> runPeerWithDriver driver peer (startDState driver)

            acceptRunJobServer = liftIO $ do
              (conn, _ :: SockAddr) <- accept jobSocket
              let driver = hoistDriver liftIO $ mkDriver throwIO historyJobCodec $ socketAsChannel conn
              pure $ RunJobServer \server -> do
                let peer = jobServerPeer server
                logDebug "Starting History Job server"
                fst <$> runPeerWithDriver driver peer (startDState driver)

            acceptRunQueryServer = do
              (conn, _ :: SockAddr) <- liftIO $ accept querySocket
              let driver = hoistDriver liftIO $ mkDriver throwIO historyQueryCodec $ socketAsChannel conn
              pure $ RunQueryServer \server -> do
                let peer = queryServerPeer server
                logDebug "Starting History Query server"
                fst <$> runPeerWithDriver driver peer (startDState driver)

            acceptRunSyncServer = liftIO $ do
              (conn, _ :: SockAddr) <- accept syncSocket
              let driver = hoistDriver liftIO $ mkDriver throwIO codecMarloweSync $ socketAsChannel conn
              pure $ RunSyncServer \server -> do
                let peer = marloweSyncServerPeer server
                logDebug "Starting History Sync server"
                fst <$> runPeerWithDriver driver peer (startDState driver)

          let followerPageSize = 1024 -- TODO move to config with a default
          History{..} <- liftIO $ atomically do
            historyQueries <- hoistHistoryQueries (liftIO . atomically) <$> mkHistoryQueriesInMemory
            mkHistory HistoryDependencies{..}
          runHistory
  where
    resolve p = do
      let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }
      head <$> getAddrInfo (Just hints) (Just host) (Just $ show p)

    queryChainSync :: Show a => Show e => ChainSyncQuery Void e a -> LogIO a
    queryChainSync query = do
      addr <- liftIO $ head <$> getAddrInfo (Just clientHints) (Just chainSeekHost) (Just $ show chainSeekQueryPort)
      withClientSocket (ProtocolName "Chain Sync Query") addr \socket -> do
        let driver = mkDriver (throwLogIO "Protocol failure") codecQuery (hoistChannel liftIO $ socketAsChannel socket)
        let
          onHandshakeFailure :: forall err. SchemaVersion ChainSyncQuery -> LogIO err
          onHandshakeFailure version = dieLogIO . T.pack $
            "ChainSync Query handshake failed (client: "  <> show ChainSync.querySchema <> ", server: " <> show version <> ")"
          onFailure _ = dieLogIO "failed to query chain seek server"
          client = liftQuery' ChainSync.querySchema onHandshakeFailure . pure $ query
          peer = queryClientPeer client
        result <- fst <$> runPeerWithDriver driver peer (startDState driver)
        logDebug . T.pack $ "Chain Sync Query result:" <> show result
        either onFailure pure result

data Options = Options
  { chainSeekPort       :: PortNumber
  , chainSeekQueryPort  :: PortNumber
  , commandPort         :: PortNumber
  , queryPort           :: PortNumber
  , syncPort            :: PortNumber
  , chainSeekHost       :: HostName
  , host                :: HostName
  , logAction           :: Colog.LogAction IO Colog.Message
  }

getOptions :: IO Options
getOptions = execParser $ info (helper <*> parser) infoMod
  where
    parser = Options
      <$> chainSeekPortParser
      <*> chainSeekQueryPortParser
      <*> commandPortParser
      <*> queryPortParser
      <*> syncPortParser
      <*> chainSeekHostParser
      <*> hostParser
      <*> logActionParser (LogLevel Colog.Error)

    chainSeekPortParser = option auto $ mconcat
      [ long "chain-seek-port-number"
      , value 3715
      , metavar "PORT_NUMBER"
      , help "The port number of the chain seek server."
      , showDefault
      ]

    chainSeekQueryPortParser = option auto $ mconcat
      [ long "chain-seek-query-port-number"
      , value 3716
      , metavar "PORT_NUMBER"
      , help "The port number of the chain sync query server."
      , showDefault
      ]

    commandPortParser = option auto $ mconcat
      [ long "command-port"
      , value 3717
      , metavar "PORT_NUMBER"
      , help "The port number to run the job server on."
      , showDefault
      ]

    queryPortParser = option auto $ mconcat
      [ long "query-port"
      , value 3718
      , metavar "PORT_NUMBER"
      , help "The port number to run the query server on."
      , showDefault
      ]

    syncPortParser = option auto $ mconcat
      [ long "sync-port"
      , value 3719
      , metavar "PORT_NUMBER"
      , help "The port number to run the sync server on."
      , showDefault
      ]

    chainSeekHostParser = strOption $ mconcat
      [ long "chain-seek-host"
      , value "127.0.0.1"
      , metavar "HOST_NAME"
      , help "The host name of the chain seek server."
      , showDefault
      ]

    hostParser = strOption $ mconcat
      [ long "host"
      , short 'h'
      , value "127.0.0.1"
      , metavar "HOST_NAME"
      , help "The host name to run the history server on."
      , showDefault
      ]

    infoMod = mconcat
      [ fullDesc
      , progDesc "Contract history service for Marlowe Runtime"
      , header "marlowe-history : a contract history service for the Marlowe Runtime."
      ]
