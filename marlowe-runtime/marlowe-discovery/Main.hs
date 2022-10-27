module Main
  where

import qualified Colog
import Control.Concurrent.STM (atomically)
import Control.Exception (bracket, bracketOnError, catch, throw, throwIO)
import Control.Exception.Base (SomeException)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Data.Void (vacuous)
import Language.Marlowe.Protocol.HeaderSync.Codec (codecMarloweHeaderSync)
import Language.Marlowe.Protocol.HeaderSync.Server (marloweHeaderSyncServerPeer)
import Language.Marlowe.Runtime.CLI.Option.Colog (Verbosity(LogLevel), logActionParser)
import Language.Marlowe.Runtime.ChainSync.Api (RuntimeChainSeekClient, WithGenesis(..), runtimeChainSeekCodec)
import qualified Language.Marlowe.Runtime.ChainSync.Api as ChainSync
import Language.Marlowe.Runtime.Discovery (Discovery(..), DiscoveryDependencies(..), mkDiscovery)
import Language.Marlowe.Runtime.Discovery.QueryServer (RunQueryServer(..))
import Language.Marlowe.Runtime.Discovery.SyncServer (RunSyncServer(..))
import Language.Marlowe.Runtime.Logging.Colog (logDebugM, logErrorM)
import Network.Channel (socketAsChannel)
import Network.Protocol.ChainSeek.Client (chainSeekClientPeer)
import qualified Network.Protocol.ChainSeek.Client as ChainSeek
import Network.Protocol.Driver (mkDriver)
import Network.Protocol.Query.Codec (codecQuery)
import Network.Protocol.Query.Server (queryServerPeer)
import Network.Socket
  ( AddrInfo(..)
  , AddrInfoFlag(..)
  , HostName
  , PortNumber
  , SockAddr
  , Socket
  , SocketOption(..)
  , SocketType(..)
  , accept
  , bind
  , close
  , connect
  , defaultHints
  , getAddrInfo
  , listen
  , openSocket
  , setCloseOnExecIfNeeded
  , setSocketOption
  , withFdSocket
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
import System.Exit (exitFailure)

main :: IO ()
main = run =<< getOptions

clientHints :: AddrInfo
clientHints = defaultHints { addrSocketType = Stream }

run :: Options -> IO ()
run Options{logAction=mainLogAction, ..} = withSocketsDo do
  queryAddr <- resolve queryPort
  syncAddr <- resolve syncPort

  Colog.withBackgroundLogger Colog.defCapacity mainLogAction \logAction -> do
    let
      withNetworkClient :: forall a. String -> AddrInfo -> (Socket -> IO a) -> IO a
      withNetworkClient name serverAddr action = do
        let open = openClient serverAddr
        bracket open close action `catch` \(err :: SomeException) -> do
          let
            serverInfo = show (addrAddress serverAddr)
          logErrorM logAction . T.pack $
            name <> " client (server at " <> serverInfo <> ") failure: " <> show err <> " ."
          throw err

    bracket (openServer queryAddr) close \querySocket -> do
      bracket (openServer syncAddr) close \syncSocket -> do
        let
          connectToChainSeek :: forall a. RuntimeChainSeekClient IO a -> IO a
          connectToChainSeek client = do
            chainSeekAddr <- head <$> getAddrInfo (Just clientHints) (Just chainSeekHost) (Just $ show chainSeekPort)
            withNetworkClient "Chain Sync" chainSeekAddr \chainSeekSocket -> do
              let driver = mkDriver throwIO runtimeChainSeekCodec $ socketAsChannel chainSeekSocket
              let peer = chainSeekClientPeer Genesis client
              fst <$> runPeerWithDriver driver peer (startDState driver)

          acceptRunQueryServer = do
            (conn, _ :: SockAddr) <- accept querySocket
            let driver = mkDriver throwIO codecQuery $ socketAsChannel conn
            pure $ RunQueryServer \server -> do
              let peer = queryServerPeer server
              fst <$> runPeerWithDriver driver peer (startDState driver)

          acceptRunSyncServer = do
            (conn, _ :: SockAddr) <- accept syncSocket
            let driver = mkDriver throwIO codecMarloweHeaderSync $ socketAsChannel conn
            pure $ RunSyncServer \server -> do
              let peer = marloweHeaderSyncServerPeer server
              fst <$> runPeerWithDriver driver peer (startDState driver)

        let pageSize = 1024 -- TODO move to config with a default
        Discovery{..} <- atomically $ mkDiscovery DiscoveryDependencies{..}
        let
          onHandshake name (Left _) = do
            logErrorM logAction . T.pack $ name <> " server handshake failure"
            liftIO exitFailure
          onHandshake name _ = do
            logDebugM logAction . T.pack $ name <> " server handshake succeeded"

        connectToChainSeek (ChainSeek.doHandshake ChainSync.moveSchema) >>= onHandshake "Chain Sync"
        -- runHistorySyncClient (ChainSync.doHandshake ChainSync.marloweChainSync) >>= onHandshake "History sycn"
        vacuous runDiscovery
  where
    openClient addr = bracketOnError (openSocket addr) close \sock -> do
      connect sock $ addrAddress addr
      pure sock

    openServer addr = bracketOnError (openSocket addr) close \socket -> do
      setSocketOption socket ReuseAddr 1
      withFdSocket socket setCloseOnExecIfNeeded
      bind socket $ addrAddress addr
      listen socket 2048
      return socket

    resolve p = do
      let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }
      head <$> getAddrInfo (Just hints) (Just host) (Just $ show p)

data Options = Options
  { chainSeekPort      :: PortNumber
  , chainSeekQueryPort :: PortNumber
  , queryPort          :: PortNumber
  , syncPort           :: PortNumber
  , chainSeekHost      :: HostName
  , host               :: HostName
  , logAction          :: Colog.LogAction IO Colog.Message
  }

getOptions :: IO Options
getOptions = execParser $ info (helper <*> parser) infoMod
  where
    parser = Options
      <$> chainSeekPortParser
      <*> chainSeekQueryPortParser
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

    queryPortParser = option auto $ mconcat
      [ long "query-port"
      , value 3721
      , metavar "PORT_NUMBER"
      , help "The port number to run the query server on."
      , showDefault
      ]

    syncPortParser = option auto $ mconcat
      [ long "sync-port"
      , value 3722
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
      , help "The host name to run the discovery server on."
      , showDefault
      ]

    infoMod = mconcat
      [ fullDesc
      , progDesc "Contract discovery service for Marlowe Runtime"
      , header "marlowe-discovery : a contract discovery service for the Marlowe Runtime."
      ]
