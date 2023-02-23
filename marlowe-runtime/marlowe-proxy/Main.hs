{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}

module Main
  where

import Control.Concurrent.Component
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Text.Lazy.IO as TL
import Data.UUID.V4 (nextRandom)
import Language.Marlowe.Protocol.Server (marloweServerPeer)
import Language.Marlowe.Runtime.CLI.Option (optParserWithEnvDefault)
import qualified Language.Marlowe.Runtime.CLI.Option as O
import Language.Marlowe.Runtime.Proxy
import Logging (RootSelector(..), getRootSelectorConfig)
import Network.Channel (hoistChannel, socketAsChannel)
import Network.Protocol.Codec (BinaryMessage)
import Network.Protocol.Connection (SomeConnectionSource(..), logConnectionSource)
import Network.Protocol.Driver (TcpServerDependencies(..), mkDriver, tcpServer)
import Network.Protocol.Handshake.Server (handshakeConnectionSource)
import Network.Protocol.Handshake.Types (Handshake)
import Network.Socket
  ( AddrInfo(addrAddress, addrSocketType)
  , HostName
  , PortNumber
  , SocketType(Stream)
  , close
  , connect
  , defaultHints
  , getAddrInfo
  , openSocket
  )
import Network.TypedProtocol (Driver)
import Observe.Event.Backend (hoistEventBackend, narrowEventBackend, newOnceFlagMVar)
import Observe.Event.Component (LoggerDependencies(..), logger)
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
  , optional
  , progDesc
  , short
  , showDefault
  , strOption
  , value
  )
import System.IO (stderr)
import UnliftIO.Resource (allocate)

main :: IO ()
main = run =<< getOptions

run :: Options -> IO ()
run = runComponent_ proc Options{..} -> do
  eventBackend <- logger -< LoggerDependencies
    { configFilePath = logConfigFile
    , getSelectorConfig = getRootSelectorConfig
    , newRef = nextRandom
    , newOnceFlag = newOnceFlagMVar
    , writeText = TL.hPutStr stderr
    , injectConfigWatcherSelector = ConfigWatcher
    }

  connectionSource <- tcpServer -< TcpServerDependencies
    { toPeer = marloweServerPeer
    , ..
    }

  proxy -< ProxyDependencies
    { getMarloweSyncDriver = driverFactory syncHost marloweSyncPort
    , getMarloweHeaderSyncDriver = driverFactory syncHost marloweHeaderSyncPort
    , getMarloweQueryDriver = driverFactory syncHost marloweQueryPort
    , getTxJobDriver = driverFactory txHost txPort
    , connectionSource = SomeConnectionSource
        $ logConnectionSource (hoistEventBackend liftIO $ narrowEventBackend MarloweServer eventBackend)
        $ handshakeConnectionSource connectionSource
    }

driverFactory
  :: BinaryMessage ps
  => HostName
  -> PortNumber
  -> ServerM (Driver (Handshake ps) (Maybe ByteString) ServerM)
driverFactory host port = do
  addr <- liftIO $ head <$> getAddrInfo
    (Just defaultHints { addrSocketType = Stream })
    (Just host)
    (Just $ show port)
  (_, socket) <- WrappedUnliftIO $ allocate (openSocket addr) close
  liftIO $ connect socket $ addrAddress addr
  pure $ mkDriver $ hoistChannel liftIO $ socketAsChannel socket

data Options = Options
  { host :: HostName
  , port :: PortNumber
  , syncHost :: HostName
  , marloweSyncPort :: PortNumber
  , marloweHeaderSyncPort :: PortNumber
  , marloweQueryPort :: PortNumber
  , txHost :: HostName
  , txPort :: PortNumber
  , logConfigFile :: Maybe FilePath
  }

getOptions :: IO Options
getOptions = do
  syncHostParser <- optParserWithEnvDefault O.syncHost
  marloweSyncPortParser <- optParserWithEnvDefault O.syncSyncPort
  marloweHeaderSyncPortParser <- optParserWithEnvDefault O.syncHeaderPort
  marloweQueryPortParser <- optParserWithEnvDefault O.syncQueryPort
  txHostParser <- optParserWithEnvDefault O.txHost
  txPortParser <- optParserWithEnvDefault O.txCommandPort
  execParser $ info
    ( helper <*>
      ( Options
          <$> hostParser
          <*> portParser
          <*> syncHostParser
          <*> marloweSyncPortParser
          <*> marloweHeaderSyncPortParser
          <*> marloweQueryPortParser
          <*> txHostParser
          <*> txPortParser
          <*> logConfigFileParser
      )
    )
    infoMod
  where
    hostParser = strOption $ mconcat
      [ long "host"
      , short 'h'
      , value "127.0.0.1"
      , metavar "HOST_NAME"
      , help "The host name to run the server on."
      , showDefault
      ]

    portParser = option auto $ mconcat
      [ long "port"
      , short 'p'
      , value 3700
      , metavar "PORT_NUMBER"
      , help "The port number to run the server on."
      , showDefault
      ]

    logConfigFileParser = optional $ strOption $ mconcat
      [ long "log-config-file"
      , metavar "FILE_PATH"
      , help "The logging configuration JSON file."
      ]

    infoMod = mconcat
      [ fullDesc
      , progDesc "Contract synchronization and query service for Marlowe Runtime"
      , header "marlowe-sync : a contract synchronization and query service for the Marlowe Runtime."
      ]
