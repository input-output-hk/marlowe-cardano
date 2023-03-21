{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}

module Main
  where

import Control.Concurrent.Component
import Control.Exception (bracket)
import Control.Monad ((<=<))
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.String (fromString)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.Text.Lazy.IO as TL
import Data.UUID.V4 (nextRandom)
import Hasql.Pool (UsageError(..))
import qualified Hasql.Pool as Pool
import qualified Hasql.Session as Session
import Language.Marlowe.Protocol.HeaderSync.Server (marloweHeaderSyncServerPeer)
import Language.Marlowe.Protocol.Sync.Server (marloweSyncServerPeer)
import Language.Marlowe.Runtime.Sync (SyncDependencies(..), sync)
import Language.Marlowe.Runtime.Sync.Database (hoistDatabaseQueries, logDatabaseQueries)
import qualified Language.Marlowe.Runtime.Sync.Database.PostgreSQL as Postgres
import Logging (RootSelector(..), defaultRootSelectorLogConfig, getRootSelectorConfig)
import Network.Protocol.Connection (SomeConnectionSource(..), logConnectionSource)
import Network.Protocol.Driver (TcpServerDependencies(..), tcpServer)
import Network.Protocol.Handshake.Server (handshakeConnectionSource)
import Network.Socket (HostName, PortNumber)
import Observe.Event.Backend (injectSelector, narrowEventBackend)
import Observe.Event.Component (LoggerDependencies(..), logger)
import Options.Applicative
  ( auto
  , execParser
  , fullDesc
  , header
  , help
  , helper
  , info
  , infoOption
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

main :: IO ()
main = run =<< getOptions

run :: Options -> IO ()
run Options{..} = bracket (Pool.acquire 100 (Just 5000000) (fromString databaseUri)) Pool.release $
  runComponent_ proc pool -> do
    eventBackend <- logger -< LoggerDependencies
      { configFilePath = logConfigFile
      , getSelectorConfig = getRootSelectorConfig
      , newRef = nextRandom
      , writeText = TL.hPutStr stderr
      , injectConfigWatcherSelector = injectSelector ConfigWatcher
      }

    marloweSyncSource <- tcpServer -< TcpServerDependencies
      { host
      , port = marloweSyncPort
      , toPeer = marloweSyncServerPeer
      }

    headerSyncSource <- tcpServer -< TcpServerDependencies
      { host
      , port = marloweHeaderSyncPort
      , toPeer = marloweHeaderSyncServerPeer
      }

    querySource <- tcpServer -< TcpServerDependencies
      { host
      , port = queryPort
      , toPeer = id
      }

    sync -< SyncDependencies
      { databaseQueries = logDatabaseQueries (narrowEventBackend (injectSelector Database) eventBackend) $ hoistDatabaseQueries
            (either throwUsageError pure <=< Pool.use pool)
            Postgres.databaseQueries
      , syncSource = SomeConnectionSource
          $ logConnectionSource (narrowEventBackend (injectSelector MarloweSyncServer) eventBackend)
          $ handshakeConnectionSource marloweSyncSource
      , headerSyncSource = SomeConnectionSource
          $ logConnectionSource (narrowEventBackend (injectSelector MarloweHeaderSyncServer) eventBackend)
          $ handshakeConnectionSource headerSyncSource
      , querySource = SomeConnectionSource
          $ logConnectionSource (narrowEventBackend (injectSelector MarloweQueryServer) eventBackend)
          $ handshakeConnectionSource querySource
      , httpPort = fromIntegral httpPort
      }
  where
    throwUsageError (ConnectionUsageError err)                       = error $ show err
    throwUsageError (SessionUsageError (Session.QueryError _ _ err)) = error $ show err
    throwUsageError AcquisitionTimeoutUsageError                     = error "hasql-timeout"


data Options = Options
  { databaseUri :: String
  , marloweSyncPort :: PortNumber
  , marloweHeaderSyncPort :: PortNumber
  , queryPort :: PortNumber
  , host :: HostName
  , logConfigFile :: Maybe FilePath
  , httpPort :: PortNumber
  }

getOptions :: IO Options
getOptions = execParser $ info (helper <*> printLogConfigOption <*> parser) infoMod
  where
    printLogConfigOption = infoOption
      (T.unpack $ decodeUtf8 $ encodePretty defaultRootSelectorLogConfig)
      (long "print-log-config" <> help "Print the default log configuration.")

    parser = Options
      <$> databaseUriParser
      <*> marloweSyncPortParser
      <*> marloweHeaderSyncPortParser
      <*> queryPort
      <*> hostParser
      <*> logConfigFileParser
      <*> httpPortParser

    databaseUriParser = strOption $ mconcat
      [ long "database-uri"
      , short 'd'
      , metavar "DATABASE_URI"
      , help "URI of the database where the contract information is saved."
      ]

    marloweSyncPortParser = option auto $ mconcat
      [ long "sync-port"
      , value 3724
      , metavar "PORT_NUMBER"
      , help "The port number to run the sync protocol on."
      , showDefault
      ]

    marloweHeaderSyncPortParser = option auto $ mconcat
      [ long "header-sync-port"
      , value 3725
      , metavar "PORT_NUMBER"
      , help "The port number to run the header sync protocol on."
      , showDefault
      ]

    queryPort = option auto $ mconcat
      [ long "query-port"
      , value 3726
      , metavar "PORT_NUMBER"
      , help "The port number to run the query protocol on."
      , showDefault
      ]

    hostParser = strOption $ mconcat
      [ long "host"
      , short 'h'
      , value "127.0.0.1"
      , metavar "HOST_NAME"
      , help "The host name to run the server on."
      , showDefault
      ]

    logConfigFileParser = optional $ strOption $ mconcat
      [ long "log-config-file"
      , metavar "FILE_PATH"
      , help "The logging configuration JSON file."
      ]

    httpPortParser = option auto $ mconcat
      [ long "http-port"
      , metavar "PORT_NUMBER"
      , help "Port number to serve the http healthcheck API on"
      , value 8080
      , showDefault
      ]

    infoMod = mconcat
      [ fullDesc
      , progDesc "Contract synchronization and query service for Marlowe Runtime"
      , header "marlowe-sync : a contract synchronization and query service for the Marlowe Runtime."
      ]
