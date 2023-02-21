{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}

module Main
  where

import Control.Concurrent.Component
import Control.Exception (bracket)
import Control.Monad ((<=<))
import Data.String (fromString)
import qualified Data.Text.Lazy.IO as TL
import Data.Time (secondsToNominalDiffTime)
import Data.UUID.V4 (nextRandom)
import qualified Hasql.Pool as Pool
import qualified Hasql.Session as Session
import Language.Marlowe.Protocol.HeaderSync.Server (marloweHeaderSyncServerPeer)
import Language.Marlowe.Protocol.Sync.Server (marloweSyncServerPeer)
import Language.Marlowe.Runtime.Sync (SyncDependencies(..), sync)
import Language.Marlowe.Runtime.Sync.Database (hoistDatabaseQueries, logDatabaseQueries)
import qualified Language.Marlowe.Runtime.Sync.Database.PostgreSQL as Postgres
import Logging (RootSelector(..), getRootSelectorConfig)
import Network.Protocol.Driver (SomeConnectionSource(..), TcpServerDependencies(..), logConnectionSource, tcpServer)
import Network.Protocol.Handshake.Server (handshakeConnectionSource)
import Network.Socket (HostName, PortNumber)
import Observe.Event.Backend (narrowEventBackend, newOnceFlagMVar)
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

main :: IO ()
main = run =<< getOptions

run :: Options -> IO ()
run Options{..} = bracket (Pool.acquire (100, secondsToNominalDiffTime 5, fromString databaseUri)) Pool.release $
  runComponent_ proc pool -> do
    eventBackend <- logger -< LoggerDependencies
      { configFilePath = logConfigFile
      , getSelectorConfig = getRootSelectorConfig
      , newRef = nextRandom
      , newOnceFlag = newOnceFlagMVar
      , writeText = TL.hPutStr stderr
      , injectConfigWatcherSelector = ConfigWatcher
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
      { databaseQueries = logDatabaseQueries (narrowEventBackend Database eventBackend) $ hoistDatabaseQueries
            (either throwUsageError pure <=< Pool.use pool)
            Postgres.databaseQueries
      , syncSource = SomeConnectionSource
          $ logConnectionSource (narrowEventBackend MarloweSyncServer eventBackend)
          $ handshakeConnectionSource marloweSyncSource
      , headerSyncSource = SomeConnectionSource
          $ logConnectionSource (narrowEventBackend MarloweHeaderSyncServer eventBackend)
          $ handshakeConnectionSource headerSyncSource
      , querySource = SomeConnectionSource
          $ logConnectionSource (narrowEventBackend MarloweQueryServer eventBackend)
          $ handshakeConnectionSource querySource
      }
  where
    throwUsageError (Pool.ConnectionError err)                       = error $ show err
    throwUsageError (Pool.SessionError (Session.QueryError _ _ err)) = error $ show err

data Options = Options
  { databaseUri :: String
  , marloweSyncPort :: PortNumber
  , marloweHeaderSyncPort :: PortNumber
  , queryPort :: PortNumber
  , host :: HostName
  , logConfigFile :: Maybe FilePath
  }

getOptions :: IO Options
getOptions = execParser $ info (helper <*> parser) infoMod
  where
    parser = Options
      <$> databaseUriParser
      <*> marloweSyncPortParser
      <*> marloweHeaderSyncPortParser
      <*> queryPort
      <*> hostParser
      <*> logConfigFileParser

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

    infoMod = mconcat
      [ fullDesc
      , progDesc "Contract synchronization and query service for Marlowe Runtime"
      , header "marlowe-sync : a contract synchronization and query service for the Marlowe Runtime."
      ]
