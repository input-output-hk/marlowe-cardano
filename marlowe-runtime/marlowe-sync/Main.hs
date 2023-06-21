{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Concurrent.Component
import Control.Concurrent.Component.Probes (ProbeServerDependencies(..), probeServer)
import Control.Concurrent.Component.Run (runAppMTraced)
import Control.Exception (bracket)
import Control.Monad ((<=<))
import Control.Monad.Reader (MonadReader(..))
import Data.String (fromString)
import qualified Data.Text as T
import Data.Version (showVersion)
import qualified Database.PostgreSQL.LibPQ as PQ
import Hasql.Connection (withLibPQConnection)
import qualified Hasql.Pool as Pool
import Language.Marlowe.Protocol.HeaderSync.Server (marloweHeaderSyncServerPeer)
import Language.Marlowe.Protocol.Sync.Server (marloweSyncServerPeer)
import Language.Marlowe.Runtime.Sync (MarloweSync(..), SyncDependencies(..), sync)
import Language.Marlowe.Runtime.Sync.Database (hoistDatabaseQueries, logDatabaseQueries)
import qualified Language.Marlowe.Runtime.Sync.Database.PostgreSQL as Postgres
import Logging (RootSelector(..), renderRootSelectorOTel)
import Network.Protocol.Driver (TcpServerDependencies(..))
import Network.Protocol.Driver.Trace (tcpServerTraced)
import Network.Protocol.Query.Server (queryServerPeer)
import Network.Socket (HostName, PortNumber)
import Observe.Event.Backend (injectSelector)
import OpenTelemetry.Trace
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
import Paths_marlowe_runtime (version)
import UnliftIO (liftIO, throwIO)

main :: IO ()
main = run =<< getOptions

run :: Options -> IO ()
run Options{..} = bracket (Pool.acquire 100 (Just 5000000) (fromString databaseUri)) Pool.release \pool -> do
  (dbName, dbUser, dbHost, dbPort) <- either throwIO pure =<< Pool.use pool do
    connection <- ask
    liftIO $ withLibPQConnection connection \conn -> (,,,)
      <$> PQ.db conn
      <*> PQ.user conn
      <*> PQ.host conn
      <*> PQ.port conn
  runAppMTraced instrumentationLibrary (renderRootSelectorOTel dbName dbUser dbHost dbPort) do
    flip runComponent_ () proc _ -> do
      MarloweSync{..} <- sync -< SyncDependencies
        { databaseQueries = logDatabaseQueries $ hoistDatabaseQueries
              (either throwIO pure <=< liftIO . Pool.use pool)
              Postgres.databaseQueries
        }

      tcpServerTraced "marlowe-sync" (injectSelector MarloweSyncServer) -< TcpServerDependencies
        { host
        , port = marloweSyncPort
        , toPeer = marloweSyncServerPeer
        , serverSource = syncServerSource
        }

      tcpServerTraced "marlowe-header-sync" (injectSelector MarloweHeaderSyncServer) -< TcpServerDependencies
        { host
        , port = marloweHeaderSyncPort
        , toPeer = marloweHeaderSyncServerPeer
        , serverSource = headerSyncServerSource
        }

      tcpServerTraced "sync-query" (injectSelector MarloweQueryServer) -< TcpServerDependencies
        { host
        , port = queryPort
        , toPeer = queryServerPeer
        , serverSource = queryServerSource
        }

      probeServer -< ProbeServerDependencies { port = fromIntegral httpPort, .. }
  where
    instrumentationLibrary = InstrumentationLibrary
      { libraryName = "marlowe-sync"
      , libraryVersion = T.pack $ showVersion version
      }

data Options = Options
  { databaseUri :: String
  , marloweSyncPort :: PortNumber
  , marloweHeaderSyncPort :: PortNumber
  , queryPort :: PortNumber
  , host :: HostName
  , httpPort :: PortNumber
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
