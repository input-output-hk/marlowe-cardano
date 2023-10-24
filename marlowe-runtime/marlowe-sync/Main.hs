{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Concurrent.Component
import Control.Concurrent.Component.Probes (ProbeServerDependencies (..), probeServer)
import Control.Concurrent.Component.Run (runAppMTraced)
import Control.Exception (bracket)
import Control.Monad ((<=<))
import Control.Monad.Reader (MonadReader (..))
import Data.String (fromString)
import qualified Data.Text as T
import Data.Version (showVersion)
import qualified Database.PostgreSQL.LibPQ as PQ
import Hasql.Connection (withLibPQConnection)
import qualified Hasql.Pool as Pool
import Language.Marlowe.Protocol.BulkSync.Server (marloweBulkSyncServerPeer)
import Language.Marlowe.Protocol.HeaderSync.Server (marloweHeaderSyncServerPeer)
import Language.Marlowe.Protocol.Sync.Server (marloweSyncServerPeer)
import Language.Marlowe.Runtime.Sync (MarloweSync (..), SyncDependencies (..), sync)
import Language.Marlowe.Runtime.Sync.Database (hoistDatabaseQueries, logDatabaseQueries)
import qualified Language.Marlowe.Runtime.Sync.Database.PostgreSQL as Postgres
import Logging (RootSelector (..), renderRootSelectorOTel)
import Network.Protocol.Connection (Connection (..), ConnectionTraced (..), Connector (..), ConnectorTraced (..))
import Network.Protocol.Driver (TcpServerDependencies (..))
import Network.Protocol.Driver.Trace (tcpServerTraced)
import Network.Protocol.Peer.Monad.TCP (tcpClientPeerTTraced)
import Network.Protocol.Query.Client (queryClientPeerT)
import Network.Protocol.Query.Server (queryServerPeer)
import qualified Network.Protocol.Query.Types as Query
import Network.Socket (HostName, PortNumber)
import Observe.Event.Backend (injectSelector)
import OpenTelemetry.Trace
import Options.Applicative (
  auto,
  execParser,
  fullDesc,
  header,
  help,
  helper,
  info,
  infoOption,
  long,
  metavar,
  option,
  progDescDoc,
  short,
  showDefault,
  strOption,
  value,
 )
import Paths_marlowe_runtime (version)
import Prettyprinter
import UnliftIO (liftIO, throwIO)

main :: IO ()
main = run =<< getOptions

run :: Options -> IO ()
run Options{..} = bracket (Pool.acquire 100 (Just 5000000) (fromString databaseUri)) Pool.release \pool -> do
  (dbName, dbUser, dbHost, dbPort) <-
    either throwIO pure =<< Pool.use pool do
      connection <- ask
      liftIO $ withLibPQConnection connection \conn ->
        (,,,)
          <$> PQ.db conn
          <*> PQ.user conn
          <*> PQ.host conn
          <*> PQ.port conn
  runAppMTraced instrumentationLibrary (renderRootSelectorOTel dbName dbUser dbHost dbPort) do
    flip runComponent_ () proc _ -> do
      MarloweSync{..} <-
        sync
          -<
            SyncDependencies
              { databaseQueries =
                  logDatabaseQueries $
                    hoistDatabaseQueries
                      (either throwIO pure <=< liftIO . Pool.use pool)
                      Postgres.databaseQueries
              , runtimeVersion = version
              , chainSyncQueryConnector =
                  let connectorTraced =
                        tcpClientPeerTTraced
                          "chain-query"
                          Query.TokDone
                          (injectSelector ChainSyncQueryClient)
                          chainSyncHost
                          chainQueryPort
                   in Connector do
                        ConnectionTraced{..} <- openConnectionTraced connectorTraced
                        pure $ Connection \client -> runConnectionTraced \inj -> queryClientPeerT inj client
              }

      tcpServerTraced "marlowe-sync" (injectSelector MarloweSyncServer)
        -<
          TcpServerDependencies
            { host
            , port = marloweSyncPort
            , toPeer = marloweSyncServerPeer
            , serverSource = syncServerSource
            }

      tcpServerTraced "marlowe-header-sync" (injectSelector MarloweHeaderSyncServer)
        -<
          TcpServerDependencies
            { host
            , port = marloweHeaderSyncPort
            , toPeer = marloweHeaderSyncServerPeer
            , serverSource = headerSyncServerSource
            }

      tcpServerTraced "marlowe-bulk-sync" (injectSelector MarloweBulkSyncServer)
        -<
          TcpServerDependencies
            { host
            , port = marloweBulkSyncPort
            , toPeer = marloweBulkSyncServerPeer
            , serverSource = bulkSyncServerSource
            }

      tcpServerTraced "sync-query" (injectSelector MarloweQueryServer)
        -<
          TcpServerDependencies
            { host
            , port = queryPort
            , toPeer = queryServerPeer
            , serverSource = queryServerSource
            }

      probeServer -< ProbeServerDependencies{port = fromIntegral httpPort, ..}
  where
    instrumentationLibrary =
      InstrumentationLibrary
        { libraryName = "marlowe-sync"
        , libraryVersion = T.pack $ showVersion version
        }

data Options = Options
  { databaseUri :: String
  , marloweSyncPort :: PortNumber
  , marloweHeaderSyncPort :: PortNumber
  , marloweBulkSyncPort :: PortNumber
  , queryPort :: PortNumber
  , host :: HostName
  , chainSyncHost :: HostName
  , chainQueryPort :: PortNumber
  , httpPort :: PortNumber
  }

getOptions :: IO Options
getOptions = execParser $ info (helper <*> versionOption <*> parser) infoMod
  where
    parser =
      Options
        <$> databaseUriParser
        <*> marloweSyncPortParser
        <*> marloweHeaderSyncPortParser
        <*> marloweBulkSyncPortParser
        <*> queryPort
        <*> hostParser
        <*> chainSyncHostParser
        <*> chainQueryPortParser
        <*> httpPortParser

    versionOption =
      infoOption
        ("marlowe-sync " <> showVersion version)
        (long "version" <> help "Show version.")

    databaseUriParser =
      strOption $
        mconcat
          [ long "database-uri"
          , short 'd'
          , metavar "DATABASE_URI"
          , help "URI of the database where the contract information is saved."
          ]

    marloweSyncPortParser =
      option auto $
        mconcat
          [ long "sync-port"
          , value 3724
          , metavar "PORT_NUMBER"
          , help "The port number to run the sync protocol on."
          , showDefault
          ]

    marloweHeaderSyncPortParser =
      option auto $
        mconcat
          [ long "header-sync-port"
          , value 3725
          , metavar "PORT_NUMBER"
          , help "The port number to run the header sync protocol on."
          , showDefault
          ]

    marloweBulkSyncPortParser =
      option auto $
        mconcat
          [ long "bulk-sync-port"
          , value 3730
          , metavar "PORT_NUMBER"
          , help "The port number to run the bulk sync protocol on."
          , showDefault
          ]

    queryPort =
      option auto $
        mconcat
          [ long "query-port"
          , value 3726
          , metavar "PORT_NUMBER"
          , help "The port number to run the query protocol on."
          , showDefault
          ]

    hostParser =
      strOption $
        mconcat
          [ long "host"
          , short 'h'
          , value "127.0.0.1"
          , metavar "HOST_NAME"
          , help "The host name to run the server on."
          , showDefault
          ]

    chainSyncHostParser =
      strOption $
        mconcat
          [ long "chain-sync-host"
          , value "127.0.0.1"
          , metavar "HOST_NAME"
          , help "The host name of the chain sync server."
          , showDefault
          ]

    chainQueryPortParser =
      option auto $
        mconcat
          [ long "chain-sync-query-port"
          , value 3716
          , metavar "PORT_NUMBER"
          , help "The port number of the chain sync query server."
          , showDefault
          ]

    httpPortParser =
      option auto $
        mconcat
          [ long "http-port"
          , metavar "PORT_NUMBER"
          , help "Port number to serve the http healthcheck API on"
          , value 8080
          , showDefault
          ]

    infoMod =
      mconcat
        [ fullDesc
        , progDescDoc $ Just description
        , header "marlowe-sync: Contract synchronization and query service for the Marlowe Runtime."
        ]

description :: Doc ann
description =
  concatWith
    (\a b -> a <> line <> line <> b)
    [ vcat
        [ "The contract query engine for the Marlowe Runtime. This component exposes four"
        , "protocols through which downstream components can interact with the blockchain."
        , "These are: marlowe sync, marlowe header sync, marlowe bulk sync, and marlowe query."
        ]
    , vcat
        [ "The marlowe sync protocol is a synchronization protocol which follows the history"
        , "of a specific marlowe contract."
        ]
    , vcat
        [ "The marlowe header sync protocol is a synchronization protocol which scans the chain"
        , "for new Marlowe contracts and presents them as a compact summary called a header."
        ]
    , vcat
        [ "The marlowe bulk sync protocol is a synchronization protocol which combines the"
        , "capabilities of marlowe header sync and marlowe sync. It presents a stream of blocks"
        , "which contain a combination of all three contract transaction types: creation, input"
        , "application, and payout withdrawal."
        ]
    , vcat
        [ "The marlowe query protocol supports multiple queries that allow clients to fetch"
        , "data about marlowe contracts as of the current blockchain tip. This means that it"
        , "cannot guarantee consistent results between different queries, because the chain could"
        , "update in between queries, changing the result of queries. If consistency is needed,"
        , "use one of the sync protocols."
        ]
    , vcat
        [ "marlowe-sync relies on the connected database being migrated and populated by a"
        , "marlowe-indexer instance. While marlowe-sync can operate without marlowe-indexer running,"
        , "The sqitch migrations must first be performed in order to create the expected tables, and"
        , "marlowe-indexer must be running to keep the database up-to-date."
        ]
    , vcat
        [ "marlowe-sync is designed to scale horizontally. That is to say, multiple instances can run"
        , "in parallel to scale with demand. A typical setup for this would involve running multiple"
        , "marlowe-sync instances in front of a load balancer against a scalable postgres replica"
        , "cluster being populated by a single marlowe-indexer."
        ]
    ]
