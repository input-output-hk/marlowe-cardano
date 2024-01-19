{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Concurrent.Component
import Control.Concurrent.Component.Probes (ProbeServerDependencies (..), probeServer)
import Control.Concurrent.Component.Run (AppM, runAppMTraced)
import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader (..))
import qualified Data.Set.NonEmpty as NESet
import Data.String (fromString)
import qualified Data.Text as T
import Data.Version (showVersion)
import qualified Database.PostgreSQL.LibPQ as PQ
import Hasql.Connection (withLibPQConnection)
import qualified Hasql.Pool as Pool
import Language.Marlowe.Runtime.ChainSync.Api (ChainSyncQuery (..), WithGenesis (..))
import Language.Marlowe.Runtime.Core.Api (MarloweVersion (..))
import qualified Language.Marlowe.Runtime.Core.ScriptRegistry as ScriptRegistry
import Language.Marlowe.Runtime.Indexer (MarloweIndexerDependencies (..), marloweIndexer)
import qualified Language.Marlowe.Runtime.Indexer.Database.PostgreSQL as PostgreSQL
import qualified Language.Marlowe.Runtime.Indexer.Party as Party
import Logging (RootSelector (..), renderRootSelectorOTel)
import Network.Protocol.ChainSeek.Client (chainSeekClientPeer)
import qualified Network.Protocol.ChainSeek.Types as ChainSeek
import Network.Protocol.Connection (
  Connection (..),
  ConnectionTraced (..),
  Connector (..),
  ConnectorTraced (..),
  runConnector,
 )
import Network.Protocol.Peer.Monad.TCP (tcpClientPeerTTraced)
import Network.Protocol.Query.Client (QueryClient, queryClientPeerT, request)
import qualified Network.Protocol.Query.Types as Query
import Network.Socket (AddrInfo (..), HostName, PortNumber, SocketType (..), defaultHints)
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
import Options.Applicative.Help.Pretty
import Paths_marlowe_runtime (version)
import UnliftIO (MonadUnliftIO (..), throwIO)

main :: IO ()
main = run =<< getOptions

clientHints :: AddrInfo
clientHints = defaultHints{addrSocketType = Stream}

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
    scripts <- case ScriptRegistry.getScripts MarloweV1 of
      NESet.IsEmpty -> fail "No known marlowe scripts"
      NESet.IsNonEmpty scripts -> pure scripts
    securityParameter <- queryChainSync $ request GetSecurityParameter
    flip runComponent_ () proc _ -> do
      probes <-
        marloweIndexer
          -<
            MarloweIndexerDependencies
              { chainSyncConnector =
                  let connectorTraced =
                        tcpClientPeerTTraced
                          "chain-seek"
                          ChainSeek.TokDone
                          (injectSelector ChainSeekClient)
                          chainSeekHost
                          chainSeekPort
                   in Connector do
                        ConnectionTraced{..} <- openConnectionTraced connectorTraced
                        pure $ Connection \client -> runConnectionTraced \inj -> chainSeekClientPeer Genesis inj client
              , chainSyncQueryConnector
              , databaseQueries = PostgreSQL.databaseQueries pool securityParameter
              , pollingInterval = 1
              , marloweScriptHashes = NESet.map ScriptRegistry.marloweScript scripts
              , payoutScriptHashes = NESet.map ScriptRegistry.payoutScript scripts
              , indexParties = withRunInIO \runInIO ->
                  either throwIO pure =<< Pool.use pool do
                    connection <- ask
                    liftIO $ runInIO $ Party.indexParties connection
              }
      probeServer -< ProbeServerDependencies{port = fromIntegral httpPort, ..}
  where
    instrumentationLibrary =
      InstrumentationLibrary
        { libraryName = "marlowe-indexer"
        , libraryVersion = T.pack $ showVersion version
        }

    chainSyncQueryConnector :: Connector (QueryClient ChainSyncQuery) (AppM Span RootSelector)
    chainSyncQueryConnector =
      let connectorTraced =
            tcpClientPeerTTraced
              "chain-query"
              Query.TokDone
              (injectSelector ChainQueryClient)
              chainSeekHost
              chainSeekQueryPort
       in Connector do
            ConnectionTraced{..} <- openConnectionTraced connectorTraced
            pure $ Connection \client -> runConnectionTraced \inj -> queryClientPeerT inj client

    queryChainSync :: QueryClient ChainSyncQuery (AppM Span RootSelector) a -> AppM Span RootSelector a
    queryChainSync = runConnector chainSyncQueryConnector

data Options = Options
  { chainSeekPort :: PortNumber
  , chainSeekQueryPort :: PortNumber
  , chainSeekHost :: HostName
  , databaseUri :: String
  , httpPort :: PortNumber
  }

getOptions :: IO Options
getOptions = execParser $ info (helper <*> versionOption <*> parser) infoMod
  where
    parser =
      Options
        <$> chainSeekPortParser
        <*> chainSeekQueryPortParser
        <*> chainSeekHostParser
        <*> databaseUriParser
        <*> httpPortParser

    versionOption =
      infoOption
        ("marlowe-indexer " <> showVersion version)
        (long "version" <> help "Show version.")

    chainSeekPortParser =
      option auto $
        mconcat
          [ long "chain-sync-port"
          , value 3715
          , metavar "PORT_NUMBER"
          , help "The port number of the chain sync server."
          , showDefault
          ]

    chainSeekQueryPortParser =
      option auto $
        mconcat
          [ long "chain-sync-query-port"
          , value 3716
          , metavar "PORT_NUMBER"
          , help "The port number of the chain sync query server."
          , showDefault
          ]

    chainSeekHostParser =
      strOption $
        mconcat
          [ long "chain-sync-host"
          , value "127.0.0.1"
          , metavar "HOST_NAME"
          , help "The host name of the chain sync server."
          , showDefault
          ]

    databaseUriParser =
      strOption $
        mconcat
          [ long "database-uri"
          , short 'd'
          , metavar "DATABASE_URI"
          , help "URI of the database where the contract information is saved."
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
        , header "marlowe-indexer: Contract indexing service for the Marlowe Runtime."
        ]

description :: Doc
description =
  concatWith
    (\a b -> a <> line <> line <> b)
    [ vcat
        [ "The contract indexer for the Marlowe Runtime. It connects to a marlowe-chain-sync"
        , "instance using both the chain seek and chain query protocol. It scans the chain for"
        , "Marlowe contract transactions and saves them in a postgresql database. This database"
        , "can be queried by downstream components, such as marlowe-sync."
        ]
    , vcat
        [ "There should only be one instance of marlowe-indexer writing data to a given marlowe"
        , "database. There is no need to run multiple indexers. If you would like to scale runtime"
        , "services, it is recommended to deploy a postgres replica cluster, run one indexer to"
        , "populate it, and as many marlowe-sync instances as required to read from it."
        ]
    , vcat
        [ "Before running the indexer, the database must be created and migrated using"
        , "sqitch. The migration plan and SQL scripts are included in the source code"
        , "folder for marlowe-indexer."
        ]
    ]

concatWith :: (a -> a -> a) -> [a] -> a
concatWith _ [] = error "concatWith: empty list"
concatWith _ [a] = a
concatWith f (a : b : as) = concatWith f $ f a b : as
