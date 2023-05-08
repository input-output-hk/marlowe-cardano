{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main
  where

import Control.Concurrent.Component
import Control.Concurrent.Component.Probes (ProbeServerDependencies(..), probeServer)
import Control.Exception (bracket)
import Control.Monad ((<=<))
import Control.Monad.Event.Class
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.With
import Data.GeneralAllocate
import Data.String (fromString)
import qualified Data.Text as T
import Data.Version (showVersion)
import qualified Database.PostgreSQL.LibPQ as PQ
import Hasql.Connection (withLibPQConnection)
import qualified Hasql.Pool as Pool
import Language.Marlowe.Protocol.HeaderSync.Server (marloweHeaderSyncServerPeer)
import Language.Marlowe.Protocol.Sync.Server (marloweSyncServerPeer)
import Language.Marlowe.Runtime.Sync (SyncDependencies(..), sync)
import Language.Marlowe.Runtime.Sync.Database (hoistDatabaseQueries, logDatabaseQueries)
import qualified Language.Marlowe.Runtime.Sync.Database.PostgreSQL as Postgres
import Logging (RootSelector(..), renderRootSelectorOTel)
import Network.Protocol.Connection (SomeConnectionSourceTraced(..))
import Network.Protocol.Driver (TcpServerDependencies(..), tcpServerTraced)
import Network.Protocol.Handshake.Server (handshakeConnectionSourceTraced)
import Network.Socket (HostName, PortNumber)
import Observe.Event.Backend (EventBackend, hoistEventBackend, injectSelector)
import Observe.Event.Render.OpenTelemetry (tracerEventBackend)
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
import UnliftIO (MonadIO, MonadUnliftIO, liftIO, throwIO)

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
  withTracer \tracer ->
    runAppM (tracerEventBackend tracer $ renderRootSelectorOTel dbName dbUser dbHost dbPort) do
      flip runComponent_ () proc _ -> do
        marloweSyncSource <- tcpServerTraced (injectSelector MarloweSyncServer) -< TcpServerDependencies
          { host
          , port = marloweSyncPort
          , toPeer = marloweSyncServerPeer
          }

        headerSyncSource <- tcpServerTraced (injectSelector MarloweHeaderSyncServer) -< TcpServerDependencies
          { host
          , port = marloweHeaderSyncPort
          , toPeer = marloweHeaderSyncServerPeer
          }

        querySource <- tcpServerTraced (injectSelector MarloweQueryServer) -< TcpServerDependencies
          { host
          , port = queryPort
          , toPeer = id
          }

        probes <- sync -< SyncDependencies
          { databaseQueries = logDatabaseQueries $ hoistDatabaseQueries
                (either throwIO pure <=< liftIO . Pool.use pool)
                Postgres.databaseQueries
          , syncSource = SomeConnectionSourceTraced (injectSelector MarloweSyncServer)
              $ handshakeConnectionSourceTraced marloweSyncSource
          , headerSyncSource = SomeConnectionSourceTraced (injectSelector MarloweHeaderSyncServer)
              $ handshakeConnectionSourceTraced headerSyncSource
          , querySource = SomeConnectionSourceTraced (injectSelector MarloweQueryServer)
              $ handshakeConnectionSourceTraced querySource
          }

        probeServer -< ProbeServerDependencies { port = fromIntegral httpPort, .. }
  where
    withTracer f = bracket
      initializeGlobalTracerProvider
      shutdownTracerProvider
      \provider -> f $ makeTracer provider instrumentationLibrary tracerOptions

    instrumentationLibrary = InstrumentationLibrary
      { libraryName = "marlowe-sync"
      , libraryVersion = T.pack $ showVersion version
      }

runAppM :: EventBackend IO r RootSelector -> AppM r a -> IO a
runAppM eventBackend = flip runReaderT (hoistEventBackend liftIO eventBackend) . unAppM

newtype AppM r a = AppM
  { unAppM :: ReaderT (EventBackend (AppM r) r RootSelector) IO a
  } deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadFail)

instance MonadWith (AppM r) where
  type WithException (AppM r) = WithException IO
  stateThreadingGeneralWith
    :: forall a b releaseReturn
     . GeneralAllocate (AppM r) (WithException IO) releaseReturn b a
    -> (a -> AppM r b)
    -> AppM r (b, releaseReturn)
  stateThreadingGeneralWith (GeneralAllocate allocA) go = AppM . ReaderT $ \r -> do
    let
      allocA' :: (forall x. IO x -> IO x) -> IO (GeneralAllocated IO (WithException IO) releaseReturn b a)
      allocA' restore = do
        let
          restore' :: forall x. AppM r x -> AppM r x
          restore' mx = AppM . ReaderT $ restore . (runReaderT . unAppM) mx
        GeneralAllocated a releaseA <- (runReaderT . unAppM) (allocA restore') r
        let
          releaseA' relTy = (runReaderT . unAppM) (releaseA relTy) r
        pure $ GeneralAllocated a releaseA'
    stateThreadingGeneralWith (GeneralAllocate allocA') (flip (runReaderT . unAppM) r . go)

instance MonadEvent r RootSelector (AppM r) where
  askBackend = askBackendReaderT AppM id
  localBackend = localBackendReaderT AppM unAppM id

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
