{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Control.Monad.Event.Class
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.With
import Data.Either (fromRight)
import Data.GeneralAllocate
import qualified Data.Set.NonEmpty as NESet
import Data.String (fromString)
import qualified Data.Text as T
import Data.Version (showVersion)
import Data.Void (Void)
import qualified Database.PostgreSQL.LibPQ as PQ
import Hasql.Connection (withLibPQConnection)
import qualified Hasql.Pool as Pool
import Language.Marlowe.Runtime.ChainSync.Api (ChainSyncQuery(..))
import Language.Marlowe.Runtime.Core.Api (MarloweVersion(..))
import qualified Language.Marlowe.Runtime.Core.ScriptRegistry as ScriptRegistry
import Language.Marlowe.Runtime.Indexer (MarloweIndexerDependencies(..), marloweIndexer)
import qualified Language.Marlowe.Runtime.Indexer.Database.PostgreSQL as PostgreSQL
import Logging (RootSelector(..), renderRootSelectorOTel)
import Network.Protocol.ChainSeek.Client (chainSeekClientPeer)
import Network.Protocol.Connection (ClientConnectorTraced, SomeConnectorTraced(..))
import Network.Protocol.Driver.Trace (TcpClientSelector, runConnectorTraced, tcpClientTraced)
import Network.Protocol.Handshake.Client (handshakeClientConnectorTraced)
import Network.Protocol.Handshake.Types (Handshake)
import Network.Protocol.Query.Client (QueryClient, liftQuery, queryClientPeer)
import Network.Protocol.Query.Types (Query)
import Network.Socket (AddrInfo(..), HostName, PortNumber, SocketType(..), defaultHints)
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
import UnliftIO (MonadUnliftIO, throwIO)

main :: IO ()
main = run =<< getOptions

clientHints :: AddrInfo
clientHints = defaultHints { addrSocketType = Stream }

run :: Options -> IO ()
run Options{..} = bracket (Pool.acquire 100 (Just 5000000) (fromString databaseUri)) Pool.release \pool -> do
  (dbName, dbUser, dbHost, dbPort) <- either throwIO pure =<< Pool.use pool do
    connection <- ask
    liftIO $ withLibPQConnection connection \conn -> (,,,)
      <$> PQ.db conn
      <*> PQ.user conn
      <*> PQ.host conn
      <*> PQ.port conn
  scripts <- case ScriptRegistry.getScripts MarloweV1 of
    NESet.IsEmpty -> fail "No known marlowe scripts"
    NESet.IsNonEmpty scripts -> pure scripts
  withTracer \tracer ->
    runAppM (tracerEventBackend tracer $ renderRootSelectorOTel dbName dbUser dbHost dbPort) do
      securityParameter <- queryChainSync GetSecurityParameter
      flip runComponent_ () proc _ -> do
        probes <- marloweIndexer -< MarloweIndexerDependencies
          { chainSyncConnector = SomeConnectorTraced (injectSelector ChainSeekClient)
              $ handshakeClientConnectorTraced
              $ tcpClientTraced
                  (injectSelector ChainSeekClient)
                  chainSeekHost
                  chainSeekPort
                  chainSeekClientPeer
          , chainSyncQueryConnector = SomeConnectorTraced
              (injectSelector ChainQueryClient)
              chainSyncQueryConnector
          , databaseQueries = PostgreSQL.databaseQueries pool securityParameter
          , pollingInterval = 1
          , marloweScriptHashes = NESet.map ScriptRegistry.marloweScript scripts
          , payoutScriptHashes = NESet.map ScriptRegistry.payoutScript scripts
          }
        probeServer -< ProbeServerDependencies { port = fromIntegral httpPort, .. }
  where
    withTracer f = bracket
      initializeGlobalTracerProvider
      shutdownTracerProvider
      \provider -> f $ makeTracer provider instrumentationLibrary tracerOptions

    instrumentationLibrary = InstrumentationLibrary
      { libraryName = "marlowe-indexer"
      , libraryVersion = T.pack $ showVersion version
      }

    chainSyncQueryConnector :: ClientConnectorTraced
      (Handshake (Query ChainSyncQuery))
      (QueryClient ChainSyncQuery)
      Span
      TcpClientSelector
      (AppM Span)
    chainSyncQueryConnector = handshakeClientConnectorTraced
      $ tcpClientTraced (injectSelector ChainQueryClient) chainSeekHost chainSeekQueryPort queryClientPeer

    queryChainSync :: ChainSyncQuery Void e a -> AppM Span a
    queryChainSync = fmap (fromRight $ error "failed to query chain sync server")
      . runConnectorTraced (injectSelector ChainQueryClient) chainSyncQueryConnector
      . liftQuery

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
  { chainSeekPort :: PortNumber
  , chainSeekQueryPort :: PortNumber
  , chainSeekHost :: HostName
  , databaseUri :: String
  , httpPort :: PortNumber
  }

getOptions :: IO Options
getOptions = execParser $ info (helper <*> parser) infoMod
  where
    parser = Options
      <$> chainSeekPortParser
      <*> chainSeekQueryPortParser
      <*> chainSeekHostParser
      <*> databaseUriParser
      <*> httpPortParser

    chainSeekPortParser = option auto $ mconcat
      [ long "chain-sync-port"
      , value 3715
      , metavar "PORT_NUMBER"
      , help "The port number of the chain sync server."
      , showDefault
      ]

    chainSeekQueryPortParser = option auto $ mconcat
      [ long "chain-sync-query-port"
      , value 3716
      , metavar "PORT_NUMBER"
      , help "The port number of the chain sync query server."
      , showDefault
      ]

    chainSeekHostParser = strOption $ mconcat
      [ long "chain-sync-host"
      , value "127.0.0.1"
      , metavar "HOST_NAME"
      , help "The host name of the chain sync server."
      , showDefault
      ]

    databaseUriParser = strOption $ mconcat
      [ long "database-uri"
      , short 'd'
      , metavar "DATABASE_URI"
      , help "URI of the database where the contract information is saved."
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
      , progDesc "Contract indexing service for Marlowe Runtime"
      , header "marlowe-indexer : a contract indexing service for the Marlowe Runtime."
      ]
