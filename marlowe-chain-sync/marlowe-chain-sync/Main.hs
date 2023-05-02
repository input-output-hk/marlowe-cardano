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

import Cardano.Api
  ( CardanoEra(..)
  , CardanoMode
  , ConsensusModeParams(..)
  , EpochSlots(..)
  , EraInMode(..)
  , LocalNodeConnectInfo(..)
  , TxInMode(..)
  )
import qualified Cardano.Api as Cardano (connectToLocalNode)
import Control.Concurrent.Component
import Control.Concurrent.Component.Probes (ProbeServerDependencies(..), probeServer)
import Control.Exception (bracket)
import Control.Monad.Event.Class
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.With (MonadWith(..))
import Data.GeneralAllocate
import Data.String (IsString(fromString))
import qualified Data.Text as T
import Data.Version (showVersion)
import Database.PostgreSQL.LibPQ (db, user)
import qualified Database.PostgreSQL.LibPQ as PQ
import Hasql.Connection (withLibPQConnection)
import qualified Hasql.Pool as Pool
import Language.Marlowe.Runtime.ChainSync (ChainSyncDependencies(..), chainSync)
import qualified Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL as PostgreSQL
import Language.Marlowe.Runtime.ChainSync.NodeClient (NodeClient(..), NodeClientDependencies(..), nodeClient)
import Logging (RootSelector(..), renderRootSelectorOTel)
import Network.Protocol.ChainSeek.Server (chainSeekServerPeerTraced)
import Network.Protocol.Connection (SomeConnectionSourceTraced(..))
import Network.Protocol.Driver (TcpServerDependenciesTraced(..), tcpServerTraced)
import Network.Protocol.Handshake.Server (handshakeConnectionSourceTraced)
import Network.Protocol.Job.Server (jobServerPeerTraced)
import Network.Protocol.Query.Server (queryServerPeerTraced)
import Observe.Event (EventBackend)
import Observe.Event.Explicit (hoistEventBackend, injectSelector)
import Observe.Event.Render.OpenTelemetry (tracerEventBackend)
import OpenTelemetry.Trace
import Options (Options(..), getOptions)
import Paths_marlowe_chain_sync (version)
import UnliftIO (MonadUnliftIO, throwIO)

main :: IO ()
main = run =<< getOptions (showVersion version)

run :: Options -> IO ()
run Options{..} = bracket (Pool.acquire 100 (Just 5000000) (fromString databaseUri)) Pool.release \pool -> do
  (dbName, dbUser, dbHost, dbPort) <- either throwIO pure =<< Pool.use pool do
    connection <- ask
    liftIO $ withLibPQConnection connection \conn -> (,,,)
      <$> db conn
      <*> user conn
      <*> PQ.host conn
      <*> PQ.port conn
  withTracer \tracer ->
    runAppM (tracerEventBackend tracer $ renderRootSelectorOTel dbName dbUser dbHost dbPort)
      $ runComponent_ appComponent pool
  where
    withTracer f = bracket
      initializeGlobalTracerProvider
      shutdownTracerProvider
      \provider -> f $ makeTracer provider instrumentationLibrary tracerOptions

    instrumentationLibrary = InstrumentationLibrary
      { libraryName = "marlowe-chain-sync"
      , libraryVersion = T.pack $ showVersion version
      }

    appComponent :: Component (AppM Span) Pool.Pool ()
    appComponent = proc pool -> do
      syncSource <- tcpServerTraced $ injectSelector ChainSeekServer -< TcpServerDependenciesTraced
        { host
        , port
        , toPeer = chainSeekServerPeerTraced
        }

      querySource <- tcpServerTraced $ injectSelector QueryServer -< TcpServerDependenciesTraced
        { host
        , port = queryPort
        , toPeer = queryServerPeerTraced
        }

      jobSource <- tcpServerTraced $ injectSelector JobServer -< TcpServerDependenciesTraced
        { host
        , port = commandPort
        , toPeer = jobServerPeerTraced
        }

      NodeClient{..} <- nodeClient -< NodeClientDependencies
        { connectToLocalNode = Cardano.connectToLocalNode localNodeConnectInfo
        }

      probes <- chainSync -< ChainSyncDependencies
        { databaseQueries = PostgreSQL.databaseQueries pool networkId
        , syncSource = SomeConnectionSourceTraced (injectSelector ChainSeekServer)
            $ handshakeConnectionSourceTraced syncSource
        , querySource = SomeConnectionSourceTraced (injectSelector QueryServer)
            $ handshakeConnectionSourceTraced querySource
        , jobSource = SomeConnectionSourceTraced (injectSelector JobServer)
            $ handshakeConnectionSourceTraced jobSource
        , queryLocalNodeState = queryNode
        , submitTxToNodeLocal = \era tx -> submitTxToNode $ TxInMode tx case era of
            ByronEra -> ByronEraInCardanoMode
            ShelleyEra -> ShelleyEraInCardanoMode
            AllegraEra -> AllegraEraInCardanoMode
            MaryEra -> MaryEraInCardanoMode
            AlonzoEra -> AlonzoEraInCardanoMode
            BabbageEra -> BabbageEraInCardanoMode
        }

      probeServer -< ProbeServerDependencies { port = fromIntegral httpPort, .. }

    localNodeConnectInfo :: LocalNodeConnectInfo CardanoMode
    localNodeConnectInfo = LocalNodeConnectInfo
      -- The epoch slots ignored after Byron.
      { localConsensusModeParams = CardanoModeParams $ EpochSlots 21600
      , localNodeNetworkId = networkId
      , localNodeSocketPath = nodeSocket
      }

runAppM :: EventBackend IO r RootSelector -> AppM r a -> IO a
runAppM eventBackend = flip runReaderT (hoistEventBackend liftIO eventBackend). unAppM

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
  localBackend = localBackendReaderT AppM unAppM id
  askBackend = askBackendReaderT AppM id
