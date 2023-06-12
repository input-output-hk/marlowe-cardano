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

import Cardano.Api (CardanoMode, ConsensusModeParams(..), EpochSlots(..), LocalNodeConnectInfo(..))
import qualified Cardano.Api as Cardano
import Cardano.Api.Byron (toByronRequiresNetworkMagic)
import qualified Cardano.Chain.Genesis as Byron
import Cardano.Crypto (abstractHashToBytes, decodeAbstractHash)
import Colog (LogAction, Message, cmap, fmtMessage, logTextStdout)
import Control.Concurrent.Component
import Control.Concurrent.Component.Probes (ProbeServerDependencies(..), probeServer)
import Control.Exception (bracket)
import Control.Monad.Event.Class
import Control.Monad.Reader (MonadReader(..), asks, withReaderT)
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT, withExceptT)
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.With (MonadWith, WithException, stateThreadingGeneralWith)
import Data.Aeson (eitherDecodeFileStrict)
import Data.Bifunctor (first)
import Data.GeneralAllocate (GeneralAllocate(..), GeneralAllocated(GeneralAllocated))
import Data.String (IsString(fromString))
import Data.Text (unpack)
import qualified Data.Text as T
import Data.Time (secondsToNominalDiffTime)
import Data.Version (showVersion)
import qualified Database.PostgreSQL.LibPQ as PQ
import Hasql.Connection (withLibPQConnection)
import qualified Hasql.Pool as Pool
import Language.Marlowe.Runtime.ChainIndexer (ChainIndexerDependencies(..), chainIndexer)
import qualified Language.Marlowe.Runtime.ChainIndexer.Database.PostgreSQL as PostgreSQL
import Language.Marlowe.Runtime.ChainIndexer.Genesis (computeGenesisBlock)
import Logging (RootSelector(..), renderRootSelectorOTel)
import Observe.Event (EventBackend)
import Observe.Event.Backend (hoistEventBackend)
import Observe.Event.Render.OpenTelemetry (tracerEventBackend)
import OpenTelemetry.Trace
import Options (Options(..), getOptions)
import Paths_marlowe_chain_sync (version)
import UnliftIO (MonadIO, MonadUnliftIO, liftIO, throwIO)

main :: IO ()
main = run =<< getOptions (showVersion version)

run :: Options -> IO ()
run Options{..} = do
  genesisConfigResult <- runExceptT do
    hash <- ExceptT $ pure $ decodeAbstractHash genesisConfigHash
    (hash,) <$> withExceptT
      (mappend "failed to read byron genesis file: " . T.pack . show)
      (Byron.mkConfigFromFile (toByronRequiresNetworkMagic networkId) genesisConfigFile hash)
  (hash, genesisConfig) <- either (fail . unpack) pure genesisConfigResult
  shelleyGenesis <- either error id <$> eitherDecodeFileStrict shelleyGenesisFile
  let
    genesisBlock = computeGenesisBlock (abstractHashToBytes hash) genesisConfig shelleyGenesis
    appComponent = proc pool -> do
      probes <- chainIndexer -< ChainIndexerDependencies
        { connectToLocalNode = \client -> do
            connectRef <- emitImmediateEventFields (ConnectToNode @Span) [localNodeConnectInfo]
            liftIO $ Cardano.connectToLocalNode localNodeConnectInfo $ client connectRef
        , databaseQueries = PostgreSQL.databaseQueries pool genesisBlock
        , persistRateLimit
        , genesisBlock
        , maxCost
        , costModel
        }
      probeServer -< ProbeServerDependencies
        { probes
        , port = httpPort
        }

  bracket (Pool.acquire 100 (Just 5000000) (fromString databaseUri)) Pool.release \pool -> do
    (dbName, dbUser, dbHost, dbPort) <- either throwIO pure =<< Pool.use pool do
      connection <- ask
      liftIO $ withLibPQConnection connection \conn -> (,,,)
        <$> PQ.db conn
        <*> PQ.user conn
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
      { libraryName = "marlowe-chain-indexer"
      , libraryVersion = T.pack $ showVersion version
      }

    localNodeConnectInfo :: LocalNodeConnectInfo CardanoMode
    localNodeConnectInfo = LocalNodeConnectInfo
      -- FIXME read from config - what is the appropriate value?
      { localConsensusModeParams = CardanoModeParams $ EpochSlots 21600
      , localNodeNetworkId = networkId
      , localNodeSocketPath = nodeSocket
      }

    persistRateLimit = secondsToNominalDiffTime 1

runAppM :: EventBackend IO r (RootSelector r) -> AppM r a -> IO a
runAppM eventBackend = flip runReaderT (hoistEventBackend liftIO eventBackend, cmap fmtMessage logTextStdout) . unAppM

newtype AppM r a = AppM
  { unAppM :: ReaderT (EventBackend (AppM r) r (RootSelector r), LogAction (AppM r) Message) IO a
  } deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO)

instance MonadReader (LogAction (AppM r) Message) (AppM r) where
  ask = AppM $ asks snd
  local f (AppM m) = AppM $ withReaderT (fmap f) m

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

instance MonadEvent r (RootSelector r) (AppM r) where
  askBackend = askBackendReaderT AppM fst
  localBackend = localBackendReaderT AppM unAppM first
