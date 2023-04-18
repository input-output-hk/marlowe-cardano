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
import Control.Concurrent.Component.UnliftIO (convertComponent)
import Control.Exception (bracket)
import Control.Monad.Base (MonadBase)
import Control.Monad.Event.Class
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.With (MonadWith(..))
import Data.GeneralAllocate
import Data.String (IsString(fromString))
import qualified Data.Text.Lazy.IO as TL
import Data.UUID.V4 (nextRandom)
import qualified Hasql.Pool as Pool
import Language.Marlowe.Runtime.ChainSync (ChainSyncDependencies(..), chainSync)
import qualified Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL as PostgreSQL
import Language.Marlowe.Runtime.ChainSync.NodeClient (NodeClient(..), NodeClientDependencies(..), nodeClient)
import Logging (RootSelector(..), getRootSelectorConfig)
import Network.Protocol.ChainSeek.Server (chainSeekServerPeer)
import Network.Protocol.Connection (SomeConnectionSource(..), logConnectionSource)
import Network.Protocol.Driver (TcpServerDependencies(..), tcpServer)
import Network.Protocol.Handshake.Server (handshakeConnectionSource)
import Network.Protocol.Job.Server (jobServerPeer)
import Network.Protocol.Query.Server (queryServerPeer)
import Observe.Event (EventBackend)
import Observe.Event.Component (LoggerDependencies(..), withLogger)
import Observe.Event.Explicit (hoistEventBackend, injectSelector)
import Options (Options(..), getOptions)
import System.IO (stderr)
import UnliftIO (MonadUnliftIO)

main :: IO ()
main = run =<< getOptions "0.0.0.0"

run :: Options -> IO ()
run Options{..} = bracket (Pool.acquire 100 (Just 5000000) (fromString databaseUri)) Pool.release $
  runComponent_ $ withLogger loggerDependencies runAppM appComponent
  where
    loggerDependencies = LoggerDependencies
      { configFilePath = logConfigFile
      , getSelectorConfig = getRootSelectorConfig
      , newRef = nextRandom
      , writeText = TL.hPutStr stderr
      , injectConfigWatcherSelector = injectSelector ConfigWatcher
      }

    appComponent :: Component (AppM r) Pool.Pool ()
    appComponent = proc pool -> do
      syncSource <- tcpServer -< TcpServerDependencies
        { host
        , port
        , toPeer = chainSeekServerPeer
        }

      querySource <- tcpServer -< TcpServerDependencies
        { host
        , port = queryPort
        , toPeer = queryServerPeer
        }

      jobSource <- tcpServer -< TcpServerDependencies
        { host
        , port = commandPort
        , toPeer = jobServerPeer
        }

      NodeClient{..} <- convertComponent nodeClient -< NodeClientDependencies
        { connectToLocalNode = Cardano.connectToLocalNode localNodeConnectInfo
        }

      probes <- convertComponent chainSync -< ChainSyncDependencies
        { databaseQueries = PostgreSQL.databaseQueries pool networkId
        , syncSource = SomeConnectionSource
            $ logConnectionSource (injectSelector ChainSeekServer)
            $ handshakeConnectionSource syncSource
        , querySource = SomeConnectionSource
            $ logConnectionSource (injectSelector QueryServer)
            $ handshakeConnectionSource querySource
        , jobSource = SomeConnectionSource
            $ logConnectionSource (injectSelector JobServer)
            $ handshakeConnectionSource jobSource
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
  } deriving newtype (Functor, Applicative, Monad, MonadBase IO, MonadBaseControl IO, MonadIO, MonadUnliftIO, MonadFail)

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
