{-# LANGUAGE Arrows #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}

module Language.Marlowe.Runtime.Proxy
  where

import Control.Arrow (returnA)
import Control.Concurrent.Component
import Control.Concurrent.Component.Probes
import Control.Monad.Event.Class (MonadEvent)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Language.Marlowe.Protocol.HeaderSync.Types (MarloweHeaderSync)
import Language.Marlowe.Protocol.Query.Types (MarloweQuery)
import Language.Marlowe.Protocol.Server (MarloweRuntimeServer(..))
import Language.Marlowe.Protocol.Sync.Types (MarloweSync)
import Language.Marlowe.Runtime.Transaction.Api (MarloweTxCommand)
import Network.Protocol.Connection
  ( SomeConnectionSource
  , SomeConnectionSourceTraced
  , SomeServerConnector
  , SomeServerConnectorTraced
  , acceptSomeConnector
  , acceptSomeConnectorTraced
  )
import Network.Protocol.Driver (runSomeConnector, runSomeConnectorTraced)
import Network.Protocol.Handshake.Types (Handshake)
import Network.Protocol.Job.Types (Job)
import Network.Protocol.Peer.Trace (DriverTraced, HasSpanContext)
import UnliftIO (MonadUnliftIO(..))

data ProxyDependencies r s m = forall dState. ProxyDependencies
  { getMarloweSyncDriver :: m (DriverTraced (Handshake MarloweSync) dState r m)
  , getMarloweHeaderSyncDriver :: m (DriverTraced (Handshake MarloweHeaderSync) dState r m)
  , getMarloweQueryDriver :: m (DriverTraced (Handshake MarloweQuery) dState r m)
  , getTxJobDriver :: m (DriverTraced (Handshake (Job MarloweTxCommand)) dState r m)
  , connectionSourceTraced :: SomeConnectionSourceTraced (MarloweRuntimeServer r) r s m
  , connectionSource :: SomeConnectionSource (MarloweRuntimeServer r) m
  }

proxy :: (MonadUnliftIO m, MonadEvent r s m, HasSpanContext r) => Component m (ProxyDependencies r s (ResourceT m)) Probes
proxy = proc deps -> do
  (serverComponent (component_ workerTraced) \ProxyDependencies{..} -> do
    connector <- runResourceT $ acceptSomeConnectorTraced connectionSourceTraced
    pure WorkerDependenciesTraced{..}) -< deps
  (serverComponent (component_ worker) \ProxyDependencies{..} -> do
    connector <- runResourceT $ acceptSomeConnector connectionSource
    pure WorkerDependencies{..}) -< deps
  returnA -< Probes
    { startup = pure True
    , liveness = pure True
    , readiness = pure True
    }

data WorkerDependenciesTraced r s m = forall dState. WorkerDependenciesTraced
  { getMarloweSyncDriver :: m (DriverTraced (Handshake MarloweSync) dState r m)
  , getMarloweHeaderSyncDriver :: m (DriverTraced (Handshake MarloweHeaderSync) dState r m)
  , getMarloweQueryDriver :: m (DriverTraced (Handshake MarloweQuery) dState r m)
  , getTxJobDriver :: m (DriverTraced (Handshake (Job MarloweTxCommand)) dState r m)
  , connector :: SomeServerConnectorTraced (MarloweRuntimeServer r) r s m
  }

data WorkerDependencies r m = forall dState. WorkerDependencies
  { getMarloweSyncDriver :: m (DriverTraced (Handshake MarloweSync) dState r m)
  , getMarloweHeaderSyncDriver :: m (DriverTraced (Handshake MarloweHeaderSync) dState r m)
  , getMarloweQueryDriver :: m (DriverTraced (Handshake MarloweQuery) dState r m)
  , getTxJobDriver :: m (DriverTraced (Handshake (Job MarloweTxCommand)) dState r m)
  , connector :: SomeServerConnector (MarloweRuntimeServer r) m
  }

workerTraced :: (MonadUnliftIO m, MonadEvent r s m, HasSpanContext r) => WorkerDependenciesTraced r s (ResourceT m) -> m ()
workerTraced WorkerDependenciesTraced{..} = runResourceT $ runSomeConnectorTraced connector MarloweRuntimeServer{result = (), ..}

worker :: MonadUnliftIO m => WorkerDependencies r (ResourceT m) -> m ()
worker WorkerDependencies{..} = runResourceT $ runSomeConnector connector MarloweRuntimeServer{result = (), ..}
