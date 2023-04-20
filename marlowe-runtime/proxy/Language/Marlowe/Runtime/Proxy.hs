{-# LANGUAGE Arrows #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}

module Language.Marlowe.Runtime.Proxy
  where

import Control.Arrow (returnA)
import Control.Concurrent.Component
import Control.Concurrent.Component.Probes
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Language.Marlowe.Protocol.HeaderSync.Types (MarloweHeaderSync)
import Language.Marlowe.Protocol.Query.Types (MarloweQuery)
import Language.Marlowe.Protocol.Server (MarloweRuntimeServer(..), wrapDriver)
import Language.Marlowe.Protocol.Sync.Types (MarloweSync)
import Language.Marlowe.Runtime.Transaction.Api (MarloweTxCommand)
import Network.Protocol.Connection (SomeConnectionSource, SomeServerConnector, acceptSomeConnector)
import Network.Protocol.Driver (runSomeConnector)
import Network.Protocol.Handshake.Types (Handshake)
import Network.Protocol.Job.Types (Job)
import Network.TypedProtocol (Driver)
import UnliftIO (MonadUnliftIO(..))

data ProxyDependencies m = forall dState. ProxyDependencies
  { getMarloweSyncDriver :: m (Driver (Handshake MarloweSync) dState m)
  , getMarloweHeaderSyncDriver :: m (Driver (Handshake MarloweHeaderSync) dState m)
  , getMarloweQueryDriver :: m (Driver (Handshake MarloweQuery) dState m)
  , getTxJobDriver :: m (Driver (Handshake (Job MarloweTxCommand)) dState m)
  , connectionSource :: SomeConnectionSource MarloweRuntimeServer m
  }

proxy :: MonadUnliftIO m => Component m (ProxyDependencies (ResourceT m)) Probes
proxy = proc deps -> do
  (serverComponent (component_ worker) \ProxyDependencies{..} -> do
    connector <- runResourceT $ acceptSomeConnector connectionSource
    pure WorkerDependencies{..}) -< deps
  returnA -< Probes
    { startup = pure True
    , liveness = pure True
    , readiness = pure True
    }

data WorkerDependencies m = forall dState. WorkerDependencies
  { getMarloweSyncDriver :: m (Driver (Handshake MarloweSync) dState m)
  , getMarloweHeaderSyncDriver :: m (Driver (Handshake MarloweHeaderSync) dState m)
  , getMarloweQueryDriver :: m (Driver (Handshake MarloweQuery) dState m)
  , getTxJobDriver :: m (Driver (Handshake (Job MarloweTxCommand)) dState m)
  , connector :: SomeServerConnector MarloweRuntimeServer m
  }

worker :: MonadUnliftIO m => WorkerDependencies (ResourceT m) -> m ()
worker WorkerDependencies{..} = runResourceT $ runSomeConnector connector MarloweRuntimeServer
  { result = ()
  , getMarloweSyncDriver = wrapDriver <$> getMarloweSyncDriver
  , getMarloweHeaderSyncDriver = wrapDriver <$> getMarloweHeaderSyncDriver
  , getMarloweQueryDriver = wrapDriver <$> getMarloweQueryDriver
  , getTxJobDriver = wrapDriver <$> getTxJobDriver
  }
