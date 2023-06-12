{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Marlowe.Runtime.Sync
  where

import Colog (Message, WithLog)
import Control.Arrow (returnA)
import Control.Concurrent.Component
import Control.Concurrent.Component.Probes
import Control.Monad.Event.Class (MonadEvent)
import Language.Marlowe.Protocol.HeaderSync.Server (MarloweHeaderSyncServer)
import Language.Marlowe.Protocol.Query.Server (MarloweQueryServer)
import Language.Marlowe.Protocol.Sync.Server (MarloweSyncServer)
import Language.Marlowe.Runtime.Sync.Database (DatabaseQueries)
import Language.Marlowe.Runtime.Sync.MarloweHeaderSyncServer
import Language.Marlowe.Runtime.Sync.MarloweSyncServer
import Language.Marlowe.Runtime.Sync.QueryServer
import Network.Protocol.Connection (SomeConnectionSourceTraced)
import Network.Protocol.Driver.Trace (HasSpanContext)
import UnliftIO (MonadUnliftIO)

data SyncDependencies r s m = SyncDependencies
  { databaseQueries :: DatabaseQueries m
  , syncSource :: SomeConnectionSourceTraced MarloweSyncServer r s m
  , headerSyncSource :: SomeConnectionSourceTraced MarloweHeaderSyncServer r s m
  , querySource :: SomeConnectionSourceTraced MarloweQueryServer r s m
  }

sync :: (MonadUnliftIO m, HasSpanContext r, MonadEvent r s m, WithLog env Message m) => Component m (SyncDependencies r s m) Probes
sync = proc SyncDependencies{..} -> do
  marloweSyncServer -< MarloweSyncServerDependencies{..}
  marloweHeaderSyncServer -< MarloweHeaderSyncServerDependencies{..}
  queryServer -< QueryServerDependencies{..}
  returnA -< Probes
    { startup = pure True
    , liveness = pure True
    , readiness = pure True
    }
