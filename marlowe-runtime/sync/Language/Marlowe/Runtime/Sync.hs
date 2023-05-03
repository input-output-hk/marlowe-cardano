{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Marlowe.Runtime.Sync
  where

import Control.Arrow (returnA)
import Control.Concurrent.Component
import Control.Concurrent.Component.Probes
import Language.Marlowe.Protocol.HeaderSync.Server (MarloweHeaderSyncServer)
import Language.Marlowe.Protocol.Query.Server (MarloweQueryServer)
import Language.Marlowe.Protocol.Sync.Server (MarloweSyncServer)
import Language.Marlowe.Runtime.Sync.Database (DatabaseQueries)
import Language.Marlowe.Runtime.Sync.MarloweHeaderSyncServer
import Language.Marlowe.Runtime.Sync.MarloweSyncServer
import Language.Marlowe.Runtime.Sync.QueryServer
import Network.Protocol.Connection (SomeConnectionSource)
import UnliftIO (MonadUnliftIO)

data SyncDependencies m = SyncDependencies
  { databaseQueries :: DatabaseQueries m
  , syncSource :: SomeConnectionSource MarloweSyncServer m
  , headerSyncSource :: SomeConnectionSource MarloweHeaderSyncServer m
  , querySource :: SomeConnectionSource MarloweQueryServer m
  }

sync :: MonadUnliftIO m => Component m (SyncDependencies m) Probes
sync = proc SyncDependencies{..} -> do
  marloweSyncServer -< MarloweSyncServerDependencies{..}
  marloweHeaderSyncServer -< MarloweHeaderSyncServerDependencies{..}
  queryServer -< QueryServerDependencies{..}
  returnA -< Probes
    { startup = pure True
    , liveness = pure True
    , readiness = pure True
    }
