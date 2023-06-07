{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Marlowe.Runtime.Sync where

import Colog (Message, WithLog)
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
import Network.Protocol.Connection (ConnectionSource)
import UnliftIO (MonadUnliftIO)

data SyncDependencies m = SyncDependencies
  { databaseQueries :: DatabaseQueries m
  , syncSource :: ConnectionSource MarloweSyncServer m
  , headerSyncSource :: ConnectionSource MarloweHeaderSyncServer m
  , querySource :: ConnectionSource MarloweQueryServer m
  }

sync :: (MonadUnliftIO m, WithLog env Message m) => Component m (SyncDependencies m) Probes
sync = proc SyncDependencies{..} -> do
  marloweSyncServer -< MarloweSyncServerDependencies{..}
  marloweHeaderSyncServer -< MarloweHeaderSyncServerDependencies{..}
  queryServer -< QueryServerDependencies{..}
  returnA -< Probes
    { startup = pure True
    , liveness = pure True
    , readiness = pure True
    }
