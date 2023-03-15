{-# LANGUAGE Arrows #-}

module Language.Marlowe.Runtime.Sync
  where

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

data SyncDependencies = SyncDependencies
  { databaseQueries :: DatabaseQueries IO
  , syncSource :: SomeConnectionSource MarloweSyncServer IO
  , headerSyncSource :: SomeConnectionSource MarloweHeaderSyncServer IO
  , querySource :: SomeConnectionSource MarloweQueryServer IO
  , httpPort :: Int
  }

sync :: Component IO SyncDependencies ()
sync = proc SyncDependencies{..} -> do
  marloweSyncServer -< MarloweSyncServerDependencies{..}
  marloweHeaderSyncServer -< MarloweHeaderSyncServerDependencies{..}
  queryServer -< QueryServerDependencies{..}
  probeServer -< ProbeServerDependencies
    { probes = Probes
        { startup = pure True
        , liveness = pure True
        , readiness = pure True
        }
    , port = httpPort
    }
