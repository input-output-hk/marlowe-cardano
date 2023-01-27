{-# LANGUAGE Arrows #-}

module Language.Marlowe.Runtime.Sync
  where

import Control.Concurrent.Component
import Language.Marlowe.Protocol.Sync.Server (MarloweSyncServer)
import Language.Marlowe.Runtime.Sync.Database (DatabaseQueries)
import Language.Marlowe.Runtime.Sync.MarloweSyncServer (MarloweSyncServerDependencies(..), marloweSyncServer)
import Network.Protocol.Driver (RunServer)

data SyncDependencies = SyncDependencies
  { databaseQueries :: DatabaseQueries IO
  , acceptRunMarloweSyncServer :: IO (RunServer IO MarloweSyncServer)
  }

sync :: Component IO SyncDependencies ()
sync = proc SyncDependencies{..} -> do
  marloweSyncServer -< MarloweSyncServerDependencies{..}
