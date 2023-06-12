{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Marlowe.Runtime.Sync.QueryServer
  where

import Colog (Message, WithLog)
import Control.Concurrent.Component
import Control.Monad.Event.Class
import Language.Marlowe.Protocol.Query.Server (MarloweQueryServer, marloweQueryServer)
import Language.Marlowe.Runtime.Sync.Database (DatabaseQueries(..))
import Network.Protocol.Connection (SomeConnectionSourceTraced, SomeServerConnectorTraced, acceptSomeConnectorTraced)
import Network.Protocol.Driver.Trace (HasSpanContext, runSomeConnectorTraced)
import UnliftIO (MonadUnliftIO)

data QueryServerDependencies r s m = QueryServerDependencies
  { databaseQueries :: DatabaseQueries m
  , querySource :: SomeConnectionSourceTraced MarloweQueryServer r s m
  }

queryServer :: (MonadUnliftIO m, MonadEvent r s m, HasSpanContext r, WithLog env Message m) => Component m (QueryServerDependencies r s m) ()
queryServer = serverComponent "sync-query-server" (component_ "sync-query-worker" worker) \QueryServerDependencies{..} -> do
  connector <- acceptSomeConnectorTraced querySource
  pure WorkerDependencies{..}

data WorkerDependencies r s m = WorkerDependencies
  { databaseQueries :: DatabaseQueries m
  , connector :: SomeServerConnectorTraced MarloweQueryServer r s m
  }

worker :: (MonadUnliftIO m, MonadEvent r s m, HasSpanContext r) => WorkerDependencies r s m -> m ()
worker WorkerDependencies{..} = do
  let DatabaseQueries{..} = databaseQueries
  runSomeConnectorTraced connector $ marloweQueryServer
    getHeaders
    getContractState
    getTransaction
    getTransactions
    getWithdrawal
    getWithdrawals
