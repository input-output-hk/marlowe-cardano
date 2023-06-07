{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Marlowe.Runtime.Sync.QueryServer where

import Colog (Message, WithLog)
import Control.Concurrent.Component
import Language.Marlowe.Protocol.Query.Server (MarloweQueryServer, marloweQueryServer)
import Language.Marlowe.Runtime.Sync.Database (DatabaseQueries(..))
import Network.Protocol.Connection (ConnectionSource, Connector, acceptConnector, runConnector)
import UnliftIO (MonadUnliftIO)

data QueryServerDependencies m = QueryServerDependencies
  { databaseQueries :: DatabaseQueries m
  , querySource :: ConnectionSource MarloweQueryServer m
  }

queryServer :: (MonadUnliftIO m, WithLog env Message m) => Component m (QueryServerDependencies m) ()
queryServer = serverComponent "sync-query-server" (component_ "sync-query-worker" worker) \QueryServerDependencies{..} -> do
  connector <- acceptConnector querySource
  pure WorkerDependencies{..}

data WorkerDependencies m = WorkerDependencies
  { databaseQueries :: DatabaseQueries m
  , connector :: Connector MarloweQueryServer m
  }

worker :: MonadUnliftIO m => WorkerDependencies m -> m ()
worker WorkerDependencies{..} = do
  let DatabaseQueries{..} = databaseQueries
  runConnector connector $ marloweQueryServer
    getHeaders
    getContractState
    getTransaction
    getTransactions
    getWithdrawal
    getWithdrawals
