{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Marlowe.Runtime.Sync.QueryServer
  where

import Control.Concurrent.Component
import Language.Marlowe.Protocol.Query.Server (MarloweQueryServer, marloweQueryServer)
import Language.Marlowe.Runtime.Sync.Database (DatabaseQueries(..))
import Network.Protocol.Connection (SomeConnectionSource, SomeServerConnector, acceptSomeConnector)
import Network.Protocol.Driver (runSomeConnector)
import UnliftIO (MonadUnliftIO)

data QueryServerDependencies m = QueryServerDependencies
  { databaseQueries :: DatabaseQueries m
  , querySource :: SomeConnectionSource MarloweQueryServer m
  }

queryServer :: MonadUnliftIO m => Component m (QueryServerDependencies m) ()
queryServer = serverComponent (component_ worker) \QueryServerDependencies{..} -> do
  connector <- acceptSomeConnector querySource
  pure WorkerDependencies{..}

data WorkerDependencies m = WorkerDependencies
  { databaseQueries :: DatabaseQueries m
  , connector :: SomeServerConnector MarloweQueryServer m
  }

worker :: MonadUnliftIO m => WorkerDependencies m -> m ()
worker WorkerDependencies{..} = do
  let DatabaseQueries{..} = databaseQueries
  runSomeConnector connector $ marloweQueryServer
    getHeaders
    getContractState
    getTransaction
    getTransactions
    getWithdrawal
    getWithdrawals
