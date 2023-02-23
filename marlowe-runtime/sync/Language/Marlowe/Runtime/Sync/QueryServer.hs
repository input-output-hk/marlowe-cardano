{-# LANGUAGE DuplicateRecordFields #-}

module Language.Marlowe.Runtime.Sync.QueryServer
  where

import Control.Concurrent.Component
import Language.Marlowe.Protocol.Query.Server (MarloweQueryServer, marloweQueryServer)
import Language.Marlowe.Runtime.Sync.Database (DatabaseQueries(..))
import Network.Protocol.Connection (SomeConnectionSource, SomeServerConnector, acceptSomeConnector)
import Network.Protocol.Driver (runSomeConnector)

data QueryServerDependencies = QueryServerDependencies
  { databaseQueries :: DatabaseQueries IO
  , querySource :: SomeConnectionSource MarloweQueryServer IO
  }

queryServer :: Component IO QueryServerDependencies ()
queryServer = serverComponent (component_ worker) \QueryServerDependencies{..} -> do
  connector <- acceptSomeConnector querySource
  pure WorkerDependencies{..}

data WorkerDependencies = WorkerDependencies
  { databaseQueries :: DatabaseQueries IO
  , connector :: SomeServerConnector MarloweQueryServer IO
  }

worker :: WorkerDependencies -> IO ()
worker WorkerDependencies{..} = do
  let DatabaseQueries{..} = databaseQueries
  runSomeConnector connector $ marloweQueryServer
    getHeaders
    getContractState
    getTransaction
    getTransactions
    getWithdrawal
    getWithdrawals
