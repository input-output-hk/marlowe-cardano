{-# LANGUAGE DuplicateRecordFields #-}

module Language.Marlowe.Runtime.Sync.QueryServer
  where

import Control.Concurrent.Component
import Language.Marlowe.Protocol.Query.Server (MarloweQueryServer, marloweQueryServer)
import Language.Marlowe.Runtime.Sync.Database (DatabaseQueries(..))
import Network.Protocol.Driver (RunServer(..))

data QueryServerDependencies = QueryServerDependencies
  { databaseQueries :: DatabaseQueries IO
  , acceptRunMarloweQueryServer :: IO (RunServer IO MarloweQueryServer)
  }

queryServer :: Component IO QueryServerDependencies ()
queryServer = serverComponent (component_ worker) \QueryServerDependencies{..} -> do
  runMarloweQueryServer <- acceptRunMarloweQueryServer
  pure WorkerDependencies{..}

data WorkerDependencies = WorkerDependencies
  { databaseQueries :: DatabaseQueries IO
  , runMarloweQueryServer :: RunServer IO MarloweQueryServer
  }

worker :: WorkerDependencies -> IO ()
worker WorkerDependencies{..} = do
  let DatabaseQueries{..} = databaseQueries
  let RunServer runServer = runMarloweQueryServer
  runServer $ marloweQueryServer getHeaders getContractState getTransaction
