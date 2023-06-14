module Language.Marlowe.Runtime.Sync.QueryServer where

import Language.Marlowe.Protocol.Query.Server (MarloweQueryServer, marloweQueryServer)
import Language.Marlowe.Runtime.Sync.Database (DatabaseQueries(..))
import Network.Protocol.Connection (ServerSource(..))
import UnliftIO (MonadUnliftIO)

newtype QueryServerDependencies m = QueryServerDependencies
  { databaseQueries :: DatabaseQueries m
  }

queryServer :: MonadUnliftIO m => QueryServerDependencies m -> ServerSource MarloweQueryServer m ()
queryServer QueryServerDependencies{..} = ServerSource $ pure $ marloweQueryServer
  getHeaders
  getContractState
  getTransaction
  getTransactions
  getWithdrawal
  getWithdrawals
  where
    DatabaseQueries{..} = databaseQueries
