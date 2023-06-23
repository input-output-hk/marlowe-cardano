module Language.Marlowe.Runtime.Sync.QueryServer where

import Data.Version (Version)
import Language.Marlowe.Protocol.Query.Server (MarloweQueryServer, marloweQueryServer)
import Language.Marlowe.Runtime.ChainSync.Api (ChainSyncQuery)
import Language.Marlowe.Runtime.Sync.Database (DatabaseQueries (..))
import Network.Protocol.Connection (Connector, ServerSource (..))
import Network.Protocol.Query.Client (QueryClient)
import UnliftIO (MonadUnliftIO)

data QueryServerDependencies m = QueryServerDependencies
  { runtimeVersion :: Version
  , chainSyncQueryConnector :: Connector (QueryClient ChainSyncQuery) m
  , databaseQueries :: DatabaseQueries m
  }

queryServer :: (MonadUnliftIO m) => QueryServerDependencies m -> ServerSource MarloweQueryServer m ()
queryServer QueryServerDependencies{..} =
  ServerSource $
    pure $
      marloweQueryServer
        runtimeVersion
        chainSyncQueryConnector
        getTip
        getHeaders
        getContractState
        getTransaction
        getTransactions
        getWithdrawal
        getWithdrawals
  where
    DatabaseQueries{..} = databaseQueries
