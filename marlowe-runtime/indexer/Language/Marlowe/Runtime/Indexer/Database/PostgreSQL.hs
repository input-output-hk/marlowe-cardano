module Language.Marlowe.Runtime.Indexer.Database.PostgreSQL
  where

import qualified Hasql.Session as H
import qualified Hasql.Transaction.Sessions as H
import qualified Language.Marlowe.Runtime.Indexer.Database as DB
import Language.Marlowe.Runtime.Indexer.Database.PostgreSQL.CommitBlocks (commitBlocks)
import Language.Marlowe.Runtime.Indexer.Database.PostgreSQL.CommitRollback (commitRollback)
import Language.Marlowe.Runtime.Indexer.Database.PostgreSQL.GetIntersectionPoints (getIntersectionPoints)
import Language.Marlowe.Runtime.Indexer.Types (MarloweUTxO(MarloweUTxO))

databaseQueries :: Int -> DB.DatabaseQueries H.Session
databaseQueries securityParameter = DB.DatabaseQueries
  { commitRollback = H.transaction H.Serializable H.Write . commitRollback
  , commitBlocks = H.transaction H.Serializable H.Write . commitBlocks
  , getIntersectionPoints = H.transaction H.Serializable H.Read $ getIntersectionPoints securityParameter
  , getMarloweUTxO = const $ pure $ MarloweUTxO mempty mempty
  }
