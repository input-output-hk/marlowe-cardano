module Language.Marlowe.Runtime.Indexer.Database.PostgreSQL
  where

import qualified Hasql.Session as H
import qualified Hasql.Transaction.Sessions as H
import qualified Language.Marlowe.Runtime.Indexer.Database as DB
import Language.Marlowe.Runtime.Indexer.Database.PostgreSQL.CommitBlocks (commitBlocks)
import Language.Marlowe.Runtime.Indexer.Types (MarloweUTxO(MarloweUTxO))

databaseQueries :: DB.DatabaseQueries H.Session
databaseQueries = DB.DatabaseQueries
  { commitRollback = const $ pure ()
  , commitBlocks = H.transaction H.Serializable H.Write . commitBlocks
  , getIntersectionPoints = pure []
  , getMarloweUTxO = const $ pure $ MarloweUTxO mempty mempty
  }
