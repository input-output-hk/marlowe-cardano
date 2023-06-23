{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.Indexer.Database.PostgreSQL where

import Control.Monad.Event.Class (MonadInjectEvent, withEvent)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Hasql.Pool (Pool)
import qualified Hasql.Pool as Pool
import qualified Hasql.Session as Session
import Hasql.Transaction (Transaction)
import qualified Hasql.Transaction.Sessions as H
import qualified Hasql.Transaction.Sessions as TS
import qualified Language.Marlowe.Runtime.Indexer.Database as DB
import Language.Marlowe.Runtime.Indexer.Database.PostgreSQL.CommitBlocks (commitBlocks)
import Language.Marlowe.Runtime.Indexer.Database.PostgreSQL.CommitRollback (commitRollback)
import Language.Marlowe.Runtime.Indexer.Database.PostgreSQL.GetIntersectionPoints (getIntersectionPoints)
import Language.Marlowe.Runtime.Indexer.Database.PostgreSQL.GetMarloweUTxO (getMarloweUTxO)
import Observe.Event (addField)
import UnliftIO (throwIO)

data QuerySelector f where
  Query :: Text -> QuerySelector QueryField

data QueryField
  = SqlStatement ByteString
  | Parameters [Text]
  | Operation Text

databaseQueries
  :: forall r s m. (MonadInjectEvent r QuerySelector s m, MonadIO m) => Pool -> Int -> DB.DatabaseQueries m
databaseQueries pool securityParameter =
  DB.DatabaseQueries
    { commitRollback = transact "commitRollback" "INSERT" H.Write . commitRollback
    , commitBlocks = transact "commitBlocks" "INSERT" H.Write . commitBlocks
    , getIntersectionPoints = transact "getIntersectionPoints" "SELECT" H.Read $ getIntersectionPoints securityParameter
    , getMarloweUTxO = transact "getMarloweUTxO" "SELECT" H.Read . getMarloweUTxO
    }
  where
    transact :: Text -> Text -> TS.Mode -> Transaction a -> m a
    transact queryName operation mode m = withEvent (Query queryName) \ev -> do
      addField ev $ Operation operation
      result <- liftIO $ Pool.use pool $ TS.transaction TS.Serializable mode m
      case result of
        Left ex -> do
          case ex of
            (Pool.SessionUsageError (Session.QueryError sql params _)) -> do
              addField ev $ SqlStatement sql
              addField ev $ Parameters params
            _ -> pure ()
          throwIO ex
        Right a -> pure a
