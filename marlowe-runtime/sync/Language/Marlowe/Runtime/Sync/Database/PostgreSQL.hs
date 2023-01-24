module Language.Marlowe.Runtime.Sync.Database.PostgreSQL
  where

import qualified Hasql.Session as H
import qualified Hasql.Transaction.Sessions as T
import Language.Marlowe.Runtime.Sync.Database (DatabaseQueries(DatabaseQueries))
import Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetCreateStep (getCreateStep)
import Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetIntersectionForContract (getIntersectionForContract)
import Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetNextSteps (getNextSteps)
import Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetTipForContract (getTipForContract)

databaseQueries :: DatabaseQueries H.Session
databaseQueries = DatabaseQueries
  (T.transaction T.Serializable T.Read . getTipForContract)
  (T.transaction T.Serializable T.Read . getCreateStep)
  (\contractId version -> T.transaction T.Serializable T.Read . getIntersectionForContract contractId version)
  (\contractId version -> T.transaction T.Serializable T.Read . getNextSteps contractId version)
