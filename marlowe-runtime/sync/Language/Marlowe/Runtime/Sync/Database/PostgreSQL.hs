module Language.Marlowe.Runtime.Sync.Database.PostgreSQL
  where

import qualified Hasql.Session as H
import qualified Hasql.Transaction.Sessions as T
import Language.Marlowe.Runtime.Sync.Database (DatabaseQueries(DatabaseQueries))
import Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetContractState (getContractState)
import Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetCreateStep (getCreateStep)
import Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetHeaders (getHeaders)
import Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetIntersection (getIntersection)
import Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetIntersectionForContract (getIntersectionForContract)
import Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetNextHeaders (getNextHeaders)
import Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetNextSteps (getNextSteps)
import Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetTip (getTip)
import Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetTipForContract (getTipForContract)
import Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetTransaction (getTransaction)
import Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetTransactions (getTransactions)
import Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetWithdrawal (getWithdrawal)
import Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetWithdrawals (getWithdrawals)

databaseQueries :: DatabaseQueries H.Session
databaseQueries = DatabaseQueries
  (T.transaction T.Serializable T.Read getTip)
  (T.transaction T.Serializable T.Read . getTipForContract)
  (T.transaction T.Serializable T.Read . getCreateStep)
  (T.transaction T.Serializable T.Read . getIntersection)
  (\contractId -> T.transaction T.Serializable T.Read . getIntersectionForContract contractId)
  (T.transaction T.Serializable T.Read . getNextHeaders)
  (\version contractId -> T.transaction T.Serializable T.Read . getNextSteps version contractId)
  (fmap (T.transaction T.Serializable T.Read) . getHeaders)
  (T.transaction T.Serializable T.Read . getContractState)
  (T.transaction T.Serializable T.Read . getTransaction)
  (T.transaction T.Serializable T.Read . getTransactions)
  (T.transaction T.Serializable T.Read . getWithdrawal)
  (fmap (T.transaction T.Serializable T.Read) . getWithdrawals)
