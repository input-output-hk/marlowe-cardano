module Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetNextSteps
  where

import qualified Hasql.Transaction as T
import Language.Marlowe.Runtime.ChainSync.Api (ChainPoint)
import Language.Marlowe.Runtime.Core.Api (ContractId)
import Language.Marlowe.Runtime.Sync.Database (NextSteps)

getNextSteps :: ContractId -> ChainPoint -> T.Transaction NextSteps
getNextSteps = undefined
