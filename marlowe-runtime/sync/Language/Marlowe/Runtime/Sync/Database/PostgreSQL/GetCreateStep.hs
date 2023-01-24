module Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetCreateStep
  where

import qualified Hasql.Transaction as T
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader)
import Language.Marlowe.Runtime.Core.Api (ContractId)
import Language.Marlowe.Runtime.History.Api (SomeCreateStep)

getCreateStep :: ContractId -> T.Transaction (Maybe (BlockHeader, SomeCreateStep))
getCreateStep = undefined
