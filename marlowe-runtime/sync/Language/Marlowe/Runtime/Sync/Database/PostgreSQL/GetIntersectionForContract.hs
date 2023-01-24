module Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetIntersectionForContract
  where

import qualified Hasql.Transaction as T
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader, ChainPoint)
import Language.Marlowe.Runtime.Core.Api (ContractId, MarloweVersion)

getIntersectionForContract :: ContractId -> MarloweVersion v -> [BlockHeader] -> T.Transaction ChainPoint
getIntersectionForContract = undefined
