{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.Sync.Database
  where

import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader, ChainPoint)
import Language.Marlowe.Runtime.Core.Api (ContractId, MarloweVersion)
import Language.Marlowe.Runtime.History.Api (ContractStep, SomeCreateStep)

hoistDatabaseQueries :: (forall x. m x -> n x) -> DatabaseQueries m -> DatabaseQueries n
hoistDatabaseQueries f DatabaseQueries{..} = DatabaseQueries
  { getTipForContract = f . getTipForContract
  , getCreateStep = f . getCreateStep
  , getIntersectionForContract = fmap f . getIntersectionForContract
  , getNextSteps = fmap f . getNextSteps
  }

data DatabaseQueries m = DatabaseQueries
  { getTipForContract :: ContractId -> m ChainPoint
  , getCreateStep :: ContractId -> m (Maybe (BlockHeader, SomeCreateStep))
  , getIntersectionForContract :: ContractId -> [BlockHeader] -> m ChainPoint
  , getNextSteps :: ContractId -> ChainPoint -> m NextSteps
  }

data NextSteps
  = Rollback ChainPoint
  | Wait BlockHeader
  | forall v. Next (MarloweVersion v) BlockHeader [ContractStep v]
