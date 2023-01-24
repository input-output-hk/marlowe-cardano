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
  , getIntersectionForContract = (fmap . fmap) f . getIntersectionForContract
  , getNextSteps = (fmap . fmap) f . getNextSteps
  }

data DatabaseQueries m = DatabaseQueries
  { getTipForContract :: ContractId -> m ChainPoint
  , getCreateStep :: ContractId -> m (Maybe (BlockHeader, SomeCreateStep))
  , getIntersectionForContract :: forall v. ContractId -> MarloweVersion v -> [BlockHeader] -> m ChainPoint
  , getNextSteps :: forall v. ContractId -> MarloweVersion v -> ChainPoint -> m (NextSteps v)
  }

data NextSteps v
  = Rollback ChainPoint
  | Wait BlockHeader
  | Next BlockHeader [ContractStep v]
