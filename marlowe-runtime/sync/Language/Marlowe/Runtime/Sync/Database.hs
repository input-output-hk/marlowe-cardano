{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.Sync.Database
  where

import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader, ChainPoint)
import Language.Marlowe.Runtime.Core.Api (ContractId, MarloweVersion)
import Language.Marlowe.Runtime.History.Api (ContractStep, SomeCreateStep)

hoistDatabaseQueries :: (forall x. m x -> n x) -> DatabaseQueries m -> DatabaseQueries n
hoistDatabaseQueries f DatabaseQueries{..} = DatabaseQueries
  { getTip = f getTip
  , getTipForContract = f . getTipForContract
  , getCreateStep = f . getCreateStep
  , getIntersection = f . getIntersection
  , getNextSteps = (fmap . fmap) f . getNextSteps
  }

data DatabaseQueries m = DatabaseQueries
  { getTip :: m ChainPoint
  , getTipForContract :: ContractId -> m ChainPoint
  , getCreateStep :: ContractId -> m (Maybe (BlockHeader, SomeCreateStep))
  , getIntersection :: [BlockHeader] -> m (Maybe BlockHeader)
  , getNextSteps :: forall v. ContractId -> MarloweVersion v -> ChainPoint -> m (NextSteps v)
  }

data NextSteps v
  = Rollback ChainPoint
  | Wait BlockHeader
  | Next BlockHeader [ContractStep v]
