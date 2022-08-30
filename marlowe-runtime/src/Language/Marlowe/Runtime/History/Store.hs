{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.History.Store where

import Control.Concurrent.STM (STM, atomically, modifyTVar, newTVar, readTVarIO, writeTVar)
import Control.Concurrent.STM.TVar (readTVar)
import Control.Monad (forever, mfilter)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import Data.Semialign (Semialign (alignWith))
import Data.These (These (..))
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader, ChainPoint, WithGenesis (..))
import Language.Marlowe.Runtime.Core.Api (ContractId, MarloweVersion (..))
import Language.Marlowe.Runtime.History.Api (ContractStep, SomeCreateStep)
import Language.Marlowe.Runtime.History.Follower (ContractChanges (..), SomeContractChanges (..))
import Language.Marlowe.Runtime.History.FollowerSupervisor (UpdateContract (..))

data HistoryQueries m = HistoryQueries
  { findCreateStep   :: ContractId -> m (Maybe (BlockHeader, SomeCreateStep))
  , findIntersection :: ContractId -> [BlockHeader] -> m (Maybe Intersection)
  , findNextSteps    :: ContractId -> ChainPoint -> m FindNextStepsResponse
  , commitChanges    :: Map ContractId UpdateContract -> m ()
  }

hoistHistoryQueries :: (forall x. m x -> n x) -> HistoryQueries m -> HistoryQueries n
hoistHistoryQueries nat HistoryQueries{..} = HistoryQueries
  { findCreateStep = nat . findCreateStep
  , findIntersection = fmap nat . findIntersection
  , findNextSteps = fmap nat . findNextSteps
  , commitChanges = nat . commitChanges
  }

data HistoryStoreDependencies = HistoryStoreDependencies
  { changes        :: STM (Map ContractId UpdateContract)
  , historyQueries :: HistoryQueries IO
  }

data HistoryStore = HistoryStore
  { runHistoryStore   :: IO ()
  , findContract      :: ContractId -> IO (Maybe (BlockHeader, SomeCreateStep))
  , intersectContract :: forall v. ContractId -> MarloweVersion v -> [BlockHeader] -> IO (Maybe BlockHeader)
  , getNextSteps      :: forall v. ContractId -> MarloweVersion v -> ChainPoint -> IO (GetNextStepsResponse v)
  }

data GetNextStepsResponse v
  = Rollback ChainPoint
  | Wait BlockHeader (STM ChainPoint)
  | Next BlockHeader [ContractStep v]

data FindNextStepsResponse
  = FindRollback ChainPoint
  | FindWait BlockHeader
  | FindNext BlockHeader SomeContractSteps

data Intersection = forall v. Intersection (MarloweVersion v) BlockHeader

data SomeContractSteps = forall v. SomeContractSteps (MarloweVersion v) [ContractStep v]

mkHistoryStore :: HistoryStoreDependencies -> STM HistoryStore
mkHistoryStore HistoryStoreDependencies{..} = do
  latestBlocksPerContractVar <- newTVar Map.empty
  let
    runHistoryStore = forever do
      newChanges <- atomically awaitChanges
      commitChanges newChanges
      atomically $ updateLatestBlocks newChanges

    awaitChanges = mfilter (not . Map.null) changes

    updateLatestBlocks newChanges = do
      latestBlocksPerContract <- readTVar latestBlocksPerContractVar
      writeTVar latestBlocksPerContractVar
        =<< sequence (alignWith updateLatestBlock latestBlocksPerContract newChanges)

    updateLatestBlock = \case
      This latestBlockVar -> pure latestBlockVar
      That RemoveContract -> newTVar Nothing
      That (UpdateContract (SomeContractChanges _ contractChanges)) ->
        newTVar $ latestBlockFromContractChanges contractChanges
      These latestBlockVar RemoveContract -> do
        writeTVar latestBlockVar Nothing
        pure latestBlockVar
      These latestBlockVar (UpdateContract (SomeContractChanges _ contractChanges)) -> do
        modifyTVar latestBlockVar \latestBlock ->
          max latestBlock (latestBlockFromContractChanges contractChanges)
        pure latestBlockVar
      where
        latestBlockFromContractChanges ContractChanges{..} =
          max (fst <$> create) $ fst <$> listToMaybe (Map.toDescList steps)

    findContract = findCreateStep

    intersectContract :: ContractId -> MarloweVersion v -> [BlockHeader] -> IO (Maybe BlockHeader)
    intersectContract contractId version headers = runMaybeT do
      Intersection version' blockHeader <- MaybeT $ findIntersection contractId headers
      case (version, version') of
        (MarloweV1, MarloweV1) -> pure blockHeader

    getNextSteps :: ContractId -> MarloweVersion v -> ChainPoint -> IO (GetNextStepsResponse v)
    getNextSteps contractId version point = do
      latestBlocksPerContract <- readTVarIO latestBlocksPerContractVar
      case Map.lookup contractId latestBlocksPerContract of
        Nothing -> pure $ Rollback Genesis
        Just latestBlockVar -> do
          result <- findNextSteps contractId point
          case result of
            FindRollback point'      -> pure $ Rollback point'
            FindWait lastBlockHeader -> pure $ Wait lastBlockHeader $ maybe Genesis At <$> readTVar latestBlockVar
            FindNext blockHeader (SomeContractSteps version' steps) -> case (version, version') of
              (MarloweV1, MarloweV1) -> pure $ Next blockHeader steps

  pure HistoryStore{..}
  where
    HistoryQueries{..} = historyQueries
