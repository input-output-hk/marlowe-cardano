{-# LANGUAGE GADTs #-}
module Language.Marlowe.Runtime.History.Store.Memory where

import Control.Concurrent.STM (STM, TVar, newTVar, readTVar, writeTVar)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semialign (alignWith)
import qualified Data.Set as Set
import Data.These (These (..))
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader, ChainPoint, WithGenesis (..))
import Language.Marlowe.Runtime.Core.Api (ContractId, MarloweVersion (..))
import Language.Marlowe.Runtime.History.Api (History (..), SomeCreateStep (..), SomeHistory (..))
import qualified Language.Marlowe.Runtime.History.Api as Api
import Language.Marlowe.Runtime.History.Follower (ContractChanges (..), SomeContractChanges (..))
import Language.Marlowe.Runtime.History.FollowerSupervisor (UpdateContract (..))
import Language.Marlowe.Runtime.History.Store (FindNextStepsResponse (..), HistoryQueries (..), Intersection (..),
                                               SomeContractSteps (..))
import Witherable (catMaybes)

mkHistoryQueriesInMemory :: STM (HistoryQueries STM)
mkHistoryQueriesInMemory = do
  historyVarsVar :: TVar (Map ContractId (TVar SomeHistory)) <- newTVar Map.empty
  let
    getHistory :: ContractId -> MaybeT STM SomeHistory
    getHistory contractId = do
      historyVars <- lift $ readTVar historyVarsVar
      MaybeT $ traverse readTVar $ Map.lookup contractId historyVars

    findCreateStep :: ContractId -> STM (Maybe (BlockHeader, SomeCreateStep))
    findCreateStep contractId = runMaybeT do
      SomeHistory version History{..} <- getHistory contractId
      pure (createBlock, SomeCreateStep version create)

    findIntersection :: ContractId -> [BlockHeader] -> STM (Maybe Intersection)
    findIntersection contractId headers = runMaybeT do
      SomeHistory version History{..} <- getHistory contractId
      let allHeadersInHistory = Set.insert createBlock $ Map.keysSet steps
      let greatestCommonBlock = fst <$> Set.maxView (Set.intersection allHeadersInHistory $ Set.fromList headers)
      MaybeT $ pure $ Intersection version <$> greatestCommonBlock

    findNextSteps :: ContractId -> ChainPoint -> STM FindNextStepsResponse
    findNextSteps contractId afterPoint = do
      mHistory <- runMaybeT $ getHistory contractId
      pure case mHistory of
        Nothing -> FindRollback Genesis
        Just (SomeHistory version History{..}) ->
          case break' ((<= afterPoint) . At . fst) $ Map.toAscList steps of
            (_, (blockHeader, steps') : _) -> FindNext blockHeader $ SomeContractSteps version steps'
            ((blockHeader, _) : _, [])     -> FindWait blockHeader
            ([], [])                       -> FindWait createBlock
      where
        -- | List Prelude.break but the left hand list will be in reverse
        -- order:
        --
        -- break' (<= 3) [0, 1, 3, 5, 7, 8] == ([3, 1, 0], [5, 7, 8])
        break' :: (a -> Bool) -> [a] -> ([a], [a])
        break' p = go []
          where
            go acc [] = (acc, [])
            go acc (a : as)
              | p a = go (a : acc) as
              | otherwise = (acc, a : as)

    commitChanges :: Map ContractId UpdateContract -> STM ()
    commitChanges updates = do
      historyVars <- readTVar historyVarsVar
      writeTVar historyVarsVar . catMaybes =<< sequence (alignWith updateHistory historyVars updates)

    updateHistory :: These (TVar SomeHistory) UpdateContract -> STM (Maybe (TVar SomeHistory))
    updateHistory = \case
      -- The history for this contract is not being changed - pass it through
      This historyVar     -> pure $ Just historyVar
      -- The history for this contract doesn't exist and there is a request to
      -- remove it. Do nothing
      That RemoveContract -> pure Nothing
      -- The history for this contract doesn't exist and there is a request to
      -- update it.
      That (UpdateContract (SomeContractChanges version ContractChanges{create = changesCreate, steps})) -> case changesCreate of
        Nothing                    -> error "The first UpdateContract request must provide a create step"
        Just (createBlock, create) -> Just <$> newTVar (SomeHistory version History{..})
      -- The history for this contract exists and there is a request to remove it.
      These _ RemoveContract -> pure Nothing
      -- The history for this contract exists and there is a request to update
      -- it.
      These historyVar (UpdateContract (SomeContractChanges version ContractChanges{..})) -> case create of
        -- The request does not specify a new contract, good.
        Nothing -> do
          SomeHistory version' history@History{createBlock, steps = prevSteps} <- readTVar historyVar
          let
            updateSteps prevSteps' = case (version, version') of
              (MarloweV1, MarloweV1) -> do
                writeTVar historyVar $ SomeHistory MarloweV1 history { Api.steps = Map.unionWith (<>) prevSteps' steps }
                pure $ Just historyVar
          case rollbackTo of
            -- No rollback to process, pass the steps from the current history
            -- through unchanged.
            Nothing -> updateSteps prevSteps
            -- No rollback to process
            Just rollbackToPoint
              -- Rollback would undo the creation of the contract. Remove the
              -- history TVar from the map.
              | rollbackToPoint < At createBlock -> pure Nothing
              -- Remove all steps that occurred after the rollback from the
              -- previous history.
              | otherwise -> updateSteps $ Map.fromDistinctAscList $ takeWhile ((<= rollbackToPoint) . At . fst) $ Map.toAscList prevSteps
        -- The request specifies a new contract, programmer error!
        Just _ -> error "Cannot provide a create step for the same contract more than once"

  pure HistoryQueries{..}
