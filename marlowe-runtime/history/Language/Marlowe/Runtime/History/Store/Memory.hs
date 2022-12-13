{-# LANGUAGE GADTs #-}
module Language.Marlowe.Runtime.History.Store.Memory
  where

import Control.Concurrent.STM (STM, TVar, modifyTVar, newTVar, readTVar, writeTVar)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT, runMaybeT))
import Data.Foldable (fold)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semialign (alignWith)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.These (These(..))
import Data.Type.Equality ((:~:)(Refl))
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader, ChainPoint, WithGenesis(..))
import Language.Marlowe.Runtime.Core.Api (ContractId, assertVersionsEqual)
import Language.Marlowe.Runtime.History.Api (History(..), SomeCreateStep(..), SomeHistory(..))
import qualified Language.Marlowe.Runtime.History.Api as Api
import Language.Marlowe.Runtime.History.Follower (ContractChanges(..), SomeContractChanges(..))
import Language.Marlowe.Runtime.History.FollowerSupervisor (UpdateContract(..))
import Language.Marlowe.Runtime.History.Store
  (FindNextStepsResponse(..), HistoryQueries(..), Intersection(..), SomeContractSteps(..))
import Witherable (catMaybes)

-- | Creates an in-memory store of contract history that uses STM primitives
-- for storage.
mkHistoryQueriesInMemory :: STM (HistoryQueries STM)
mkHistoryQueriesInMemory = do
  -- A TVar containing a map of TVars containing contact histories. This allows
  -- individual histories to be updated without having to update the entire map
  -- structure, decreasing the likelihood of transaction conflicts.
  historyVarsVar :: TVar (Map ContractId (TVar SomeHistory)) <- newTVar Map.empty
  -- A TVar containing a record of rollbacks, so we know which block to roll
  -- back to when making a request from a rolled back block.
  rollbacksVar :: TVar (Map BlockHeader ChainPoint) <- newTVar Map.empty
  let
    -- Get the history for a contract by its ID.
    getHistory :: ContractId -> MaybeT STM SomeHistory
    getHistory contractId = do
      historyVars <- lift $ readTVar historyVarsVar
      MaybeT $ traverse readTVar $ Map.lookup contractId historyVars

    findCreateStep :: ContractId -> STM (Maybe (BlockHeader, SomeCreateStep))
    findCreateStep contractId = runMaybeT do
      SomeHistory version History{..} <- getHistory contractId
      pure (createBlock, SomeCreateStep version create)

    -- Find the greatest common block between the provided list and the block
    -- headers in a contract's history.
    findIntersection :: ContractId -> [BlockHeader] -> STM (Maybe Intersection)
    findIntersection contractId headers = runMaybeT do
      SomeHistory version History{..} <- getHistory contractId
      let
        allHeadersInHistory :: Set BlockHeader
        allHeadersInHistory = Set.insert createBlock $ Map.keysSet steps

        greatestCommonBlock :: Maybe BlockHeader
        greatestCommonBlock = fst <$> Set.maxView (Set.intersection allHeadersInHistory $ Set.fromList headers)
      MaybeT $ pure $ Intersection version <$> greatestCommonBlock

    -- Find the block to roll the given point back to recursively until a point
    -- which has not been rolled back is found.
    findRollback :: Maybe ChainPoint -> BlockHeader -> Map BlockHeader ChainPoint -> Maybe ChainPoint
    findRollback candidate fromBlock rollbacks = case Map.lookup fromBlock rollbacks of
      Nothing                -> candidate
      Just Genesis           -> Just Genesis
      Just (At newCandidate) -> findRollback (Just $ At newCandidate) newCandidate rollbacks

    -- Find the next steps in a contract's history after the given point.
    findNextSteps :: ContractId -> ChainPoint -> STM FindNextStepsResponse
    findNextSteps contractId afterPoint = do
      mHistory <- runMaybeT $ getHistory contractId
      mRollback <- case afterPoint of
        -- No way genesis has been rolled back.
        Genesis       -> pure Nothing
        At afterBlock -> findRollback Nothing afterBlock <$> readTVar rollbacksVar
      pure case (mRollback, mHistory) of
        -- If the history is not found, roll the client back to genesis
        (_, Nothing) -> FindRollback Genesis
        -- If the current point has been rolled back, send the rollback through.
        (Just rollbackPoint, _) -> FindRollback rollbackPoint
        (_, Just (SomeHistory version History{..})) ->
          -- Partition the history into entries at or before the given point on
          -- the left, and entries after it on the right. The left-hand entries
          -- will be in reverse order (so the head of the list will be the most
          -- recent entry that has occurred as of the given chain point).
          case break' ((<= afterPoint) . At . fst) $ Map.toAscList steps of
            -- There is at least one set of steps at a point in the future.
            (_, (blockHeader, steps') : _) -> FindNext blockHeader $ SomeContractSteps version steps'
            -- There are no steps in the future, and at least one set of steps
            -- at or before the given point.
            ((blockHeader, _) : _, [])     -> FindWait blockHeader
            -- There are no steps, so the most recent step was the create step.
            ([], [])                       -> FindWait createBlock
      where
        -- | Like Prelude.break but the left hand list will be in reverse
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
        Nothing                    -> case fold steps of
          -- Nothing to create and no steps to add = no-op
          [] -> pure Nothing
          -- adding steps without the contract existing is an error
          _  -> error "The first UpdateContract request must provide a create step"
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
            updateSteps prevSteps' = case assertVersionsEqual version version' of
              Refl -> do
                writeTVar historyVar $ SomeHistory version history { Api.steps = Map.unionWith (<>) prevSteps' steps }
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
              | otherwise -> do
                  -- Figure out which steps remain after the rollback, and
                  -- which ones don't
                  let (notRolledBack, rolledBack) = break ((> rollbackToPoint) . At . fst) $ Map.toAscList prevSteps
                  -- Mark the points of the rolled back steps with the point
                  -- they were rolled back to.
                  modifyTVar rollbacksVar $ Map.union $ Map.fromSet (const rollbackToPoint) $ Set.fromList $ fst <$> rolledBack
                  -- Update the stored steps.
                  updateSteps $ Map.fromDistinctAscList notRolledBack
        -- The request specifies a new contract, programmer error!
        Just _ -> error "Cannot provide a create step for the same contract more than once"

  pure HistoryQueries{..}
