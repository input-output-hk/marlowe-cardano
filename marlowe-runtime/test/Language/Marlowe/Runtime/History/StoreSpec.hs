{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.History.StoreSpec (spec) where

import Control.Concurrent (forkFinally, killThread)
import Control.Concurrent.STM
  (STM, TVar, atomically, modifyTVar, newEmptyTMVarIO, newTVar, putTMVar, readTVar, tryTakeTMVar, writeTVar)
import Control.Exception.Base (throwIO)
import Control.Monad (mfilter)
import Data.Either (fromRight)
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Some (Some(..), withSome)
import Data.Type.Equality (type (:~:)(Refl))
import Language.Marlowe.Runtime.ChainSync.Api (WithGenesis(..))
import Language.Marlowe.Runtime.Core.Api
  (ContractId, MarloweVersion, SomeMarloweVersion(..), Transaction(..), assertVersionsEqual)
import Language.Marlowe.Runtime.History.Api (ContractStep(..))
import Language.Marlowe.Runtime.History.Follower
  (ContractChanges(..), SomeContractChanges(..), applyRollback, isEmptyChanges)
import Language.Marlowe.Runtime.History.FollowerSupervisor (UpdateContract(..))
import Language.Marlowe.Runtime.History.Script
  ( HistoryScript(..)
  , HistoryScriptBlockState(..)
  , HistoryScriptContractState(..)
  , HistoryScriptEvent(..)
  , HistoryScriptState
  , reduceScriptEvent
  )
import Language.Marlowe.Runtime.History.Store
import Language.Marlowe.Runtime.History.Store.Memory (mkHistoryQueriesInMemory)
import Language.Marlowe.Runtime.History.Store.Model (getRoots)
import qualified Language.Marlowe.Runtime.History.Store.Model as Model
import Language.Marlowe.Runtime.History.Store.ModelSpec
  (genFindCreateStepArgs, genFindIntersectionArgs, genFindNextStepArgs, modelFromScript)
import Test.Hspec (Spec)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, Testable, discard, (===))
import Test.QuickCheck.Monadic (PropertyM, monadicIO, pick, run)

spec :: Spec
spec = do
  prop "findContract matches model" $ runStoreProp \HistoryStore{..} model -> case getRoots model of
    [] -> pure discard
    _ -> do
      contractId <- pick $ genFindCreateStepArgs model
      actual <- run $ findContract contractId
      pure $ actual === Model.findCreateStep contractId model
  prop "findNextSteps matches model" $ runStoreProp \HistoryStore{..} model -> case getRoots model of
    [] -> pure discard
    _ -> do
      (contractId, SomeMarloweVersion version, point) <- pick $ genFindNextStepArgs model
      actual <- run $ toFindNextStepsResponse version <$> getNextSteps contractId version point
      pure $ actual === Model.findNextSteps contractId point model
  prop "getIntersectionPoints matches model" $ runStoreProp \HistoryStore{..} model -> case getRoots model of
    [] -> pure discard
    _ -> do
      (contractId, SomeMarloweVersion version, blocks) <- pick $ genFindIntersectionArgs model
      actual <- run $ fmap (Intersection version) <$> intersectContract contractId version blocks
      pure $ actual === Model.findIntersection contractId blocks model

toFindNextStepsResponse :: MarloweVersion v -> GetNextStepsResponse v -> FindNextStepsResponse
toFindNextStepsResponse version = \case
  Rollback p -> FindRollback p
  Wait b _   -> FindWait b
  Next b s   -> FindNext b $ SomeContractSteps version s

runStoreProp
  :: Testable a
  => (HistoryStore -> Model.HistoryStoreModel -> PropertyM IO a)
  -> HistoryScript
  -> Property
runStoreProp storeProp script = monadicIO do
  (store@HistoryStore{..}, seedStore) <- run $ atomically setupStore
  exVar <- run newEmptyTMVarIO
  threadId <- run $ forkFinally runHistoryStore \case
    Left ex -> atomically $ putTMVar exVar ex
    _       -> pure ()
  run $ seedStore script
  let model = modelFromScript script
  result <- storeProp store model
  mEx <- run $ atomically $ tryTakeTMVar exVar
  run $ killThread threadId
  case mEx of
    Nothing -> pure result
    Just ex -> run $ throwIO ex

setupStore :: STM (HistoryStore, HistoryScript -> IO ())
setupStore = do
  changesVar <- newTVar mempty
  stateVar <- newTVar []
  historyQueries <- hoistHistoryQueries atomically <$> mkHistoryQueriesInMemory
  let
    changes = do
      newChanges <- readTVar changesVar
      writeTVar changesVar $ flip Map.mapMaybe newChanges \case
        RemoveContract                                 -> Nothing
        UpdateContract (SomeContractChanges version _) -> Just $ UpdateContract $ SomeContractChanges version mempty
      pure $ Map.filter notEmptyUpdate newChanges
    notEmptyUpdate = \case
      RemoveContract    -> False
      UpdateContract ch -> not $ isEmptyChanges ch
  store <- mkHistoryStore HistoryStoreDependencies{..}
  let
    seedStore (HistoryScript script) = do
      atomically $ traverse_ (\event -> withSome event $ runScriptEvent stateVar changesVar) script
      atomically $ void $ mfilter (all isEmptyUpdate) $ readTVar changesVar
  pure (store, seedStore)

isEmptyUpdate :: UpdateContract -> Bool
isEmptyUpdate = \case
  RemoveContract         -> False
  UpdateContract changes -> isEmptyChanges changes

runScriptEvent :: TVar HistoryScriptState -> TVar (Map ContractId UpdateContract) -> HistoryScriptEvent v -> STM ()
runScriptEvent stateVar changesVar event = do
  state <- readTVar stateVar
  modifyTVar stateVar (fromRight (error "failed to reduce generated script") . reduceScriptEvent event)
  modifyTVar changesVar case event of
    RollForward _ _ -> id
    RollBackward n ->
      let
        rollbackPoint = case drop (fromIntegral n) state of
          []                              -> Genesis
          HistoryScriptBlockState{..} : _ -> At block
      in
        fmap \case
          RemoveContract -> RemoveContract
          UpdateContract (SomeContractChanges version changes) ->
            UpdateContract $ SomeContractChanges version $ applyRollback rollbackPoint changes
    CreateContract contractId version create -> case state of
      [] -> error "create at genesis"
      HistoryScriptBlockState{..} : _ ->
        Map.insert contractId $ UpdateContract $ SomeContractChanges version $ ContractChanges
          { steps = mempty
          , create = Just (block, create)
          , rollbackTo = Nothing
          }
    ApplyInputs version transaction@Transaction{..} -> case state of
      [] -> error "create at genesis"
      HistoryScriptBlockState{block, contractStates} : _ ->
        case Map.lookup contractId contractStates of
          Nothing -> error "contract not found"
          Just (Some HistoryScriptContractState{contractVersion}) ->
            flip Map.update contractId $ Just . \case
              RemoveContract -> error "applied inputs after remove"
              UpdateContract (SomeContractChanges version' changes) -> case assertVersionsEqual version version' of
                Refl -> case assertVersionsEqual contractVersion version of
                  Refl -> UpdateContract $ SomeContractChanges version' $ changes <> ContractChanges
                    { steps = Map.singleton block [ApplyTransaction transaction]
                    , create = Nothing
                    , rollbackTo = Nothing
                    }

    Withdraw contractId version step -> case state of
      [] -> error "create at genesis"
      HistoryScriptBlockState{..} : _ ->
        flip Map.update contractId $ Just . \case
          RemoveContract -> error "applied inputs after remove"
          UpdateContract (SomeContractChanges version' changes) -> case assertVersionsEqual version version' of
            Refl ->
              let
                newChanges = ContractChanges
                  { steps = Map.singleton block [RedeemPayout step]
                  , create = Nothing
                  , rollbackTo = Nothing
                  }
              in
                UpdateContract $ SomeContractChanges version' $ changes <> newChanges
