{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE RankNTypes         #-}

module Language.Marlowe.Runtime.History.StoreSpec (spec) where

import Control.Concurrent (forkFinally, killThread)
import Control.Concurrent.STM (STM, TVar, atomically, modifyTVar, newEmptyTMVarIO, newTVar, putTMVar, readTVar,
                               tryTakeTMVar, writeTVar)
import Control.Exception.Base (throwIO)
import Control.Monad (mfilter)
import Data.Either (fromRight)
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Some (Some (..), withSome)
import Data.Type.Equality (type (:~:) (Refl))
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader (..), TxOutRef (TxOutRef), WithGenesis (..))
import Language.Marlowe.Runtime.Core.Api (ContractId (ContractId), Transaction (..), assertVersionsEqual)
import Language.Marlowe.Runtime.History.Api (ContractStep (..), SomeCreateStep (..))
import Language.Marlowe.Runtime.History.Follower (ContractChanges (..), SomeContractChanges (..), applyRollback,
                                                  isEmptyChanges)
import Language.Marlowe.Runtime.History.FollowerSupervisor (UpdateContract (..))
import Language.Marlowe.Runtime.History.Script (HistoryScript (..), HistoryScriptBlockState (..),
                                                HistoryScriptContractState (..), HistoryScriptEvent (..),
                                                HistoryScriptState, genCreateContract, genRollForward, genTxId,
                                                reduceScriptEvent)
import Language.Marlowe.Runtime.History.Store
import Language.Marlowe.Runtime.History.Store.Memory (mkHistoryQueriesInMemory)
import Test.Hspec (Spec)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, (===))
import Test.QuickCheck.Monadic (PropertyM, monadicIO, pick, run)

spec :: Spec
spec = do
  prop "Created contracts are found" $ runHistoryProp propCreatedContractsAreFound
  prop "Non created contracts are not found" $ runHistoryProp propNonCreatedContractsAreNotFound
  prop "Rolled back contracts are not found" $ runHistoryProp propRolledBackContractsAreNotFound
  prop "RollForward, RollBackward is a no-op" $ runHistoryProp propRollForwardRollBackward

propCreatedContractsAreFound :: HistoryProp
propCreatedContractsAreFound HistoryScriptBlockState{..} _ HistoryStore{..} runEvent = do
  Some event <- pick $ genCreateContract $ slotNo block
  run $ runEvent event
  let (contractId, create) = assertCreate event
  found <- run $ findContract contractId
  pure $ found === Just (block, create)

propNonCreatedContractsAreNotFound :: HistoryProp
propNonCreatedContractsAreNotFound _ _ HistoryStore{..} _ = do
  contractId <- ContractId . flip TxOutRef 0 <$> pick genTxId
  found <- run $ findContract contractId
  pure $ found === Nothing

propRolledBackContractsAreNotFound :: HistoryProp
propRolledBackContractsAreNotFound HistoryScriptBlockState{..} _ HistoryStore{..} runEvent = do
  Some event <- pick $ genCreateContract $ slotNo block
  let (contractId, _) = assertCreate event
  expected <- run $ findContract contractId
  run do
    runEvent event
    runEvent $ RollBackward 1
  found <- run $ findContract contractId
  pure $ found === expected

propRollForwardRollBackward :: HistoryProp
propRollForwardRollBackward HistoryScriptBlockState{..} _ HistoryStore{..} runEvent = do
  Some event <- pick $ genCreateContract $ slotNo block
  Some rollForward <- pick genRollForward
  run $ runEvent event
  let (contractId, _) = assertCreate event
  expected <- run $ findContract contractId
  run do
    runEvent rollForward
    runEvent $ RollBackward 1
  actual <- run $ findContract contractId
  pure $ actual === expected

assertCreate :: HistoryScriptEvent v -> (ContractId, SomeCreateStep)
assertCreate = \case
    CreateContract contractId version create -> (contractId, SomeCreateStep version create)
    _                                        -> error "failed to match irrefutable pattern"

type RunScriptEvent = forall v. HistoryScriptEvent v -> IO ()

type HistoryProp = HistoryScriptBlockState -> HistoryScriptState -> HistoryStore -> RunScriptEvent -> PropertyM IO Property

runHistoryProp :: HistoryProp -> HistoryScript -> Property
runHistoryProp test (HistoryScript script) = monadicIO do
  (store@HistoryStore{..}, changesVar, stateVar, state) <- run $ atomically do
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
    traverse_ (\event -> withSome event $ runScriptEvent stateVar changesVar) script
    (, changesVar, stateVar,) <$> mkHistoryStore HistoryStoreDependencies{..} <*> readTVar stateVar
  case state of
    []                  -> error "Generated empty script"
    blockState : state' -> do
      exVar <- run newEmptyTMVarIO
      threadId <- run $ forkFinally runHistoryStore \case
        Left ex -> atomically $ putTMVar exVar ex
        _       -> pure ()
      result <- test blockState state' store \event -> do
        -- write the changes
        atomically $ runScriptEvent stateVar changesVar event
        -- wait until they are picked up.
        let
          isEmptyUpdate = \case
            RemoveContract    -> False
            UpdateContract ch -> isEmptyChanges ch
        void $ atomically $ mfilter (all isEmptyUpdate) $ readTVar changesVar
      run $ killThread threadId
      mEx <- run $ atomically $ tryTakeTMVar exVar
      case mEx of
        Nothing -> pure result
        Just ex -> run $ throwIO ex

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
      HistoryScriptBlockState{..} : _ ->
        case Map.lookup contractId contractStates of
          Nothing -> error "contract not found"
          Just (Some HistoryScriptContractState{..}) ->
            flip Map.update contractId $ Just . \case
              RemoveContract -> error "applied inputs after remove"
              UpdateContract (SomeContractChanges version' changes) -> case assertVersionsEqual version version' of
                Refl -> case assertVersionsEqual contractVersion version of
                  Refl ->
                    let
                      step = ApplyTransaction transaction
                      newChanges = ContractChanges
                        { steps = Map.singleton block [step]
                        , create = Nothing
                        , rollbackTo = Nothing
                        }
                    in
                      UpdateContract $ SomeContractChanges version' $ changes <> newChanges
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
