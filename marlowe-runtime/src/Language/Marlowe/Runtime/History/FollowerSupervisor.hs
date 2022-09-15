{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.History.FollowerSupervisor
  where

import Control.Concurrent.Async (Concurrently(Concurrently, runConcurrently))
import Control.Concurrent.STM (STM, atomically, modifyTVar, newTVar, readTVar, writeTVar)
import Control.Monad (guard, mfilter, when, (<=<))
import Data.Foldable (sequenceA_)
import Data.Functor (void)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Language.Marlowe.Runtime.ChainSync.Api (RuntimeChainSeekClient, ScriptHash, SlotConfig)
import Language.Marlowe.Runtime.Core.AddressRegistry (MarloweScriptAddresses)
import Language.Marlowe.Runtime.Core.Api (ContractId, SomeMarloweVersion)
import Language.Marlowe.Runtime.History.Api (FollowerStatus(..))
import Language.Marlowe.Runtime.History.Follower
  (Follower(..), FollowerDependencies(..), SomeContractChanges, mkFollower)
import qualified Language.Marlowe.Runtime.History.Follower as Follower
import Witherable (Witherable(wither))

data FollowerActivation
  = Deactivate
  | Activate

data FollowerSupervisorDependencies = FollowerSupervisorDependencies
  { getMarloweVersion  :: ScriptHash -> Maybe (SomeMarloweVersion, MarloweScriptAddresses)
  , connectToChainSeek :: forall a. RuntimeChainSeekClient IO a -> IO a
  , slotConfig         :: SlotConfig
  , securityParameter  :: Int
  }

data UpdateContract
  = RemoveContract
  | UpdateContract SomeContractChanges
  deriving (Show, Eq)

data FollowerSupervisor = FollowerSupervisor
  { followContract        :: ContractId -> STM Bool
  , stopFollowingContract :: ContractId -> STM Bool
  , followerStatuses      :: STM (Map ContractId FollowerStatus)
  , changes               :: STM (Map ContractId UpdateContract)
  , runFollowerSupervisor :: IO ()
  }

mkFollowerSupervisor :: FollowerSupervisorDependencies -> STM FollowerSupervisor
mkFollowerSupervisor FollowerSupervisorDependencies{..} = do
  followersVar <- newTVar Map.empty
  followerActivationsVar <- newTVar Map.empty
  seenVar <- newTVar Set.empty
  removalsVar <- newTVar Set.empty

  let
    followContract contractId = do
      followers <- readTVar followersVar
      followerActivations <- readTVar followerActivationsVar
      mStatus <- traverse (status <=< readTVar) $ Map.lookup contractId followers
      let
        doActivate = do
          modifyTVar followerActivationsVar $ Map.insert contractId Activate
          pure True
        cancelDeactivate = do
          modifyTVar followerActivationsVar $ Map.delete contractId
          pure True
      let activation = Map.lookup contractId followerActivations
      case (mStatus, activation) of
        -- Follower has never run and there is a pending activation
        (Nothing, Just Activate)         -> pure False
        -- Follower has never run and there is no pending activation
        (Nothing, _)                     -> doActivate
        -- Follower failed previously and there is a pending activation
        (Just (Failed _), Just Activate) -> pure False
        -- Follower failed previously and there is no pending activation
        (Just (Failed _), _)             -> doActivate
        -- Follower is running and there is a pending deactivation
        (_, Just Deactivate)             -> cancelDeactivate
        -- Follower is running and there is no pending deactivation
        (_, _)                           -> pure False

    stopFollowingContract contractId = do
      followers <- readTVar followersVar
      followerActivations <- readTVar followerActivationsVar
      mStatus <- traverse (status <=< readTVar) $ Map.lookup contractId followers
      let
        doDeactivate = do
          modifyTVar followerActivationsVar $ Map.insert contractId Deactivate
          pure True
        cancelActivate = do
          modifyTVar followerActivationsVar $ Map.delete contractId
          pure True
      let activation = Map.lookup contractId followerActivations
      case (mStatus, activation) of
        -- Follower has never run and there is a pending activation
        (Nothing, Just Activate)         -> cancelActivate
        -- Follower has never run and there is no pending activation
        (Nothing, _)                     -> pure False
        -- Follower failed previously and there is a pending activation
        (Just (Failed _), Just Activate) -> cancelActivate
        -- Follower failed previously and there is no pending activation
        (Just (Failed _), _)             -> pure False
        -- Follower is running and there is a pending deactivation
        (_, Just Deactivate)             -> pure False
        -- Follower is running and there is no pending deactivation
        (_, _)                           -> doDeactivate

    followerStatuses = traverse (status <=< readTVar) =<< readTVar followersVar

    changes = do
      followers <- readTVar followersVar
      updatesFromFollowers <- wither
        (fmap (mfilter $ not . Follower.isEmptyChanges) . Follower.changes <=< readTVar)
        followers
      modifyTVar seenVar $ Set.union $ Map.keysSet updatesFromFollowers
      removals <- readTVar removalsVar
      writeTVar removalsVar Set.empty
      pure
        $ Map.union (UpdateContract <$> updatesFromFollowers)
        $ Map.fromSet (const RemoveContract) removals

    awaitActivations = do
      activations <- readTVar followerActivationsVar
      guard $ not $ Map.null activations
      writeTVar followerActivationsVar Map.empty
      pure $ Map.toList activations

    runFollowerSupervisor = do
      newFollowers <- atomically do
        activations <- awaitActivations
        flip wither activations \(contractId, activation) -> do
          followers <- readTVar followersVar
          case (activation, Map.lookup contractId followers) of
            (Activate, Nothing) -> do
              follower <- mkFollower FollowerDependencies{..}
              followerVar <- newTVar follower
              writeTVar followersVar $ Map.insert contractId followerVar followers
              pure $ Just follower
            (Activate, Just followerVar) -> do
              follower <- mkFollower FollowerDependencies{..}
              writeTVar followerVar follower
              pure $ Just follower
            (Deactivate, Nothing) -> pure Nothing
            (Deactivate, Just followerVar) -> do
              writeTVar followersVar $ Map.delete contractId followers
              Follower{cancelFollower} <- readTVar followerVar
              cancelFollower
              seen <- readTVar seenVar
              when (Set.member contractId seen) do
                modifyTVar removalsVar $ Set.insert contractId
              pure Nothing
      runConcurrently
        $ sequenceA_
        $ Concurrently <$> (runFollowerSupervisor : (void . runFollower <$> newFollowers))

  pure FollowerSupervisor {..}
