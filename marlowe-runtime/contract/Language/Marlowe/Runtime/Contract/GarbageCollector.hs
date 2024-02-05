{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.Contract.GarbageCollector (
  GarbageCollectorDependencies (..),
  garbageCollector,
) where

import Colog (Message, WithLog)
import Control.Concurrent.Component (Component, component_)
import Control.DeepSeq (force)
import Control.Monad (unless)
import Data.Foldable (fold, for_)
import Data.HashTable.IO (CuckooHashTable, new)
import qualified Data.HashTable.IO as HashMapIO
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Set (Set)
import qualified Data.Set as Set
import Language.Marlowe.Core.V1.Plate (Extract (..))
import Language.Marlowe.Core.V1.Semantics (MarloweData (..))
import Language.Marlowe.Core.V1.Semantics.Types (Case (..), Contract)
import Language.Marlowe.Protocol.BulkSync.Client
import Language.Marlowe.Runtime.ChainSync.Api (
  BlockHeader (..),
  ChainSyncQuery (..),
  DatumHash (..),
  TxOutRef (..),
  WithGenesis (..),
 )
import Language.Marlowe.Runtime.Contract.Store (ContractStore (..))
import Language.Marlowe.Runtime.Core.Api (
  MarloweVersion (..),
  Transaction (..),
  TransactionOutput (..),
  TransactionScriptOutput (..),
 )
import Language.Marlowe.Runtime.History.Api (
  CreateStep (..),
  MarloweApplyInputsTransaction (..),
  MarloweBlock (..),
  MarloweCreateTransaction (..),
  SomeCreateStep (..),
  UnspentContractOutput (..),
 )
import Network.Protocol.Connection (Connector, runConnector)
import Network.Protocol.Query.Client (QueryClient, request)
import PlutusLedgerApi.V1 (fromBuiltin)
import UnliftIO (IORef, MonadUnliftIO, atomicModifyIORef, liftIO, modifyIORef, newIORef, readIORef)
import UnliftIO.Concurrent (threadDelay)

data GarbageCollectorDependencies m = GarbageCollectorDependencies
  { contractStore :: ContractStore m
  , marloweBulkSyncConnector :: Connector MarloweBulkSyncClient m
  , chainSyncQueryConnector :: Connector (QueryClient ChainSyncQuery) m
  }

garbageCollector
  :: forall env m. (WithLog env Message m, MonadUnliftIO m) => Component m (GarbageCollectorDependencies m) ()
garbageCollector = component_ "garbage-collector" \deps -> do
  -- A buffer containing the currently live continuation hashes indexed by the output that produced them
  continuationBuffer <- liftIO new
  -- A buffer that records when an output was produced
  activationBuffer <- newIORef mempty
  -- A buffer that records when an output was consumed
  deactivationBuffer <- newIORef mempty
  securityParameter <- runConnector (chainSyncQueryConnector deps) $ request GetSecurityParameter
  run securityParameter continuationBuffer activationBuffer deactivationBuffer deps
  where
    run
      :: Int
      -> CuckooHashTable TxOutRef (Set DatumHash)
      -> IORef (IntMap (Set TxOutRef))
      -> IORef (IntMap (Set TxOutRef))
      -> GarbageCollectorDependencies m
      -> m a
    run securityParameter continuationBuffer activationBuffer deactivationBuffer GarbageCollectorDependencies{..} =
      runConnector marloweBulkSyncConnector $ MarloweBulkSyncClient idle
      where
        idle = pure . SendMsgRequestNext 255 $ next False

        next isPolling =
          ClientStNext
            { recvMsgRollForward = rollForward
            , recvMsgRollBackward = rollBackward
            , recvMsgWait = wait isPolling
            }

        rollForward blocks tip = do
          liftIO do
            for_ blocks \MarloweBlock{..} -> do
              creationOutputs <- processCreations createTransactions
              (applyOutputs, applyInputs) <- processInputApplications applyInputsTransactions
              let BlockHeader{..} = blockHeader
              let outputs = creationOutputs <> applyOutputs
              let height = fromIntegral blockNo

              unless (Set.null outputs) do
                modifyIORef activationBuffer (<> IntMap.singleton height outputs)

              activated <- fold <$> readIORef activationBuffer
              let inputs = Set.intersection applyInputs activated

              unless (Set.null inputs) do
                modifyIORef deactivationBuffer (<> IntMap.singleton height inputs)
            cullBuffers tip
          idle

        rollBackward point tip = do
          liftIO do
            rolledBack <- atomicModifyIORef activationBuffer \m -> case point of
              Genesis -> (mempty, fold m)
              At BlockHeader{..} -> case IntMap.splitLookup (fromIntegral blockNo) m of
                (toKeep, toKeep', rolledBack) ->
                  (maybe id (IntMap.insert $ fromIntegral blockNo) toKeep' toKeep, fold rolledBack)

            modifyIORef deactivationBuffer \m -> case point of
              Genesis -> mempty
              At BlockHeader{..} -> case IntMap.splitLookup (fromIntegral blockNo) m of
                (toKeep, toKeep', _) -> maybe id (IntMap.insert $ fromIntegral blockNo) toKeep' toKeep

            for_ rolledBack $ HashMapIO.delete continuationBuffer

            case tip of
              Genesis -> pure ()
              At tip' -> cullBuffers tip'
          idle

        wait isPolling = do
          unless isPolling do
            liveRoots <- liftIO $ foldMap snd <$> HashMapIO.toList continuationBuffer
            setGCRoots contractStore liveRoots
          poll

        poll = do
          threadDelay 10_000_000 -- ten seconds
          pure $ SendMsgPoll $ next True

        processCreations :: [MarloweCreateTransaction] -> IO (Set TxOutRef)
        processCreations = foldMapM \MarloweCreateTransaction{..} -> foldMapM processCreation newContracts

        processCreation :: SomeCreateStep -> IO (Set TxOutRef)
        processCreation (SomeCreateStep v CreateStep{..}) = processActivation v createOutput

        processInputApplications :: [MarloweApplyInputsTransaction] -> IO (Set TxOutRef, Set TxOutRef)
        processInputApplications = foldMapM \MarloweApplyInputsTransaction{..} -> do
          activations <- foldMapM (processActivation marloweVersion) (scriptOutput $ output marloweTransaction)
          pure (activations, Set.singleton $ txOutRef marloweInput)

        processActivation :: MarloweVersion v -> TransactionScriptOutput v -> IO (Set TxOutRef)
        processActivation MarloweV1 TransactionScriptOutput{..} = do
          -- It's very important to force the roots here to prevent the full contract from staying in memory.
          let roots = force $ getContractRoots $ marloweContract datum
          if Set.null roots
            then pure mempty
            else do
              HashMapIO.insert continuationBuffer utxo roots
              pure $ Set.singleton utxo

        cullBuffers :: BlockHeader -> IO ()
        cullBuffers BlockHeader{..} = do
          -- The minimum block height of entries to keep (tip - security parameter). All older information will be
          -- culled.
          let minHeight = fromIntegral $ blockNo - fromIntegral securityParameter
          toCull <- atomicModifyIORef deactivationBuffer \m -> case IntMap.splitLookup minHeight m of
            (toCull, minSet, toKeep) -> (maybe id (IntMap.insert minHeight) minSet toKeep, fold toCull)
          unless (null toCull) do
            for_ toCull $ HashMapIO.delete continuationBuffer
            modifyIORef activationBuffer $ IntMap.mapMaybe $ nonEmptySet . (`Set.difference` toCull)

nonEmptySet :: Set a -> Maybe (Set a)
nonEmptySet s
  | Set.null s = Nothing
  | otherwise = Just s

foldMapM :: (Traversable t, Applicative f, Monoid b) => (a -> f b) -> t a -> f b
foldMapM f = fmap fold . traverse f

getContractRoots :: Contract -> Set DatumHash
getContractRoots = foldMap getCaseRoots . extractAll @(Case Contract)

getCaseRoots :: Case a -> Set DatumHash
getCaseRoots = \case
  MerkleizedCase _ hash -> Set.singleton $ DatumHash $ fromBuiltin hash
  _ -> mempty
