{-# LANGUAGE GADTs #-}

module Data.Marlowe.LiveContracts (
  LiveContracts,
  create,
  rollForward,
  rollBackward,
  collectGarbage,
) where

import Control.Category ((>>>))
import Data.Foldable (Foldable (..))
import Data.Functor ((<&>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Language.Marlowe.Core.V1.Plate (Extract (..))
import Language.Marlowe.Core.V1.Semantics (MarloweData (..))
import Language.Marlowe.Core.V1.Semantics.Types (Case (..), Contract)
import Language.Marlowe.Runtime.ChainSync.Api (
  BlockHeader (..),
  BlockNo,
  ChainPoint,
  DatumHash (..),
  TxOutRef (..),
  WithGenesis (..),
 )
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
import PlutusLedgerApi.V1 (fromBuiltin)

-- | Records a history of live contracts over a window of history.
data LiveContracts = LiveContracts
  { securityParameter :: Int
  , history :: Map BlockNo (Map TxOutRef Contract)
  }

-- | Create a new live contracts collection.
create
  :: Int
  -- ^ The protocol security parameter (the number of blocks before a rollback is guaranteed not to happen).
  -> LiveContracts
create securityParameter =
  LiveContracts
    { securityParameter
    , history = mempty
    }

-- | Add a new marlowe block to a live contracts collection.
rollForward
  :: BlockNo
  -- ^ The current block number of the chain tip.
  -> MarloweBlock
  -- ^ The block to add.
  -> LiveContracts
  -> LiveContracts
rollForward tipBlockNo MarloweBlock{..} LiveContracts{..} =
  LiveContracts
    { history = case Map.maxViewWithKey history of
        Nothing -> Map.singleton blockNo $ updateLiveContracts mempty
        Just ((prevBlockNo, prevLiveContracts), _)
          | blockNo <= prevBlockNo -> error "rollForward: non-increasing block number"
          | otherwise -> Map.insert blockNo (updateLiveContracts prevLiveContracts) historyTrimmed
    , ..
    }
  where
    BlockHeader{..} = blockHeader

    updateLiveContracts = consumeUpdatedContracts . addNewContracts

    addNewContracts = Map.union $ getNewContracts createTransactions

    consumeUpdatedContracts = flip (foldl' consumeUpdatedContract) applyInputsTransactions

    getNewContracts :: [MarloweCreateTransaction] -> Map TxOutRef Contract
    getNewContracts = foldMap \MarloweCreateTransaction{..} ->
      Map.mapKeysMonotonic (TxOutRef txId) $
        newContracts <&> \case
          SomeCreateStep MarloweV1 CreateStep{createOutput = TransactionScriptOutput{datum = MarloweData{..}}} ->
            marloweContract

    consumeUpdatedContract :: Map TxOutRef Contract -> MarloweApplyInputsTransaction -> Map TxOutRef Contract
    consumeUpdatedContract
      liveContracts
      (MarloweApplyInputsTransaction MarloweV1 UnspentContractOutput{..} Transaction{blockHeader = _, ..}) =
        case scriptOutput output of
          Nothing -> Map.delete txOutRef liveContracts
          Just TransactionScriptOutput{..} ->
            Map.insert utxo (marloweContract datum) $
              Map.delete txOutRef liveContracts

    historyTrimmed = go history
      where
        go =
          Map.minViewWithKey >>> \case
            -- Empty history - stop trimming
            Nothing -> mempty
            Just ((k, a), h)
              -- History entry is before security window - keep trimming
              | fromIntegral k < (fromIntegral tipBlockNo - securityParameter) -> go h
              -- History entry is within security window - stop trimming
              | otherwise -> Map.insert k a h

-- | Revert a live contracts collection to a previous point.
rollBackward
  :: ChainPoint
  -- ^ The point to which to roll back.
  -> LiveContracts
  -> LiveContracts
rollBackward Genesis liveContracts = liveContracts{history = mempty}
rollBackward (At BlockHeader{..}) LiveContracts{..}
  | Map.null history = LiveContracts{..}
  | otherwise = LiveContracts{history = rollbackHistory history, ..}
  where
    rollbackHistory =
      Map.maxViewWithKey >>> \case
        Nothing -> error "rollBackward: rollback point is beyond security window"
        Just ((latestBlockNo, liveContracts), history')
          | latestBlockNo > blockNo -> rollbackHistory history'
          | otherwise -> Map.insert latestBlockNo liveContracts history'

-- | Get all store hashes which are garbage.
collectGarbage
  :: (Monad m)
  => Set DatumHash
  -- ^ The set of all hashes in the store.
  -> (DatumHash -> m (Set DatumHash))
  -- ^ A function to get the closure of contract hashes for a contract hash.
  -> LiveContracts
  -- ^ The history of live contracts.
  -> m (Set DatumHash)
collectGarbage storeHashes getClosure LiveContracts{..} = do
  -- Collect the garbage collection roots of the live contracts.
  let getCaseRoots = \case
        MerkleizedCase _ hash -> Set.singleton $ DatumHash $ fromBuiltin hash
        _ -> mempty
      getContractRoots = foldMap getCaseRoots . extractAll @(Case Contract)
      -- Only consider roots also found in the store to avoid wasteful file system access
      liveRoots = Set.intersection storeHashes $ foldMap (foldMap getContractRoots) history

      foldMapM f = foldl' (\mb a -> (<>) <$> mb <*> f a) $ pure mempty
  -- Get all live store hashes by taking the union of the closures of the live roots.
  liveHashes <- foldMapM getClosure liveRoots
  -- All store hashes that are not alive are garbage.
  pure $ Set.difference storeHashes liveHashes
