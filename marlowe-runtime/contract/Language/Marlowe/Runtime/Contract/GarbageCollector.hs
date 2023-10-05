{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Language.Marlowe.Runtime.Contract.GarbageCollector (
  GarbageCollectorDependencies (..),
  garbageCollector,
) where

import Colog (Message, WithLog)
import Control.Concurrent.Component (Component, component_)
import Control.Monad (when)
import Data.Foldable (Foldable (..))
import Data.Functor ((<&>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Language.Marlowe.Core.V1.Plate (Extract (extractAll))
import Language.Marlowe.Core.V1.Semantics (MarloweData (..))
import Language.Marlowe.Core.V1.Semantics.Types (Case (..), Contract)
import Language.Marlowe.Protocol.BulkSync.Client
import Language.Marlowe.Runtime.ChainSync.Api (
  BlockHeader (..),
  BlockNo,
  ChainPoint,
  ChainSyncQuery (..),
  DatumHash (DatumHash),
  TxOutRef (..),
  WithGenesis (..),
 )
import Language.Marlowe.Runtime.Contract.Api (ContractWithAdjacency (..))
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
import UnliftIO (MonadUnliftIO)
import UnliftIO.Concurrent (threadDelay)

data GarbageCollectorDependencies m = GarbageCollectorDependencies
  { contractStore :: ContractStore m
  , marloweBulkSyncConnector :: Connector MarloweBulkSyncClient m
  , chainSyncQueryConnector :: Connector (QueryClient ChainSyncQuery) m
  }

garbageCollector :: (WithLog env Message m, MonadUnliftIO m) => Component m (GarbageCollectorDependencies m) ()
garbageCollector = component_ "garbage-collector" run
  where
    run GarbageCollectorDependencies{..} =
      runConnector marloweBulkSyncConnector $ MarloweBulkSyncClient do
        securityParameter <- runConnector chainSyncQueryConnector $ request GetSecurityParameter
        pure $ idle $ LiveContracts securityParameter mempty
      where
        idle = SendMsgRequestNext 255 . next True

        next gcOnWait liveContracts =
          ClientStNext
            { recvMsgRollForward = rollForward liveContracts
            , recvMsgRollBackward = rollBackward liveContracts
            , recvMsgWait = wait gcOnWait liveContracts
            }

        rollForward liveContracts blocks _ =
          pure $ idle $ foldl' rollForwardLiveContracts liveContracts blocks

        rollBackward liveContracts point _ =
          pure $ idle $ rollbackLiveContracts point liveContracts

        wait gcOnWait liveContracts = do
          when gcOnWait $
            collectGarbage contractStore $
              (foldMap . foldMap) getContractRoots $
                liveContracts.liveContractsByBlockNo
          poll liveContracts

        poll liveContracts = do
          threadDelay 10_000_000 -- ten seconds
          pure $ SendMsgPoll $ next False liveContracts

collectGarbage :: (Monad m) => ContractStore m -> Set DatumHash -> m ()
collectGarbage store@ContractStore{..} liveRoots = do
  storeHashes <- getHashes
  liveHashes <- foldMapM (getClosure store) liveRoots
  let deadHashes = Set.difference storeHashes liveHashes
  deleteContracts deadHashes

getClosure :: (Functor m) => ContractStore m -> DatumHash -> m (Set DatumHash)
getClosure ContractStore{..} = fmap (foldMap closure) . getContract

getContractRoots :: Contract -> Set DatumHash
getContractRoots = foldMap getContinuations . extractAll @(Case Contract)
  where
    getContinuations = \case
      MerkleizedCase _ hash -> Set.singleton $ DatumHash $ fromBuiltin hash
      _ -> mempty

rollForwardLiveContracts :: LiveContracts -> MarloweBlock -> LiveContracts
rollForwardLiveContracts LiveContracts{..} MarloweBlock{..} =
  LiveContracts
    { liveContractsByBlockNo =
        Map.fromDistinctAscList
          . drop (Map.size liveContractsByBlockNo + 1 - securityParameter)
          . Map.toAscList
          $ insertModifyMax (blockNo blockHeader) (updateLiveContracts MarloweBlock{..}) liveContractsByBlockNo
    , ..
    }

updateLiveContracts :: MarloweBlock -> Map TxOutRef Contract -> Map TxOutRef Contract
updateLiveContracts MarloweBlock{..} =
  flip (foldl' consumeLiveContract) applyInputsTransactions
    . Map.union (foldMap newLiveContracts createTransactions)

newLiveContracts :: MarloweCreateTransaction -> Map TxOutRef Contract
newLiveContracts MarloweCreateTransaction{..} =
  Map.mapKeysMonotonic (TxOutRef txId) $
    newContracts <&> \case
      SomeCreateStep MarloweV1 CreateStep{createOutput = TransactionScriptOutput{datum = MarloweData{..}}} ->
        marloweContract

consumeLiveContract :: Map TxOutRef Contract -> MarloweApplyInputsTransaction -> Map TxOutRef Contract
consumeLiveContract
  liveContracts
  (MarloweApplyInputsTransaction MarloweV1 UnspentContractOutput{..} Transaction{..}) = case scriptOutput output of
    Nothing -> Map.delete txOutRef liveContracts
    Just TransactionScriptOutput{..} ->
      Map.insert utxo (marloweContract datum) $
        Map.delete txOutRef liveContracts

rollbackLiveContracts :: ChainPoint -> LiveContracts -> LiveContracts
rollbackLiveContracts point LiveContracts{..} =
  LiveContracts
    { liveContractsByBlockNo = case point of
        Genesis -> mempty
        At BlockHeader{..} ->
          Map.fromDistinctDescList
            . dropWhile ((> blockNo) . fst)
            . Map.toDescList
            $ liveContractsByBlockNo
    , ..
    }

data LiveContracts = LiveContracts
  { securityParameter :: Int
  , liveContractsByBlockNo :: Map BlockNo (Map TxOutRef Contract)
  }

insertModifyMax :: (Monoid a, Ord k) => k -> (a -> a) -> Map k a -> Map k a
insertModifyMax k f m = case Map.maxViewWithKey m of
  Nothing -> Map.singleton k $ f mempty
  Just ((k', a), m') -> Map.insert k (f a) $ Map.insert k' a m'

foldMapM :: (Foldable t, Applicative m, Monoid b) => (a -> m b) -> t a -> m b
foldMapM f = foldl' (\mb a -> (<>) <$> mb <*> f a) $ pure mempty
