{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}
module ChainSync.Store where

import ChainSync.Client (ChainSyncMsg (..), ChainSyncQuery (..), SendPortWithRollback (..), TxWithBlockHeader (..),
                         onRollback)
import Control.Arrow ((&&&))
import Control.Distributed.Process (Closure, Process, SendPort, match, matchChan, newChan, receiveWait, sendChan)
import Control.Distributed.Process.Closure (mkClosure, remotable)
import Control.Distributed.Process.Serializable (Serializable)
import Control.Monad (foldM)
import Data.Binary (Binary)
import Data.Data (Typeable)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word32)
import GHC.Generics (Generic)
import Language.Marlowe.Runtime.Chain.Types (MarloweBlockHeader (..), MarloweBlockNo (..), MarloweChainEvent (..),
                                             MarloweChainPoint (..), MarloweSlotNo (..), MarloweTx (..),
                                             MarloweTxIn (..), MarloweTxOut (..), TxOutRef (..))

data ChainSyncStoreConfig = ChainSyncStoreConfig
  { directory :: FilePath
  , batchSize :: Word32
  }
  deriving stock (Generic, Typeable, Show, Eq)
  deriving anyclass Binary

sendResponse :: Serializable a => SendPortWithRollback a -> a -> Process ()
sendResponse SendPortWithRollback{..} = sendChan onResponse

sendRollback :: SendPortWithRollback a -> MarloweChainPoint -> Process ()
sendRollback SendPortWithRollback{..} = sendChan onRollback

data ChainSyncStoreDependencies = ChainSyncStoreDependencies
  { config        :: ChainSyncStoreConfig
  , initQueryChan :: SendPort (SendPort ChainSyncQuery)
  }
  deriving (Generic, Typeable, Show, Eq)
  deriving anyclass Binary

data ChainSyncStoreState = ChainSyncStoreState
  { blockIndexBySlotNo     :: IntMap (MarloweBlockHeader, [MarloweTx])
  , blockIndexByBlockNo    :: IntMap (MarloweBlockHeader, [MarloweTx])
  , consumerIndex          :: Map TxOutRef TxWithBlockHeader
  , pendingConsumerQueries :: Map TxOutRef (Set (SendPortWithRollback TxWithBlockHeader))
  , pendingBlockQueries    :: IntMap (Set (SendPortWithRollback MarloweBlockHeader))
  }

chainSyncStore :: ChainSyncStoreDependencies -> Process ()
chainSyncStore ChainSyncStoreDependencies{..} = do
  (sendQuery, receiveQuery) <- newChan
  let
    txInToTxOutRef (MarloweTxIn txId txIx _) = TxOutRef txId txIx
    getPointNo MarloweChainPointAtGenesis = Nothing
    getPointNo (MarloweChainPoint slot _) = Just slot

    go state = traverse_  go =<< receiveWait
      [ match $ handleMsg state
      , fmap Just . matchChan receiveQuery $ handleQuery state
      ]

    handleMsg ChainSyncStoreState{..} (ChainSyncEvent (MarloweRollBackward point _)) = do
      let
        pointNo = getPointNo point
        afterRollbackPoint (MarloweBlockHeader slotNo _ _, _) = Just slotNo > pointNo
        (beforeRollbackBySlot, afterRollback) = IntMap.partition afterRollbackPoint blockIndexBySlotNo
        (beforeRollbackByBlock, _) = IntMap.partition afterRollbackPoint blockIndexByBlockNo
        (txOutsConsumedAfterRollback, txOutsProducedAfterRollback) =
          (fmap fst &&& fmap snd) do
            (_, (_, txns)) <- IntMap.toList afterRollback
            MarloweTx{..} <- txns
            pure (txInToTxOutRef <$> marloweTx_inputs, marloweTxOut_txOutRef <$> marloweTx_outputs)
        foldConsumer pending txOutRef = case Map.lookup txOutRef pending of
          Nothing -> pure pending
          Just recipients -> do
            traverse_ (flip sendRollback point) recipients
            pure $ Map.delete txOutRef pending
        foldBlock pending (MarloweBlockHeader _ _ (MarloweBlockNo blockNo), _) = do
          let blockNoInt = fromIntegral blockNo
          case IntMap.lookup blockNoInt pending of
            Nothing -> pure pending
            Just recipients -> do
              traverse_ (flip sendRollback point) recipients
              pure $ IntMap.delete blockNoInt pending
      newPendingConsumerQueries <- foldM foldConsumer pendingConsumerQueries $ concat txOutsProducedAfterRollback
      newPendingBlockQueries <- foldM foldBlock pendingBlockQueries afterRollback
      pure $ Just ChainSyncStoreState
        { blockIndexBySlotNo = beforeRollbackBySlot
        , blockIndexByBlockNo = beforeRollbackByBlock
        , consumerIndex = Map.withoutKeys consumerIndex $ Set.fromList $ concat txOutsConsumedAfterRollback
        , pendingConsumerQueries = newPendingConsumerQueries
        , pendingBlockQueries = newPendingBlockQueries
        }
    handleMsg ChainSyncStoreState{..} (ChainSyncEvent (MarloweRollForward header newTxs _)) = do
      let
        MarloweBlockHeader (MarloweSlotNo slotNo) _ (MarloweBlockNo blockNo) = header
        newConsumers = do
          tx@MarloweTx{..} <- newTxs
          input <- marloweTx_inputs
          pure (txInToTxOutRef input, TxWithBlockHeader header tx)
        foldConsumer requests (consumed, consumer) = case Map.lookup consumed requests of
          Nothing -> pure requests
          Just recipients -> do
            traverse_ (flip sendResponse consumer) recipients
            pure $ Map.delete consumed requests
      newPendingConsumerQueries <- foldM foldConsumer pendingConsumerQueries newConsumers
      pure $ Just ChainSyncStoreState
        { blockIndexBySlotNo = IntMap.insert (fromIntegral slotNo) (header, newTxs) blockIndexBySlotNo
        , blockIndexByBlockNo = IntMap.insert (fromIntegral blockNo) (header, newTxs) blockIndexByBlockNo
        , consumerIndex = Map.union consumerIndex $ Map.fromList newConsumers
        , pendingConsumerQueries = newPendingConsumerQueries
        , pendingBlockQueries = IntMap.delete (fromIntegral blockNo) pendingBlockQueries
        }
    handleMsg _ ChainSyncDone = pure Nothing
    handleMsg state (ChainSyncStart _ _) = pure $ Just state

    handleQuery state@ChainSyncStoreState{..} = \case
      QueryTxThatConsumes txOut replyTo ->
        -- have we already seen this txOut get consumed?
        case Map.lookup txOut consumerIndex of
          -- If so, reply straight away
          Just tx -> sendResponse replyTo tx $> state
          -- Otherwise remember the request for later
          Nothing -> pure state
            { pendingConsumerQueries = Map.insertWith
                Set.union
                txOut
                (Set.singleton replyTo)
                pendingConsumerQueries
            }

      QueryBlockNo (MarloweBlockNo blockNo) replyTo -> do
        let blockInt = fromIntegral blockNo
        -- do we already have this block?
        case IntMap.lookup blockInt blockIndexByBlockNo of
          -- If so, reply straight away
          Just (header, _) -> sendResponse replyTo header $> state
          -- If not, remember the request for later
          Nothing          -> pure state
            { pendingBlockQueries = IntMap.insertWith
                Set.union
                blockInt
                (Set.singleton replyTo)
                pendingBlockQueries
            }

      QueryIntersectionPoints replyTo -> do
        let mMaxItem = fst <$> IntMap.maxView blockIndexBySlotNo
        let toPoint (MarloweBlockHeader slot hash _, _) = MarloweChainPoint slot hash
        let intersectionPoints = maybeToList $ toPoint <$> mMaxItem
        sendChan replyTo intersectionPoints
        pure state

  sendChan initQueryChan sendQuery
  go ChainSyncStoreState
    { blockIndexBySlotNo = mempty
    , blockIndexByBlockNo = mempty
    , consumerIndex = mempty
    , pendingConsumerQueries = mempty
    , pendingBlockQueries = mempty
    }

remotable ['chainSyncStore]

process :: ChainSyncStoreDependencies -> Closure (Process ())
process = $(mkClosure 'chainSyncStore)
