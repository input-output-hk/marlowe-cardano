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
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeApplications          #-}
module ChainSync.Store where

import ChainSync.Client (ChainSyncMsg (..))
import ChainSync.Database (Block (..), ChainSyncQuery (..), TxWithBlockHeader (..), getConsumer, getTip, rollBackward,
                           rollBackwardToGenesis, rollForward)
import Control.Distributed.Process (Closure, Process, ProcessId, ProcessMonitorNotification (..), SendPort, getSelfPid,
                                    match, matchChan, monitor, newChan, receiveChan, receiveWait, sendChan)
import Control.Distributed.Process.Closure (mkClosure, remotable)
import Control.Distributed.Process.Internal.Types (ReceivePort)
import Control.Distributed.Process.Serializable (Serializable)
import Control.Monad (unless, (<=<))
import Data.Binary (Binary)
import Data.Data (Typeable)
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word64)
import GHC.Generics (Generic)
import Language.Marlowe.Runtime.Chain.Types (MarloweBlockHeader (..), MarloweBlockNo (..), MarloweChainEvent (..),
                                             MarloweChainPoint (..), MarloweChainTip (..), MarloweSlotNo (..),
                                             MarloweTx (..), MarloweTxIn (..), MarloweTxOut (..), TxOutRef (..))

sendSubscriber :: (Serializable e, Serializable a) => Set ProcessId -> Either e a -> Subscriber e a -> Process ()
sendSubscriber deadProcesses msg Subscriber{..} = unless (Set.member pid deadProcesses) $ sendChan replyChan msg

sendResponse :: (Serializable e, Serializable a) => Set ProcessId -> a -> Subscriber e a -> Process ()
sendResponse deadProcesses = sendSubscriber deadProcesses . Right

sendError :: (Serializable e, Serializable a) => Set ProcessId -> e -> Subscriber e a -> Process ()
sendError deadProcesses = sendSubscriber deadProcesses . Left

getConsumingTx
  :: SendPort ChainStoreQuery
  -> TxOutRef
  -> Process (ReceivePort (Either MarloweChainPoint TxWithBlockHeader))
getConsumingTx queryChan txOutRef = do
  (replyChan, receiveResponse) <- newChan
  pid <- getSelfPid
  sendChan queryChan $ GetConsumingTx txOutRef $ Subscriber pid replyChan
  pure receiveResponse

awaitSecurityParameter
  :: SendPort ChainStoreQuery
  -> MarloweBlockHeader
  -> Process (Either MarloweChainPoint ())
awaitSecurityParameter queryChan fromBlock = do
  (replyChan, receiveResponse) <- newChan
  pid <- getSelfPid
  sendChan queryChan $ AwaitSecurityParameter fromBlock $ Subscriber pid replyChan
  receiveChan receiveResponse

data ChainSyncStoreDependencies = ChainSyncStoreDependencies
  { dbChan        :: SendPort ChainSyncQuery
  , initStoreChan :: SendPort (SendPort ChainStoreQuery)
  }
  deriving (Generic, Typeable, Show, Eq)
  deriving anyclass Binary

data Subscriber e a = Subscriber
  { pid       :: ProcessId
  , replyChan :: SendPort (Either e a)
  }
  deriving (Generic, Typeable, Show, Eq)
  deriving anyclass Binary

type ConsumerSubscriber = Subscriber MarloweChainPoint TxWithBlockHeader
type SecurityParamSubscriber = Subscriber MarloweChainPoint ()

data ChainStoreQuery
  = GetConsumingTx TxOutRef ConsumerSubscriber
  | AwaitSecurityParameter MarloweBlockHeader SecurityParamSubscriber
  deriving (Generic, Typeable, Show, Eq)
  deriving anyclass Binary

data ChainSyncStoreState = ChainSyncStoreState
  { consumerSubscribers      :: Map TxOutRef (Map ProcessId ConsumerSubscriber)
  , securityParamSubscribers :: IntMap (Map ProcessId SecurityParamSubscriber)
  , deadSubscribers          :: Set ProcessId
  }

securityParameter :: Word64
securityParameter = 2160

chainSyncStore :: ChainSyncStoreDependencies -> Process ()
chainSyncStore ChainSyncStoreDependencies{..} = do
  (sendQuery, receiveQuery) <- newChan
  sendChan initStoreChan sendQuery
  let
    initialState = ChainSyncStoreState
      { consumerSubscribers = mempty
      , securityParamSubscribers = mempty
      , deadSubscribers = mempty
      }

    go state@ChainSyncStoreState{..} = receiveWait
      [ match \case
          ChainSyncDone -> pure ()
          ChainSyncEvent (MarloweRollForward header txs tip) -> do
            rollForward dbChan (Block header txs) tip
            go =<< rollForwardSubscribers state header txs
          ChainSyncEvent (MarloweRollBackward MarloweChainPointAtGenesis tip) -> do
            rollBackwardToGenesis dbChan tip
            go =<< rollbackAllSubscribers state
          ChainSyncEvent (MarloweRollBackward (MarloweChainPoint slot hash) tip) -> do
            rolledBackBlocks <- rollBackward dbChan slot hash tip
            go =<< rollbackSubscribers slot hash state rolledBackBlocks
          _ -> go state
      , matchChan receiveQuery $ go <=< \case
          GetConsumingTx ref subscriber               -> handleGetConsumingTx state ref subscriber
          AwaitSecurityParameter fromBlock subscriber -> handleAwaitSecurityParameter state fromBlock subscriber
      , match \(ProcessMonitorNotification _ pid _) -> go state { deadSubscribers = Set.insert pid deadSubscribers }
      ]

    handleAwaitSecurityParameter
      state@ChainSyncStoreState{..}
      (MarloweBlockHeader _ _ (MarloweBlockNo fromBlock))
      subscriber@Subscriber{..} = do
        tip <- getTip dbChan
        let
          tipBlockNo = case tip of
            MarloweChainTipAtGenesis                   -> 0
            MarloweChainTip _ _ (MarloweBlockNo block) -> block
        if tipBlockNo - fromBlock >= securityParameter then do
          sendResponse deadSubscribers () subscriber
          pure state
        else do
          void $ monitor pid
          pure state
            { securityParamSubscribers = IntMap.insertWith
                Map.union
                (fromIntegral fromBlock)
                (Map.singleton pid subscriber)
                securityParamSubscribers
            }

    handleGetConsumingTx state@ChainSyncStoreState{..} ref subscriber@Subscriber{..} =
      getConsumer dbChan ref >>= \case
        Just consumer -> state <$ sendResponse deadSubscribers consumer subscriber
        Nothing -> do
          void $ monitor pid
          pure state
            { consumerSubscribers = Map.insertWith
                Map.union
                ref
                (Map.singleton pid subscriber)
                consumerSubscribers
            }



    rollbackAllSubscribers ChainSyncStoreState{..} = do
      traverse_ (traverse_ (sendError deadSubscribers MarloweChainPointAtGenesis)) consumerSubscribers
      traverse_ (traverse_ (sendError deadSubscribers MarloweChainPointAtGenesis)) securityParamSubscribers
      pure initialState

    rollbackSubscribers rollbackToSlot hash state@ChainSyncStoreState{..} rolledBackBlocks = do
      let
        rolledBackTxOuts = Set.fromList
          $   fmap marloweTxOut_txOutRef
          $   marloweTx_outputs
          =<< txs
          =<< rolledBackBlocks
        isProducerRolledBack ref = Set.member ref rolledBackTxOuts
        (consumerSubscribersRolledBack, consumerSubscribers') =
          Map.partitionWithKey (const . isProducerRolledBack) consumerSubscribers
        isSlotAfterRollback slot = MarloweSlotNo (fromIntegral slot) > rollbackToSlot
        (securityParamSubscribersRolledBack, securityParamSubscribers') =
          IntMap.partitionWithKey (const . isSlotAfterRollback) securityParamSubscribers
        point = MarloweChainPoint rollbackToSlot hash
      traverse_ (traverse_ (sendError deadSubscribers point)) consumerSubscribersRolledBack
      traverse_ (traverse_ (sendError deadSubscribers point)) securityParamSubscribersRolledBack
      pure state
        { consumerSubscribers = consumerSubscribers'
        , securityParamSubscribers = securityParamSubscribers'
        }

    rollForwardSubscribers state@ChainSyncStoreState{..} header@(MarloweBlockHeader _ _ (MarloweBlockNo blockNo)) txs = do
      let
        txOutsConsumed = Map.fromList do
          tx@MarloweTx{..} <- txs
          MarloweTxIn txid txix _ <- marloweTx_inputs
          pure (TxOutRef txid txix, TxWithBlockHeader header tx)
        consumerSubscribersToNotify = Map.intersectionWith (\ref -> fmap (ref,)) txOutsConsumed consumerSubscribers
        consumerSubscribers' = Map.difference consumerSubscribers consumerSubscribersToNotify
        maxConfirmedBlock = fromIntegral $ blockNo - securityParameter
        keysToNotify = case IntMap.minViewWithKey securityParamSubscribers of
          Nothing -> mempty
          Just ((minKey, _), _)
            | minKey > maxConfirmedBlock -> mempty
            | otherwise -> case IntMap.maxViewWithKey securityParamSubscribers of
              Nothing               -> mempty
              Just ((maxKey, _), _) -> IntSet.fromDistinctAscList [minKey..min maxConfirmedBlock maxKey]
        securityParamSubscribers' = IntMap.withoutKeys securityParamSubscribers keysToNotify
        securityParamSubscribersToNotify = IntMap.difference securityParamSubscribers securityParamSubscribers'
      traverse_ (traverse_ (uncurry $ sendResponse deadSubscribers)) consumerSubscribersToNotify
      traverse_ (traverse_ (sendResponse deadSubscribers ())) securityParamSubscribersToNotify
      pure state
        { consumerSubscribers = consumerSubscribers'
        , securityParamSubscribers = securityParamSubscribers'
        }
  go initialState

remotable ['chainSyncStore]

process :: ChainSyncStoreDependencies -> Closure (Process ())
process = $(mkClosure 'chainSyncStore)
