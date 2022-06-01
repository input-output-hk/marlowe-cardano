{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeApplications          #-}
module ChainSync.Database where

import Control.Arrow ((&&&))
import Control.Distributed.Process (Closure, Process, SendPort, newChan, receiveChan, sendChan)
import Control.Distributed.Process.Closure (mkClosure, remotable)
import Data.Binary (Binary)
import Data.Data (Typeable)
import Data.Foldable (fold)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import GHC.Generics (Generic)
import Language.Marlowe.Runtime.Chain.Types (MarloweBlockHeader (..), MarloweBlockHeaderHash, MarloweChainPoint (..),
                                             MarloweChainTip (..), MarloweSlotNo (..), MarloweTx (..), MarloweTxId,
                                             MarloweTxIn (MarloweTxIn), TxOutRef (..))

newtype ChainSyncDatabaseDependencies = ChainSyncDatabaseDependencies
  { initStoreChan :: SendPort ChainSyncQueryChan
  }
  deriving (Generic, Typeable, Show, Eq)
  deriving anyclass Binary

data TxWithBlockHeader = TxWithBlockHeader
  { blockHeader :: MarloweBlockHeader
  , tx          :: MarloweTx
  }
  deriving (Generic, Typeable, Show, Eq)
  deriving anyclass Binary

data ChainSyncQuery
  = GetIntersectionPoints (SendPort [MarloweChainPoint])
  | GetTip (SendPort MarloweChainTip)
  | GetConsumer TxOutRef (SendPort (Maybe TxWithBlockHeader))
  | RollForward Block MarloweChainTip
  | RollBackwardToGenesis MarloweChainTip
  | RollBackward MarloweSlotNo MarloweBlockHeaderHash MarloweChainTip (SendPort [Block])
  deriving (Generic, Typeable, Show, Eq)
  deriving anyclass Binary

type ChainSyncQueryChan = SendPort ChainSyncQuery

data Block = Block
  { header :: MarloweBlockHeader
  , txs    :: [MarloweTx]
  }
  deriving (Generic, Typeable, Show, Eq)
  deriving anyclass Binary

data ChainSyncDatabaseState = ChainSyncDatabaseState
  { blocks             :: Map MarloweBlockHeaderHash Block
  , transactions       :: Map MarloweTxId TxWithBlockHeader
  , intersectionPoints :: [MarloweChainPoint]
  , tip                :: MarloweChainTip
  , consumerIndex      :: Map TxOutRef MarloweTxId
  , consumerSlotIndex  :: IntMap (Set TxOutRef)
  }

rollForward :: ChainSyncQueryChan -> Block -> MarloweChainTip -> Process ()
rollForward chan block = sendChan chan . RollForward block

rollBackwardToGenesis :: ChainSyncQueryChan -> MarloweChainTip -> Process ()
rollBackwardToGenesis chan = sendChan chan . RollBackwardToGenesis

rollBackward :: ChainSyncQueryChan -> MarloweSlotNo -> MarloweBlockHeaderHash -> MarloweChainTip -> Process [Block]
rollBackward chan slot hash tip = do
  (sendBlocks, receiveBlocks) <- newChan
  sendChan chan $ RollBackward slot hash tip sendBlocks
  receiveChan receiveBlocks

getIntersectionPoints :: ChainSyncQueryChan -> Process [MarloweChainPoint]
getIntersectionPoints sendQuery = do
  (sendPort, receiveResponse) <- newChan
  sendChan sendQuery $ GetIntersectionPoints sendPort
  receiveChan receiveResponse

getTip :: ChainSyncQueryChan -> Process MarloweChainTip
getTip sendQuery = do
  (sendPort, receiveResponse) <- newChan
  sendChan sendQuery $ GetTip sendPort
  receiveChan receiveResponse

getConsumer :: ChainSyncQueryChan -> TxOutRef -> Process (Maybe TxWithBlockHeader)
getConsumer sendQuery ref = do
  (sendPort, receiveResponse) <- newChan
  sendChan sendQuery $ GetConsumer ref sendPort
  receiveChan receiveResponse

chainSyncDatabase :: ChainSyncDatabaseDependencies -> Process ()
chainSyncDatabase ChainSyncDatabaseDependencies{..} = do
  (sendQuery, receiveQuery) <- newChan
  sendChan initStoreChan sendQuery
  let go state = go =<< handleQuery state =<< receiveChan receiveQuery
  go initialState
    where
      initialState = ChainSyncDatabaseState
        { blocks = mempty
        , transactions = mempty
        , intersectionPoints = mempty
        , tip = MarloweChainTipAtGenesis
        , consumerIndex = mempty
        , consumerSlotIndex = mempty
        }
      handleQuery state@ChainSyncDatabaseState{..} = \case
        GetIntersectionPoints sendResponse       -> state <$ sendChan sendResponse intersectionPoints
        RollForward block newTip                 -> handleRollForward newTip state block
        RollBackwardToGenesis newTip             -> pure initialState { tip = newTip }
        RollBackward slot hash newTip sendBlocks -> handleRollBackward newTip state sendBlocks slot hash
        GetTip sendTip                           -> state <$ sendChan sendTip tip
        GetConsumer txOut sendTx                 -> state <$ sendChan sendTx (lookupConsumer state txOut)

      lookupConsumer ChainSyncDatabaseState{..} txOut = do
        txId <- Map.lookup txOut consumerIndex
        Map.lookup txId transactions

      handleRollForward
        newTip
        ChainSyncDatabaseState{..}
        block@Block{header = header@(MarloweBlockHeader slot hash _), txs} = do
          let
            newConsumptions = Map.fromList do
              MarloweTx{..} <- txs
              MarloweTxIn txid txix _ <- marloweTx_inputs
              pure (TxOutRef txid txix, marloweTx_id)
            MarloweSlotNo slotInt = slot
          pure ChainSyncDatabaseState
            { blocks = Map.insert hash block blocks
            , transactions = Map.union transactions
                $ Map.fromList
                $ (marloweTx_id &&& TxWithBlockHeader header) <$> txs
            , intersectionPoints = [MarloweChainPoint slot hash]
            , tip = newTip
            , consumerIndex = Map.union consumerIndex newConsumptions
            , consumerSlotIndex = IntMap.insert (fromIntegral slotInt) (Map.keysSet newConsumptions) consumerSlotIndex
            }

      handleRollBackward newTip ChainSyncDatabaseState{..} sendBlocks rollbackSlot hash = do
        let
          beforeRollback Block{header = MarloweBlockHeader slot _ _} = slot <= rollbackSlot
          (beforeBlocks, afterBlocks) = Map.partition beforeRollback blocks
          afterTxIds = do
            Block{txs} <- Map.elems afterBlocks
            MarloweTx{..} <- txs
            pure marloweTx_id
          MarloweSlotNo slotInt = rollbackSlot
          (consumerSlotIndexAfterRollback, consumerSlotIndexBeforeRollback) =
            IntMap.partitionWithKey (const . (> fromIntegral slotInt)) consumerSlotIndex
        sendChan sendBlocks $ Map.elems afterBlocks
        pure ChainSyncDatabaseState
          { blocks = beforeBlocks
          , transactions = foldr Map.delete transactions afterTxIds
          , intersectionPoints = [MarloweChainPoint rollbackSlot hash]
          , tip = newTip
          , consumerIndex = Map.withoutKeys consumerIndex $ fold consumerSlotIndexAfterRollback
          , consumerSlotIndex = consumerSlotIndexBeforeRollback
          }

remotable ['chainSyncDatabase]

process :: ChainSyncDatabaseDependencies -> Closure (Process ())
process = $(mkClosure 'chainSyncDatabase)
