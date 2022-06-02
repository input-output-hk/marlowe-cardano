{-# LANGUAGE BangPatterns              #-}
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
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Binary (Binary, decodeOrFail, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Data (Typeable)
import Data.Foldable (fold)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime, secondsToNominalDiffTime)
import GHC.Generics (Generic)
import Language.Marlowe.Runtime.Chain.Types (MarloweBlockHeader (..), MarloweBlockHeaderHash, MarloweChainPoint (..),
                                             MarloweChainTip (..), MarloweSlotNo (..), MarloweTx (..), MarloweTxId,
                                             MarloweTxIn (MarloweTxIn), TxOutRef (..))
import System.Directory (doesFileExist, removeFile)

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
  | GetBlocksAfter MarloweChainPoint (SendPort (Maybe [Block]))
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
  , blockSlotIndex     :: IntMap MarloweBlockHeaderHash
  }
  deriving (Generic, Typeable)
  deriving anyclass (Binary)

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

getBlocksAfter :: ChainSyncQueryChan -> MarloweChainPoint -> Process (Maybe [Block])
getBlocksAfter sendQuery point = do
  (sendPort, receiveResponse) <- newChan
  sendChan sendQuery $ GetBlocksAfter point sendPort
  receiveChan receiveResponse

file :: FilePath
file = "./chain-db"

writeFrequency :: NominalDiffTime
writeFrequency = secondsToNominalDiffTime 5

chainSyncDatabase :: ChainSyncDatabaseDependencies -> Process ()
chainSyncDatabase ChainSyncDatabaseDependencies{..} = do
  (sendQuery, receiveQuery) <- newChan
  sendChan initStoreChan sendQuery
  let go (lastWrite, state) = go =<< handleQuery lastWrite state =<< receiveChan receiveQuery
  !initialState <- liftIO do
    fileExists <- doesFileExist file
    if fileExists then do
      bytes <- BS.readFile file
      pure case decodeOrFail $ LBS.fromStrict bytes of
        Left _        -> emptyState
        Right (_,_,a) -> a
    else do
      LBS.writeFile file $ encode emptyState
      pure emptyState
  lastWrite <- liftIO getCurrentTime
  go (lastWrite, initialState)
    where
      emptyState = ChainSyncDatabaseState
        { blocks = mempty
        , transactions = mempty
        , intersectionPoints = mempty
        , tip = MarloweChainTipAtGenesis
        , consumerIndex = mempty
        , consumerSlotIndex = mempty
        , blockSlotIndex = mempty
        }
      handleQuery lastWrite state@ChainSyncDatabaseState{..} = \case
        GetIntersectionPoints sendResponse       -> (lastWrite, state) <$ sendChan sendResponse intersectionPoints
        RollForward block newTip                 -> handleRollForward newTip lastWrite state block
        RollBackwardToGenesis newTip             -> saveState emptyState { tip = newTip }
        RollBackward slot hash newTip sendBlocks -> handleRollBackward newTip state sendBlocks slot hash
        GetTip sendTip                           -> (lastWrite, state) <$ sendChan sendTip tip
        GetConsumer txOut sendTx                 -> (lastWrite, state) <$ sendChan sendTx (lookupConsumer state txOut)
        GetBlocksAfter point sendBlock                 -> (lastWrite, state) <$ sendChan  sendBlock (lookupBlocksAfer state point)

      saveState state = liftIO do
        fileExists <- doesFileExist file
        when fileExists do
          removeFile file
        LBS.writeFile file $ encode state
        now <- getCurrentTime
        pure (now, state)

      lookupConsumer ChainSyncDatabaseState{..} txOut = do
        txId <- Map.lookup txOut consumerIndex
        Map.lookup txId transactions

      lookupBlocksAfer ChainSyncDatabaseState{..} MarloweChainPointAtGenesis =
        Just $ Map.elems blocks
      lookupBlocksAfer ChainSyncDatabaseState{..} (MarloweChainPoint (MarloweSlotNo slot) hash) = do
        _ <- Map.lookup hash blocks
        let
          filteredIndex = IntMap.difference blockSlotIndex
            $ IntMap.withoutKeys blockSlotIndex
            $ IntSet.fromDistinctAscList [0..fromIntegral slot]
        pure $ Map.elems $ Map.withoutKeys blocks $ Set.fromList $ IntMap.elems filteredIndex

      handleRollForward
        newTip
        lastWrite
        ChainSyncDatabaseState{..}
        block@Block{header = header@(MarloweBlockHeader slot hash _), txs} = do
          let
            newConsumptions = Map.fromList do
              MarloweTx{..} <- txs
              MarloweTxIn txid txix _ <- marloweTx_inputs
              pure (TxOutRef txid txix, marloweTx_id)
            MarloweSlotNo slotInt = slot
          now <- liftIO getCurrentTime
          let diff = diffUTCTime now lastWrite
          let
            action
              | diff >= writeFrequency = saveState
              | otherwise = pure . (lastWrite,)
          action ChainSyncDatabaseState
            { blocks = Map.insert hash block blocks
            , transactions = Map.union transactions
                $ Map.fromList
                $ (marloweTx_id &&& TxWithBlockHeader header) <$> txs
            , intersectionPoints = [MarloweChainPoint slot hash]
            , tip = newTip
            , consumerIndex = Map.union consumerIndex newConsumptions
            , consumerSlotIndex = IntMap.insert (fromIntegral slotInt) (Map.keysSet newConsumptions) consumerSlotIndex
            , blockSlotIndex = IntMap.insert (fromIntegral slotInt) hash blockSlotIndex
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
          (_, blockSlotIndexBeforeRollback) =
            IntMap.partitionWithKey (const . (> fromIntegral slotInt)) blockSlotIndex
        sendChan sendBlocks $ Map.elems afterBlocks
        saveState ChainSyncDatabaseState
          { blocks = beforeBlocks
          , transactions = foldr Map.delete transactions afterTxIds
          , intersectionPoints = [MarloweChainPoint rollbackSlot hash]
          , tip = newTip
          , consumerIndex = Map.withoutKeys consumerIndex $ fold consumerSlotIndexAfterRollback
          , consumerSlotIndex = consumerSlotIndexBeforeRollback
          , blockSlotIndex = blockSlotIndexBeforeRollback
          }

remotable ['chainSyncDatabase]

process :: ChainSyncDatabaseDependencies -> Closure (Process ())
process = $(mkClosure 'chainSyncDatabase)
