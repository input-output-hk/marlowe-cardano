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
import Control.Distributed.Process (Closure, Process, SendPort, newChan, receiveChan, say, sendChan, spawnLocal)
import Control.Distributed.Process.Closure (mkClosure, remotable)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Binary (Binary, decode, decodeOrFail, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Data (Typeable)
import Data.Functor (void)
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Base (when)
import GHC.Generics (Generic)
import Language.Marlowe.Runtime.Chain.Types (MarloweBlockHeader (..), MarloweBlockHeaderHash, MarloweBlockNo (..),
                                             MarloweChainPoint (..), MarloweChainTip (..), MarloweSlotNo (..),
                                             MarloweTx (..), MarloweTxId, slotToIntegral)
import System.Directory (createDirectory, doesDirectoryExist, renamePath)

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
  | GetBlocksAfter MarloweChainPoint (SendPort (Maybe [Block]))
  | RollForward Block MarloweChainTip
  | RollBackward MarloweChainPoint MarloweChainTip
  deriving (Generic, Typeable, Show, Eq)
  deriving anyclass Binary

type ChainSyncQueryChan = SendPort ChainSyncQuery

data Block = Block
  { header :: MarloweBlockHeader
  , txs    :: [MarloweTx]
  }
  deriving (Generic, Typeable, Show, Eq)
  deriving anyclass Binary

data BlockKey = BlockKey MarloweSlotNo MarloweBlockHeaderHash
  deriving (Generic, Typeable, Show, Eq, Ord)
  deriving anyclass Binary

data ChainSyncDatabaseState = ChainSyncDatabaseState
  { blocks               :: Map BlockKey (MarloweBlockNo, Map MarloweTxId MarloweTx)
  , intersectionPoints   :: [MarloweChainPoint]
  , tip                  :: MarloweChainTip
  , lastSavedBlocksCount :: Int
  }
  deriving (Generic, Typeable)
  deriving anyclass (Binary)

rollForward :: ChainSyncQueryChan -> Block -> MarloweChainTip -> Process ()
rollForward chan block = sendChan chan . RollForward block

rollBackward :: ChainSyncQueryChan -> MarloweChainPoint -> MarloweChainTip -> Process  ()
rollBackward chan point = sendChan chan . RollBackward point

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

getBlocksAfter :: ChainSyncQueryChan -> MarloweChainPoint -> Process (Maybe [Block])
getBlocksAfter sendQuery point = do
  (sendPort, receiveResponse) <- newChan
  sendChan sendQuery $ GetBlocksAfter point sendPort
  receiveChan receiveResponse

dir :: FilePath
dir = "./chain-db"

blocksFile :: FilePath
blocksFile = dir <> "/blocks"

tipFile :: FilePath
tipFile = dir <> "/tip"

writeFrequency :: Int
writeFrequency = 400

chainSyncDatabase :: ChainSyncDatabaseDependencies -> Process ()
chainSyncDatabase ChainSyncDatabaseDependencies{..} = do
  (sendQuery, receiveQuery) <- newChan
  sendChan initStoreChan sendQuery
  let go state = go =<< handleQuery state =<< receiveChan receiveQuery
  initialState <- liftIO do
    directoryExists <- doesDirectoryExist dir
    unless directoryExists do
      createDirectory dir
      LBS.writeFile tipFile $ encode MarloweChainTipAtGenesis
      LBS.writeFile blocksFile $ encode $ blocks emptyState
    tipBytes <- BS.readFile tipFile
    blockBytes <- BS.readFile blocksFile
    let tip = decode $ LBS.fromStrict tipBytes
    let
      blocks = case decodeOrFail $ LBS.fromStrict blockBytes of
        Left _        -> mempty
        Right (_,_,b) -> b
      toPoint (BlockKey s h) = MarloweChainPoint s h
    pure $ ChainSyncDatabaseState blocks (take 1 $ toPoint . fst <$> Map.toDescList blocks) tip (Map.size blocks)
  go initialState
    where
      emptyState = ChainSyncDatabaseState
        { blocks = mempty
        , intersectionPoints = mempty
        , tip = MarloweChainTipAtGenesis
        , lastSavedBlocksCount = 0
        }

      handleQuery state@ChainSyncDatabaseState{..} = \case
        GetIntersectionPoints sendResponse       -> state <$ sendChan sendResponse intersectionPoints
        GetBlocksAfter point sendResponse        -> state <$ sendChan sendResponse (lookupBlocksAfer state point)
        RollForward block newTip                 -> saveStateDebounced block newTip $ handleRollForward newTip state block
        RollBackward MarloweChainPointAtGenesis newTip             -> saveState emptyState { tip = newTip }
        RollBackward point newTip                -> handleRollBackward state point newTip
        GetTip sendTip                           -> state <$ sendChan sendTip tip

      saveStateDebounced Block{..} tip state = do
        let tipNo = getTipNo tip
        let MarloweBlockHeader slotNo _ _ = header
        let blockCount = Map.size $ blocks state
        let unsavedBlocks = blockCount - lastSavedBlocksCount state
        if tipNo == slotToIntegral slotNo || unsavedBlocks >= writeFrequency then
          saveState state
        else
          pure state

      overwrite path a = do
        let tmpPath = path <> "-tmp"
        LBS.writeFile tmpPath $ encode a
        renamePath tmpPath path

      saveState state@ChainSyncDatabaseState{..} = do
        let blockCount = Map.size blocks
        when (blockCount /= lastSavedBlocksCount) $ void $ spawnLocal do
          say $ "saving chain state with " <> show blockCount <> " blocks"
          liftIO $ overwrite blocksFile blocks
        liftIO $ overwrite tipFile tip
        pure state { lastSavedBlocksCount = blockCount }

      lookupBlocksAfer ChainSyncDatabaseState{..} point = fmap toBlocks $ case point of
        MarloweChainPointAtGenesis -> Just blocks
        MarloweChainPoint slot hash -> do
          -- make sure this block is actually in the database
          _ <- Map.lookup (BlockKey slot hash) blocks
          pure $ snd $ breakMap (BlockKey slot hash) blocks

      toBlocks = fmap toBlock . Map.toAscList

      toBlock (BlockKey slot hash, (blockNo, txs)) =
        Block (MarloweBlockHeader slot hash blockNo) $ Map.elems txs

      breakMap k = bimap Map.fromDistinctAscList Map.fromDistinctAscList . break ((> k) . fst) . Map.toAscList

      handleRollForward newTip ChainSyncDatabaseState{..} (Block (MarloweBlockHeader slot hash blockNo) txs) =
          let
            txMap = Map.fromList $ (marloweTx_id &&& id) <$> txs
            blockKey = BlockKey slot hash
          in
            ChainSyncDatabaseState
              { blocks = if Map.null txMap then blocks else Map.insert blockKey (blockNo, txMap) blocks
              , intersectionPoints = [MarloweChainPoint slot hash]
              , tip = newTip
              , lastSavedBlocksCount = lastSavedBlocksCount
              }

      handleRollBackward _ MarloweChainPointAtGenesis newTip =
        saveState emptyState { tip = newTip }
      handleRollBackward ChainSyncDatabaseState{..} point@(MarloweChainPoint slot hash) newTip =
        do
        let
          (beforeBlocks, _) = breakMap (BlockKey slot hash) blocks
        saveState ChainSyncDatabaseState
          { blocks = beforeBlocks
          , intersectionPoints = [point]
          , tip = newTip
          , lastSavedBlocksCount = lastSavedBlocksCount
          }

      getTipNo MarloweChainTipAtGenesis                = 0
      getTipNo (MarloweChainTip (MarloweSlotNo n) _ _) = n

remotable ['chainSyncDatabase]

process :: ChainSyncDatabaseDependencies -> Closure (Process ())
process = $(mkClosure 'chainSyncDatabase)
