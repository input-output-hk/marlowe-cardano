{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module defines a worker process that subscribes to contract header
-- updates via the MarloweHeaderSync protocol and indexes them for fast
-- loading.

module Language.Marlowe.Runtime.Web.Server.ContractHeaderIndexer
  where

import Control.Concurrent.STM
import Control.Concurrent.STM.Delay (newDelay, waitDelay)
import Data.Functor (void)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Void (Void)
import Data.Word (Word64)
import Language.Marlowe.Protocol.HeaderSync.Client
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Core.Api (ContractId)
import Language.Marlowe.Runtime.Discovery.Api
import qualified Language.Marlowe.Runtime.Web as Web
import Language.Marlowe.Runtime.Web.Server.DTO (toDTO)
import Network.Protocol.Driver (RunAsPeer)
import Observe.Event (EventBackend, addField, withEvent)
import Observe.Event.DSL (FieldSpec(..), SelectorSpec(..))
import Observe.Event.Render.JSON.DSL.Compile (compile)
import Observe.Event.Syntax ((≔))
import Servant.Pagination

type ContractHeaders = [Web.ContractHeader]

compile $ SelectorSpec ["contract", "header", "indexer"]
  [ ["new", "headers"] ≔ FieldSpec ["new", "headers"]
      [ "slotNo" ≔ ''Word64
      , "blockNo" ≔ ''Word64
      , "blockHeaderHash" ≔ ''String
      , ["contract", "headers"] ≔ ''ContractHeaders
      ]
  , ["rollback", "to", "block"] ≔ FieldSpec "rollback"
      [ "rollbackSlotNo" ≔ ''Word64
      , "rollbackBlockNo" ≔ ''Word64
      , "rollbackBlockHeaderHash" ≔ ''String
      ]
  , ["rollback", "to", "genesis"] ≔ ''Void
  , "wait" ≔ ''Void
  ]

-- | Dependencies for the a ContractHeaderIndexer
data ContractHeaderIndexerDependencies r = ContractHeaderIndexerDependencies
  { runMarloweHeaderSyncClient :: RunAsPeer IO MarloweHeaderSyncClient
  , eventBackend :: EventBackend IO r ContractHeaderIndexerSelector
  }

-- | Signature for a delegate that loads a list of contract headers.
type LoadContractHeaders m
   = Maybe ContractId -- ^ ID of the contract to start from.
  -> Int -- ^ Limit: the maximum number of contract headers to load.
  -> Int -- ^ Offset: how many contract headers after the initial one to skip.
  -> RangeOrder -- ^ Whether to load an ascending or descending list.
  -> m (Maybe [ContractHeader]) -- ^ Nothing if the initial ID is not found

-- | Public API of the ContractHeaderIndexer
data ContractHeaderIndexer = ContractHeaderIndexer
  { runContractHeaderIndexer :: IO () -- ^ Run the indexer process
  , loadContractHeaders :: LoadContractHeaders IO -- ^ Load contract headers from the indexer.
  }

-- | Create a new contract header indexer.
mkContractHeaderIndexer :: ContractHeaderIndexerDependencies r -> STM ContractHeaderIndexer
mkContractHeaderIndexer ContractHeaderIndexerDependencies{..} = do
  -- State variable that stores contract headers in a nested map. The outer
  -- IntMap indexes collections of contract headers by slot number, and the
  -- inner map indexes the contract headers for that slot by contract ID. This
  -- nesting encodes the two levels of ordering for contract headers - sort
  -- first by slot, then by contractId as a byte string.
  contractsTVar <- newTVar (mempty :: IntMap (Map ContractId ContractHeader))
  -- State variable that stores a reverse index that allows the slot number to
  -- be looked up for a particular contract ID.
  slotIndexTVar <- newTVar (mempty :: Map ContractId IntMap.Key)
  -- Synchronization variable that is used to wait until the sync client has
  -- caught up to the tip of the server.
  inSync <- newEmptyTMVar
  let
    -- Client that keeps the local state synchronized with an upstream peer
    -- (e.g. a running instance of marlowe-discovery).
    client = MarloweHeaderSyncClient $ pure clientIdle
    -- When Idle, always request the next set of headers.
    clientIdle = SendMsgRequestNext clientNext
    clientNext = ClientStNext
      -- Handle new headers by updating the sate variables
      { recvMsgNewHeaders = \block contracts -> withEvent eventBackend NewHeaders \ev -> do
          addField ev $ SlotNo $ Chain.unSlotNo $ Chain.slotNo block
          addField ev $ BlockNo $ Chain.unBlockNo $ Chain.blockNo block
          addField ev $ BlockHeaderHash $ show $ Chain.headerHash block
          addField ev $ ContractHeaders $ toDTO contracts
          atomically $ addNewContractHeaders contractsTVar slotIndexTVar block contracts
          pure clientIdle
      -- Handle rollbacks by removing rolled back contract headers.
      , recvMsgRollBackward = \point -> do
          case point of
            Chain.Genesis -> withEvent eventBackend RollbackToGenesis \_ -> do
              atomically $ rollback contractsTVar slotIndexTVar point
            Chain.At block -> withEvent eventBackend RollbackToBlock \ev -> do
              addField ev $ RollbackSlotNo $ Chain.unSlotNo $ Chain.slotNo block
              addField ev $ RollbackBlockNo $ Chain.unBlockNo $ Chain.blockNo block
              addField ev $ RollbackBlockHeaderHash $ show $ Chain.headerHash block
              atomically $ rollback contractsTVar slotIndexTVar point
          pure clientIdle
      -- When told to wait, wait for 1 second then poll the server again.
      , recvMsgWait = withEvent eventBackend Wait \_ -> do
          delay <- newDelay 1_000_000 -- 1 second
          atomically do
            -- Waiting means we are caught up to the tip. Unblock queries by
            -- putting a value the inSync TMVar.
            void $ tryPutTMVar inSync ()
            waitDelay delay
            pure $ SendMsgPoll clientNext
      }
  pure ContractHeaderIndexer
    { runContractHeaderIndexer = runMarloweHeaderSyncClient client
    , loadContractHeaders = \startFrom limit offset order -> atomically do
        -- Wait until we are in sync.
        readTMVar inSync
        -- Load all contracts
        contracts <- readTVar contractsTVar
        -- Load the reverse index
        index <- readTVar slotIndexTVar
        -- Flatten the contracts from the starting point, in the requested
        -- order. Then skip the requested quantity of results and take the
        -- requested quantity.
        pure
          $ List.take limit . List.drop offset
          <$> flattenFrom order index contracts startFrom
    }

-- Updates the state variables of the indexer to exclude values after a
-- particular chain point.
rollback
  :: TVar (IntMap (Map ContractId ContractHeader))
  -> TVar (Map ContractId IntMap.Key)
  -> Chain.ChainPoint
  -> STM ()
rollback contractsTVar slotIndexTVar = \case
  -- Rolling back to Genesis means discarding everything.
  Chain.Genesis -> do
    writeTVar contractsTVar mempty
    writeTVar slotIndexTVar mempty
  -- Rolling back to a point means discarding all records with slot numbers
  -- greater than that point.
  Chain.At Chain.BlockHeader{..} -> do
    modifyTVar contractsTVar $ IntMap.filterWithKey (\k _ -> k <= fromIntegral slotNo)
    modifyTVar slotIndexTVar $ Map.filter (\v -> v <= fromIntegral slotNo)

-- Updates the start variables of the indexer to include new headers at a new
-- block.
addNewContractHeaders
  :: TVar (IntMap (Map ContractId ContractHeader))
  -> TVar (Map ContractId IntMap.Key)
  -> Chain.BlockHeader
  -> [ContractHeader]
  -> STM ()
addNewContractHeaders contractsTVar slotIndexTVar Chain.BlockHeader{..} contracts = do
  modifyTVar contractsTVar
    $ IntMap.insert (fromIntegral slotNo)
    $ Map.fromList
    $ tag contractId <$> contracts
  modifyTVar slotIndexTVar
    $ Map.union
    $ Map.fromList
    $ (,fromIntegral slotNo) . contractId <$> contracts

-- Convert a value to a tuple using a projection function for the first
-- element.
tag :: (b -> a) -> b -> (a, b)
tag f a = (f a, a)

flattenFrom
  :: RangeOrder
  -> Map ContractId IntMap.Key
  -> IntMap (Map ContractId ContractHeader)
  -> Maybe ContractId
  -> Maybe [ContractHeader]
flattenFrom order index blocks = \case
  Nothing -> Just $ flattenHeaders (pure False) =<< flattenBlocks (pure False) blocks
  Just startFrom -> do
    startFromBlock <- Map.lookup startFrom index
    case flattenHeaders (/= startFrom) =<< flattenBlocks (< startFromBlock) blocks of
      [] -> Nothing
      hs -> Just hs
  where
    flattenHeaders p = fmap snd . dropWhile (p . fst) . case order of
      RangeAsc -> Map.toAscList
      RangeDesc -> Map.toDescList
    flattenBlocks p = fmap snd . dropWhile (p . fst) . case order of
      RangeAsc -> IntMap.toAscList
      RangeDesc -> IntMap.toDescList
