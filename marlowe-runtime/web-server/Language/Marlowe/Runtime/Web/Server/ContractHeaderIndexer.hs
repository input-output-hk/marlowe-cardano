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
import Language.Marlowe.Runtime.Web.Server.Util (applyRangeToAscList)
import Network.Protocol.Driver (RunClient)
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
  { runMarloweHeaderSyncClient :: RunClient IO MarloweHeaderSyncClient
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
          atomically $ addNewContractHeaders contractsTVar block contracts
          pure clientIdle
      -- Handle rollbacks by removing rolled back contract headers.
      , recvMsgRollBackward = \point -> do
          case point of
            Chain.Genesis -> withEvent eventBackend RollbackToGenesis \_ -> do
              atomically $ rollback contractsTVar point
            Chain.At block -> withEvent eventBackend RollbackToBlock \ev -> do
              addField ev $ RollbackSlotNo $ Chain.unSlotNo $ Chain.slotNo block
              addField ev $ RollbackBlockNo $ Chain.unBlockNo $ Chain.blockNo block
              addField ev $ RollbackBlockHeaderHash $ show $ Chain.headerHash block
              atomically $ rollback contractsTVar point
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
        contracts <- readTVar contractsTVar
        pure
          $ applyRangeToAscList contractId startFrom limit offset order
          $ fmap snd . Map.toAscList . snd =<< IntMap.toAscList contracts
    }

-- Updates the state variables of the indexer to exclude values after a
-- particular chain point.
rollback
  :: TVar (IntMap (Map ContractId ContractHeader))
  -> Chain.ChainPoint
  -> STM ()
rollback contractsTVar = \case
  -- Rolling back to Genesis means discarding everything.
  Chain.Genesis -> writeTVar contractsTVar mempty
  -- Rolling back to a point means discarding all records with slot numbers
  -- greater than that point.
  Chain.At Chain.BlockHeader{..} ->
    modifyTVar contractsTVar $ IntMap.filterWithKey (\k _ -> k <= fromIntegral slotNo)

-- Updates the start variables of the indexer to include new headers at a new
-- block.
addNewContractHeaders
  :: TVar (IntMap (Map ContractId ContractHeader))
  -> Chain.BlockHeader
  -> [ContractHeader]
  -> STM ()
addNewContractHeaders contractsTVar Chain.BlockHeader{..} contracts = do
  modifyTVar contractsTVar
    $ IntMap.insert (fromIntegral slotNo)
    $ Map.fromList
    $ tag contractId <$> contracts

-- Convert a value to a tuple using a projection function for the first
-- element.
tag :: (b -> a) -> b -> (a, b)
tag f a = (f a, a)
