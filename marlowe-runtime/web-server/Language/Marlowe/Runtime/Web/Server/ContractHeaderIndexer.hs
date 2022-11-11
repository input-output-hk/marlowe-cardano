{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.Web.Server.ContractHeaderIndexer
  where

import Control.Concurrent.STM
import Control.Concurrent.STM.Delay (newDelay, waitDelay)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe
import Data.Bifunctor (Bifunctor(bimap))
import Data.Functor (void)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Language.Marlowe.Protocol.HeaderSync.Client
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Core.Api (ContractId)
import Language.Marlowe.Runtime.Discovery.Api
import Servant.Pagination

newtype ContractHeaderIndexerDependencies = ContractHeaderIndexerDependencies
  { runMarloweHeaderSyncClient :: forall a. MarloweHeaderSyncClient IO a -> IO a
  }

type LoadContractHeaders m
   = Maybe ContractId
  -> Int
  -> Int
  -> RangeOrder
  -> m (Maybe [ContractHeader])

data ContractHeaderIndexer = ContractHeaderIndexer
  { runContractHeaderIndexer :: IO ()
  , loadContractHeaders :: LoadContractHeaders IO
  }

mkContractHeaderIndexer :: ContractHeaderIndexerDependencies-> IO ContractHeaderIndexer
mkContractHeaderIndexer ContractHeaderIndexerDependencies{..} = do
  headersTVar <- newTVarIO mempty
  indexTVar <- newTVarIO mempty
  inSync <- newEmptyTMVarIO
  let
    client = MarloweHeaderSyncClient
      $ pure clientIdle
    clientIdle = SendMsgRequestNext clientNext
    clientNext = ClientStNext
      { recvMsgNewHeaders = \block headers -> do
          atomically $ addNewContractHeaders headersTVar indexTVar block headers
          pure clientIdle
      , recvMsgRollBackward = \point -> do
          atomically $ rollback headersTVar indexTVar point
          pure clientIdle
      , recvMsgWait = do
          delay <- newDelay 500_000 -- 0.5 seconds
          atomically do
            void $ tryPutTMVar inSync ()
            waitDelay delay
            pure $ SendMsgPoll clientNext
      }
  pure ContractHeaderIndexer
    { runContractHeaderIndexer = runMarloweHeaderSyncClient client
    , loadContractHeaders = \startFrom limit offset order -> atomically $ runMaybeT do
        lift $ readTMVar inSync
        headers <- lift $ readTVar headersTVar
        index <- lift $ readTVar indexTVar
        headersFlat <- MaybeT $ pure $ flattenFrom order index headers startFrom
        pure
          $ List.take limit
          $ List.drop offset headersFlat
    }

rollback
  :: TVar (IntMap (Map ContractId ContractHeader))
  -> TVar (Map ContractId IntMap.Key)
  -> Chain.ChainPoint
  -> STM ()
rollback headersTVar indexTVar = \case
  Chain.Genesis -> do
    writeTVar headersTVar mempty
    writeTVar indexTVar mempty
  Chain.At Chain.BlockHeader{..} -> do
    blocks <- readTVar headersTVar
    let
      (blocksBefore, blocksAfter) =
        bimap IntMap.fromDistinctAscList IntMap.fromDistinctAscList
          $ break ((>= fromIntegral slotNo) . fst)
          $ IntMap.toAscList blocks
      contractsAfter = foldMap Map.keysSet blocksAfter
    writeTVar headersTVar blocksBefore
    modifyTVar indexTVar $ flip Map.withoutKeys contractsAfter

addNewContractHeaders
  :: TVar (IntMap (Map ContractId ContractHeader))
  -> TVar (Map ContractId IntMap.Key)
  -> Chain.BlockHeader
  -> [ContractHeader]
  -> STM ()
addNewContractHeaders headersTVar indexTVar Chain.BlockHeader{..} headers = do
  modifyTVar headersTVar
    $ IntMap.insert (fromIntegral slotNo)
    $ Map.fromList
    $ tag contractId <$> headers
  modifyTVar indexTVar
    $ Map.union
    $ Map.fromList
    $ (,fromIntegral slotNo) . contractId <$> headers

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
