{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Process.ContractHeaderIndexer
  where

import Control.Concurrent.STM
import Control.Concurrent.STM.Delay (newDelay, waitDelay)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe
import Data.Aeson (Value(Null))
import Data.Bifunctor (Bifunctor(bimap))
import Data.Coerce (coerce)
import Data.Functor (void)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import Language.Marlowe.Protocol.HeaderSync.Client
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import qualified Language.Marlowe.Runtime.Core.Api as Core
import qualified Language.Marlowe.Runtime.Discovery.Api as Discovery
import Language.Marlowe.Runtime.Web
import Servant.Pagination

data ContractHeaderIndexer = ContractHeaderIndexer
  { runContractHeaderIndexer :: IO ()
  , loadContractHeaders :: Range "contractId" TxOutRef -> IO (Maybe [ContractHeader])
  }

-- | A function signature for running a client for some protocol in some monad m.
type RunClient m client = forall a. client m a -> m a

mkContractHeaderIndexer :: RunClient IO MarloweHeaderSyncClient -> IO ContractHeaderIndexer
mkContractHeaderIndexer runDiscoveryClient = do
  headersTVar <- newTVarIO mempty
  indexTVar <- newTVarIO mempty
  inSync <- newEmptyTMVarIO
  let
    client = MarloweHeaderSyncClient
      $ pure clientIdle
    clientIdle = SendMsgRequestNext clientNext
    clientNext = ClientStNext
      { recvMsgNewHeaders = \Chain.BlockHeader{..} headers -> atomically do
          modifyTVar headersTVar
            $ IntMap.insert (fromIntegral slotNo)
            $ Map.fromList
            $ (\header@Discovery.ContractHeader{..} -> (toApiTxOutRef contractId, toApiContractHeader header)) <$> headers
          modifyTVar indexTVar
            $ Map.union
            $ Map.fromList
            $ (,fromIntegral slotNo) . toApiTxOutRef . Discovery.contractId <$> headers
          pure clientIdle
      , recvMsgRollBackward = \point -> atomically do
          case point of
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
          pure clientIdle
      , recvMsgWait = do
          delay <- newDelay 500_000 -- 0.5 seconds
          atomically do
            void $ tryPutTMVar inSync ()
            waitDelay delay
            pure $ SendMsgPoll clientNext
      }
  pure ContractHeaderIndexer
    { runContractHeaderIndexer = runDiscoveryClient client
    , loadContractHeaders = \range@Range{..} -> atomically $ runMaybeT do
        lift $ readTMVar inSync
        headers <- lift $ readTVar headersTVar
        index <- lift $ readTVar indexTVar
        headersFlat <- MaybeT $ pure $ flattenFrom rangeOrder index headers rangeValue
        pure $ applyRange range headersFlat
    }

toApiTxOutRef :: Core.ContractId -> TxOutRef
toApiTxOutRef (Core.ContractId Chain.TxOutRef{..}) = TxOutRef
  { txId = coerce txId
  , txIx = coerce txIx
  }

toApiContractHeader :: Discovery.ContractHeader -> ContractHeader
toApiContractHeader Discovery.ContractHeader{..} = ContractHeader
  { contractId = toApiTxOutRef contractId
  , roleTokenMintingPolicyId = coerce rolesCurrency
  , version = case marloweVersion of
      Core.SomeMarloweVersion Core.MarloweV1 -> V1
  , metadata = Metadata Null <$ metadata -- TODO
  , status = Confirmed $ toApiBlockHeader blockHeader
  }

toApiBlockHeader :: Chain.BlockHeader -> BlockHeader
toApiBlockHeader Chain.BlockHeader{..} = BlockHeader
  { slotNo = coerce slotNo
  , blockNo = coerce blockNo
  , blockHeaderHash = coerce headerHash
  }

flattenFrom
  :: RangeOrder
  -> Map TxOutRef IntMap.Key
  -> IntMap (Map TxOutRef ContractHeader)
  -> Maybe TxOutRef
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
      RangeDesc -> Map.toAscList
    flattenBlocks p = fmap snd . dropWhile (p . fst) . case order of
      RangeAsc -> IntMap.toAscList
      RangeDesc -> IntMap.toAscList
