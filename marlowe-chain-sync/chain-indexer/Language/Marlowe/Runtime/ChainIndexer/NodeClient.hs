{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Marlowe.Runtime.ChainIndexer.NodeClient (
  Changes (..),
  CostModel (..),
  NodeClient (..),
  NodeClientDependencies (..),
  isEmptyChanges,
  nodeClient,
  toEmptyChanges,
) where

import Cardano.Api (
  Block (..),
  BlockHeader (..),
  BlockInMode (..),
  BlockNo (..),
  ChainPoint (..),
  ChainSyncClientPipelined (..),
  ChainTip (..),
  LocalChainSyncClient (..),
  LocalNodeClientProtocols (..),
  LocalNodeClientProtocolsInMode,
  ShelleyBasedEra (..),
  SlotNo (..),
  getBlockHeader,
  serialiseToRawBytes,
 )
import Cardano.Api.ChainSync.ClientPipelined (
  ClientPipelinedStIdle (..),
  ClientPipelinedStIntersect (..),
  ClientStNext (..),
  MkPipelineDecision,
  N (..),
  Nat (..),
  PipelineDecision (..),
  mapChainSyncClientPipelined,
  pipelineDecisionLowHighMark,
  runPipelineDecision,
 )
import Cardano.Chain.Block (ABody (..))
import qualified Cardano.Chain.Block as BL
import Cardano.Chain.UTxO (txpTxs)
import qualified Cardano.Ledger.Alonzo.TxSeq as A
import qualified Cardano.Ledger.Block as C
import Cardano.Ledger.Shelley.BlockChain (txSeqTxns)
import Colog (Message, WithLog, logInfo)
import Control.Arrow ((&&&))
import Control.Concurrent.Component
import Control.Concurrent.STM (STM, TVar, modifyTVar, newTVar, readTVar, writeTVar)
import Control.Monad (guard, when)
import Control.Monad.IO.Class (liftIO)
import Data.Base16.Types (extractBase16)
import Data.ByteString.Base16 (encodeBase16)
import Data.IORef (IORef)
import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap
import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..))
import qualified Data.Sequence.Strict as StrictSeq
import Data.String (fromString)
import Data.Time (UTCTime, diffUTCTime, getCurrentTime, secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Language.Marlowe.Runtime.ChainIndexer.Database (GetIntersectionPoints (..))
import qualified Ouroboros.Consensus.Byron.Ledger as C
import qualified Ouroboros.Consensus.Shelley.Ledger as C
import Ouroboros.Network.Point (WithOrigin (..))
import Text.Printf (printf)
import UnliftIO (MonadIO, MonadUnliftIO, atomicModifyIORef, atomically, newIORef, withRunInIO, writeIORef)

type NumberedCardanoBlock = (BlockNo, BlockInMode)
type NumberedChainTip = (WithOrigin BlockNo, ChainTip)

-- | Describes a batch of chain data changes to write.
data Changes = Changes
  { changesRollback :: !(Maybe ChainPoint)
  -- ^ Point to rollback to before writing any blocks.
  , changesBlocks :: ![BlockInMode]
  -- ^ New blocks to write.
  , changesTip :: !ChainTip
  -- ^ Most recently observed tip of the local node.
  , changesLocalTip :: !ChainTip
  -- ^ Chain tip the changes will advance the local state to.
  , changesBlockCount :: !Int
  -- ^ Number of blocks in the change set.
  , changesTxCount :: !Int
  -- ^ Number of transactions in the change set.
  }

-- | An empty Changes collection.
emptyChanges :: Changes
emptyChanges = Changes Nothing [] ChainTipAtGenesis ChainTipAtGenesis 0 0

-- | Make a set of changes into an empty set (preserves the tip and point fields).
toEmptyChanges :: Changes -> Changes
toEmptyChanges changes =
  changes
    { changesRollback = Nothing
    , changesBlocks = []
    , changesBlockCount = 0
    , changesTxCount = 0
    }

-- | Returns True if the change set is empty.
isEmptyChanges :: Changes -> Bool
isEmptyChanges (Changes Nothing [] _ _ _ _) = True
isEmptyChanges _ = False

-- | Parameters for estimating the cost of writing a batch of changes.
data CostModel = CostModel
  { blockCost :: Int
  , txCost :: Int
  }
  deriving (Show, Eq)

-- | Computes the cost of a change set. The value is a unitless heuristic.
--   Prevents large numbers of transactions and blocks being held in memory.
cost :: CostModel -> Changes -> Int
cost CostModel{..} Changes{..} = changesBlockCount * blockCost + changesTxCount * txCost

-- | The set of dependencies needed by the NodeClient component.
data NodeClientDependencies m = NodeClientDependencies
  { connectToLocalNode :: !(LocalNodeClientProtocolsInMode -> m ())
  -- ^ Connect to the local node.
  , getIntersectionPoints :: !(GetIntersectionPoints m)
  -- ^ How to load the set of initial intersection points for the chain sync client.
  -- | The maximum cost a set of changes is allowed to incur before the
  -- NodeClient blocks.
  , maxCost :: Int
  , costModel :: CostModel
  }

-- | The public API of the NodeClient component.
data NodeClient = NodeClient
  { getChanges :: STM Changes
  -- ^ An STM action that atomically reads and clears the current change set.
  , connected :: STM Bool
  }

-- | Create a new NodeClient component.
nodeClient
  :: forall env m
   . (MonadUnliftIO m, WithLog env Message m)
  => Component m (NodeClientDependencies m) NodeClient
nodeClient = component "indexer-node-client" \NodeClientDependencies{..} -> do
  changesVar <- newTVar emptyChanges
  connectedVar <- newTVar False

  let getChanges :: STM Changes
      getChanges = do
        changes <- readTVar changesVar
        modifyTVar changesVar toEmptyChanges
        pure changes

      pipelinedClient' :: ChainSyncClientPipelined BlockInMode ChainPoint ChainTip m ()
      pipelinedClient' =
        mapChainSyncClientPipelined id id (blockToBlockNo &&& id) (chainTipToBlockNo &&& id) $
          pipelinedClient costModel maxCost changesVar getIntersectionPoints

      runNodeClient :: m ()
      runNodeClient = withRunInIO \runInIO ->
        runInIO $
          connectToLocalNode $
            LocalNodeClientProtocols
              { localChainSyncClient =
                  let ChainSyncClientPipelined client = pipelinedClient'
                   in LocalChainSyncClientPipelined $ hoistClient runInIO $ ChainSyncClientPipelined do
                        atomically $ writeTVar connectedVar True
                        client
              , localTxSubmissionClient = Nothing
              , localTxMonitoringClient = Nothing
              , localStateQueryClient = Nothing
              }

      connected = readTVar connectedVar

  pure (runNodeClient, NodeClient{getChanges, connected})

hoistClient
  :: forall block point tip m n a
   . (Functor m)
  => (forall x. m x -> n x)
  -> ChainSyncClientPipelined block point tip m a
  -> ChainSyncClientPipelined block point tip n a
hoistClient f (ChainSyncClientPipelined m) = ChainSyncClientPipelined $ f $ hoistIdle <$> m
  where
    hoistIdle :: ClientPipelinedStIdle i block point tip m a -> ClientPipelinedStIdle i block point tip n a
    hoistIdle = \case
      SendMsgRequestNext next mNext -> SendMsgRequestNext (hoistNext next) (f $ hoistNext <$> mNext)
      SendMsgRequestNextPipelined idle -> SendMsgRequestNextPipelined $ hoistIdle idle
      SendMsgFindIntersect points intersect -> SendMsgFindIntersect points $ hoistIntersect intersect
      SendMsgDone a -> SendMsgDone a
      CollectResponse mIdle next -> CollectResponse (fmap (f . fmap hoistIdle) mIdle) (hoistNext next)

    hoistNext :: ClientStNext i block point tip m a -> ClientStNext i block point tip n a
    hoistNext ClientStNext{..} =
      ClientStNext
        { recvMsgRollForward = fmap (f . fmap hoistIdle) . recvMsgRollForward
        , recvMsgRollBackward = fmap (f . fmap hoistIdle) . recvMsgRollBackward
        }

    hoistIntersect :: ClientPipelinedStIntersect block point tip m a -> ClientPipelinedStIntersect block point tip n a
    hoistIntersect ClientPipelinedStIntersect{..} =
      ClientPipelinedStIntersect
        { recvMsgIntersectFound = fmap (f . fmap hoistIdle) . recvMsgIntersectFound
        , recvMsgIntersectNotFound = f . fmap hoistIdle . recvMsgIntersectNotFound
        }

blockHeaderToBlockNo :: BlockHeader -> BlockNo
blockHeaderToBlockNo (BlockHeader _ _ blockNo) = blockNo

blockToBlockNo :: BlockInMode -> BlockNo
blockToBlockNo (BlockInMode _ block) = blockHeaderToBlockNo $ getBlockHeader block

chainTipToBlockNo :: ChainTip -> WithOrigin BlockNo
chainTipToBlockNo = \case
  ChainTipAtGenesis -> Origin
  ChainTip _ _ blockNo -> At blockNo

pipelinedClient
  :: forall env m
   . (MonadIO m, WithLog env Message m)
  => CostModel
  -> Int
  -> TVar Changes
  -> GetIntersectionPoints m
  -> ChainSyncClientPipelined NumberedCardanoBlock ChainPoint NumberedChainTip m ()
pipelinedClient costModel maxCost changesVar getIntersectionPoints =
  ChainSyncClientPipelined do
    headers <- sortOn (Down . fmap blockHeaderToBlockNo) <$> runGetIntersectionPoints getIntersectionPoints
    case headers of
      (At (BlockHeader _ hash no) : _) ->
        logInfo $
          fromString $
            printf "Tip of local chain: %s (#%d)" (extractBase16 (encodeBase16 (serialiseToRawBytes hash))) (unBlockNo no)
      _ -> logInfo "Local chain empty"
    pure $
      SendMsgFindIntersect
        (headerToPoint <$> headers)
        ClientPipelinedStIntersect
          { recvMsgIntersectFound = \point tip -> do
              logInfo "Intersected with node chain"
              let getSlotAndBlock = case point of
                    ChainPointAtGenesis -> const Nothing
                    ChainPoint pointSlot _ -> \case
                      Origin -> Nothing
                      At (BlockHeader (SlotNo s) _ b)
                        | SlotNo s <= pointSlot -> Just (fromIntegral s, b)
                        | otherwise -> Nothing
                  slotNoToBlockNo = IntMap.fromList $ mapMaybe getSlotAndBlock headers
              clientStIdle slotNoToBlockNo point tip
          , recvMsgIntersectNotFound = \tip -> do
              logInfo "No intersection with node chain"
              clientStIdle mempty ChainPointAtGenesis tip
          }
  where
    clientStIdle
      :: IntMap BlockNo
      -> ChainPoint
      -> NumberedChainTip
      -> m (ClientPipelinedStIdle 'Z NumberedCardanoBlock ChainPoint NumberedChainTip m ())
    clientStIdle slotNoToBlockNo point nodeTip = do
      lastLog <- newIORef $ posixSecondsToUTCTime 0
      let clientTip = case point of
            ChainPointAtGenesis -> Origin
            ChainPoint (SlotNo s) _ -> case IntMap.lookup (fromIntegral s) slotNoToBlockNo of
              Nothing -> error $ "Unable to find block number for chain point " <> show point
              Just b -> At b
      pure $ mkClientStIdle lastLog costModel maxCost changesVar slotNoToBlockNo pipelinePolicy Zero clientTip nodeTip

    headerToPoint Origin = ChainPointAtGenesis
    headerToPoint (At (BlockHeader s h _)) = ChainPoint s h

    -- How to pipeline. If we have fewer than 50 requests in flight, send
    -- another request. When we hit 50, start collecting responses until we
    -- have 1 request in flight, then repeat. If we are caught up to tip,
    -- requests will not be pipelined.
    pipelinePolicy :: MkPipelineDecision
    pipelinePolicy = pipelineDecisionLowHighMark 1 50

mkClientStIdle
  :: forall env m n
   . (MonadIO m, WithLog env Message m)
  => IORef UTCTime
  -> CostModel
  -> Int
  -> TVar Changes
  -> IntMap BlockNo
  -> MkPipelineDecision
  -> Nat n
  -> WithOrigin BlockNo
  -> NumberedChainTip
  -> ClientPipelinedStIdle n NumberedCardanoBlock ChainPoint NumberedChainTip m ()
mkClientStIdle lastLog costModel maxCost changesVar slotNoToBlockNo pipelineDecision n clientTip nodeTip =
  case (n, runPipelineDecision pipelineDecision n clientTip (fst nodeTip)) of
    (_, (Request, pipelineDecision')) ->
      SendMsgRequestNext (collect pipelineDecision' n) $ pure (collect pipelineDecision' n)
    (_, (Pipeline, pipelineDecision')) ->
      nextPipelineRequest pipelineDecision'
    (Succ n', (CollectOrPipeline, pipelineDecision')) ->
      CollectResponse
        (Just $ pure $ nextPipelineRequest pipelineDecision')
        (collect pipelineDecision' n')
    (Succ n', (Collect, pipelineDecision')) ->
      CollectResponse Nothing (collect pipelineDecision' n')
  where
    nextPipelineRequest
      :: MkPipelineDecision
      -> ClientPipelinedStIdle n NumberedCardanoBlock ChainPoint NumberedChainTip m ()
    nextPipelineRequest pipelineDecision' =
      SendMsgRequestNextPipelined $
        mkClientStIdle lastLog costModel maxCost changesVar slotNoToBlockNo pipelineDecision' (Succ n) clientTip nodeTip

    collect
      :: forall n'
       . MkPipelineDecision
      -> Nat n'
      -> ClientStNext n' NumberedCardanoBlock ChainPoint NumberedChainTip m ()
    collect = mkClientStNext lastLog costModel maxCost changesVar slotNoToBlockNo

mkClientStNext
  :: (MonadIO m, WithLog env Message m)
  => IORef UTCTime
  -> CostModel
  -> Int
  -> TVar Changes
  -> IntMap BlockNo
  -> MkPipelineDecision
  -> Nat n
  -> ClientStNext n NumberedCardanoBlock ChainPoint NumberedChainTip m ()
mkClientStNext lastLog costModel maxCost changesVar slotNoToBlockNo pipelineDecision n =
  ClientStNext
    { recvMsgRollForward = \(blockNo, blockInMode@(BlockInMode _ block)) tip -> do
        let BlockHeader slotNo hash _ = getBlockHeader block
        now <- liftIO getCurrentTime
        canLog <- atomicModifyIORef lastLog \lastLogValue ->
          if diffUTCTime now lastLogValue >= minLogPeriod
            then (now, True)
            else (lastLogValue, False)
        when canLog do
          let localBlockNo = unBlockNo blockNo
          let remoteBlockNo = case fst tip of
                Origin -> 0
                At remote -> unBlockNo remote
          logInfo $
            fromString $
              printf
                "Rolled forward to block %s (#%d of %d) (%d%%)"
                (extractBase16 (encodeBase16 (serialiseToRawBytes hash)))
                localBlockNo
                remoteBlockNo
                ((localBlockNo * 100) `div` remoteBlockNo)
        atomically do
          changes <- readTVar changesVar
          let nextChanges =
                changes
                  { changesBlocks = blockInMode : changesBlocks changes
                  , changesTip = snd tip
                  , changesLocalTip = ChainTip slotNo hash blockNo
                  , changesBlockCount = changesBlockCount changes + 1
                  , changesTxCount = changesTxCount changes + blockTxCount blockInMode
                  }
          -- Retry unless either the current change set is empty, or the next
          -- change set would not be too expensive.
          guard $ isEmptyChanges changes || cost costModel nextChanges <= maxCost
          writeTVar changesVar nextChanges
        let clientTip = At blockNo
        let slotNoToInt (SlotNo s) = fromIntegral s
        let slotNoToBlockNo' = IntMap.insert (slotNoToInt slotNo) blockNo slotNoToBlockNo
        pure $ mkClientStIdle lastLog costModel maxCost changesVar slotNoToBlockNo' pipelineDecision n clientTip tip
    , recvMsgRollBackward = \point tip -> do
        now <- liftIO getCurrentTime
        writeIORef lastLog now
        let clientTip = case point of
              ChainPointAtGenesis -> Origin
              ChainPoint (SlotNo s) hash -> case IntMap.lookup (fromIntegral s) slotNoToBlockNo of
                Nothing -> error $ "Unable to find block number for chain point " <> show point
                Just b -> At (hash, b)
        logInfo case snd tip of
          ChainTipAtGenesis -> "Local node rolled back to genesis."
          ChainTip _ hash no ->
            fromString $
              printf
                "Local node switched to new tip %s (#%d)"
                (extractBase16 (encodeBase16 (serialiseToRawBytes hash)))
                (unBlockNo no)
        let remote = case fst tip of
              Origin -> 0
              At r -> unBlockNo r
        logInfo case clientTip of
          Origin -> "Rolled back to genesis"
          At (hash, BlockNo local) ->
            fromString $
              printf
                "Rolled back to block %s (#%d of %d) (%d%%)"
                (extractBase16 (encodeBase16 (serialiseToRawBytes hash)))
                local
                remote
                ((local * 100) `div` remote)
        atomically do
          Changes{..} <- readTVar changesVar
          let changesBlocks' = case point of
                ChainPointAtGenesis -> []
                ChainPoint slot _ -> dropWhile ((> slot) . blockSlot) changesBlocks
              newChanges =
                Changes
                  { changesBlocks = changesBlocks'
                  , changesRollback = case changesRollback of
                      -- If there was no previous rollback, and we still have blocks
                      -- in the batch after the rollback, we don't need to actually
                      -- process the rollback.
                      Nothing -> point <$ guard (null changesBlocks')
                      -- Otherwise, we need to process whichever rollback was to an
                      -- earlier point: the previous one, or this new one.
                      Just prevRollback -> Just $ minPoint point prevRollback
                  , changesTip = snd tip
                  , changesLocalTip = case (point, clientTip) of
                      (ChainPointAtGenesis, _) -> ChainTipAtGenesis
                      (_, Origin) -> ChainTipAtGenesis
                      (ChainPoint slotNo hash, At (_, blockNo)) -> ChainTip slotNo hash blockNo
                  , changesBlockCount = length changesBlocks'
                  , changesTxCount = sum $ blockTxCount <$> changesBlocks
                  }
          writeTVar changesVar newChanges
        let slotNoToBlockNo' = case point of
              ChainPointAtGenesis -> mempty
              ChainPoint (SlotNo s) _ ->
                IntMap.fromDistinctAscList $
                  reverse $
                    dropWhile ((s <) . fromIntegral . fst) $
                      IntMap.toDescList slotNoToBlockNo
        pure $ mkClientStIdle lastLog costModel maxCost changesVar slotNoToBlockNo' pipelineDecision n (snd <$> clientTip) tip
    }
  where
    minLogPeriod = secondsToNominalDiffTime 1

blockTxCount :: BlockInMode -> Int
blockTxCount (BlockInMode _ block) = case block of
  ByronBlock (C.ByronBlock (BL.ABOBBlock (BL.ABlock _ body _)) _ _) -> length . txpTxs $ bodyTxPayload body
  ByronBlock C.ByronBlock{} -> 0
  ShelleyBlock ShelleyBasedEraShelley (C.ShelleyBlock (C.Block _ txs) _) -> StrictSeq.length $ txSeqTxns txs
  ShelleyBlock ShelleyBasedEraAllegra (C.ShelleyBlock (C.Block _ txs) _) -> StrictSeq.length $ txSeqTxns txs
  ShelleyBlock ShelleyBasedEraMary (C.ShelleyBlock (C.Block _ txs) _) -> StrictSeq.length $ txSeqTxns txs
  ShelleyBlock ShelleyBasedEraAlonzo (C.ShelleyBlock (C.Block _ txs) _) -> StrictSeq.length $ A.txSeqTxns txs
  ShelleyBlock ShelleyBasedEraBabbage (C.ShelleyBlock (C.Block _ txs) _) -> StrictSeq.length $ A.txSeqTxns txs
  ShelleyBlock ShelleyBasedEraConway (C.ShelleyBlock (C.Block _ txs) _) -> StrictSeq.length $ A.txSeqTxns txs

minPoint :: ChainPoint -> ChainPoint -> ChainPoint
minPoint ChainPointAtGenesis _ = ChainPointAtGenesis
minPoint _ ChainPointAtGenesis = ChainPointAtGenesis
minPoint p1@(ChainPoint s1 _) p2@(ChainPoint s2 _)
  | s1 < s2 = p1
  | otherwise = p2

blockSlot :: BlockInMode -> SlotNo
blockSlot (BlockInMode _ (getBlockHeader -> (BlockHeader slot _ _))) = slot
