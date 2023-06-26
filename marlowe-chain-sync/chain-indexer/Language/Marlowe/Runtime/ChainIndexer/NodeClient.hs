{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.ChainIndexer.NodeClient (
  Changes (..),
  CostModel (..),
  NodeClient (..),
  NodeClientDependencies (..),
  NodeClientSelector (..),
  RollBackwardField (..),
  RollForwardField (..),
  isEmptyChanges,
  nodeClient,
  toEmptyChanges,
) where

import Cardano.Api (
  Block (..),
  BlockHeader (..),
  BlockInMode (..),
  BlockNo,
  CardanoMode,
  ChainPoint (..),
  ChainSyncClientPipelined (..),
  ChainTip (..),
  LocalChainSyncClient (..),
  LocalNodeClientProtocols (..),
  LocalNodeClientProtocolsInMode,
  SlotNo (..),
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
import Colog (Message, WithLog)
import Control.Arrow ((&&&))
import Control.Concurrent.Component
import Control.Concurrent.STM (STM, TVar, modifyTVar, newTVar, readTVar, writeTVar)
import Control.Monad (guard)
import Control.Monad.Event.Class
import Data.List (sortOn)
import Data.Ord (Down (..))
import Language.Marlowe.Runtime.ChainIndexer.Database (CardanoBlock, GetIntersectionPoints (..))
import Observe.Event (NewEventArgs (..), addField, reference)
import Observe.Event.Backend (setAncestorEventBackend)
import Ouroboros.Network.Point (WithOrigin (..))
import UnliftIO (MonadIO, MonadUnliftIO, atomically, withRunInIO)

type NumberedCardanoBlock = (BlockNo, CardanoBlock)
type NumberedChainTip = (WithOrigin BlockNo, ChainTip)

-- | Describes a batch of chain data changes to write.
data Changes r = Changes
  { changesRollback :: !(Maybe ChainPoint)
  -- ^ Point to rollback to before writing any blocks.
  , changesBlocks :: ![CardanoBlock]
  -- ^ New blocks to write.
  , changesTip :: !ChainTip
  -- ^ Most recently observed tip of the local node.
  , changesLocalTip :: !ChainTip
  -- ^ Chain tip the changes will advance the local state to.
  , changesBlockCount :: !Int
  -- ^ Number of blocks in the change set.
  , changesTxCount :: !Int
  -- ^ Number of transactions in the change set.
  , changesEvents :: ![r]
  -- ^ References to the events that have touched these changes
  }

-- | An empty Changes collection.
emptyChanges :: Changes r
emptyChanges = Changes Nothing [] ChainTipAtGenesis ChainTipAtGenesis 0 0 []

-- | Make a set of changes into an empty set (preserves the tip and point fields).
toEmptyChanges :: Changes r -> Changes r
toEmptyChanges changes =
  changes
    { changesRollback = Nothing
    , changesBlocks = []
    , changesBlockCount = 0
    , changesTxCount = 0
    , changesEvents = []
    }

-- | Returns True if the change set is empty.
isEmptyChanges :: Changes r -> Bool
isEmptyChanges (Changes Nothing [] _ _ _ _ _) = True
isEmptyChanges _ = False

-- | Parameters for estimating the cost of writing a batch of changes.
data CostModel = CostModel
  { blockCost :: Int
  , txCost :: Int
  }
  deriving (Show, Eq)

-- | Computes the cost of a change set. The value is a unitless heuristic.
--   Prevents large numbers of transactions and blocks being held in memory.
cost :: CostModel -> Changes r -> Int
cost CostModel{..} Changes{..} = changesBlockCount * blockCost + changesTxCount * txCost

-- | The set of dependencies needed by the NodeClient component.
data NodeClientDependencies r m = NodeClientDependencies
  { connectToLocalNode :: !((r -> LocalNodeClientProtocolsInMode CardanoMode) -> m ())
  -- ^ Connect to the local node.
  , getIntersectionPoints :: !(GetIntersectionPoints m)
  -- ^ How to load the set of initial intersection points for the chain sync client.
  -- | The maximum cost a set of changes is allowed to incur before the
  -- NodeClient blocks.
  , maxCost :: Int
  , costModel :: CostModel
  }

-- | The public API of the NodeClient component.
data NodeClient r = NodeClient
  { getChanges :: STM (Changes r)
  -- ^ An STM action that atomically reads and clears the current change set.
  , connected :: STM Bool
  }

data NodeClientSelector f where
  Intersect :: NodeClientSelector [ChainPoint]
  IntersectFound :: NodeClientSelector (ChainPoint, ChainTip)
  IntersectNotFound :: NodeClientSelector ChainTip
  RollForward :: NodeClientSelector RollForwardField
  RollBackward :: NodeClientSelector RollBackwardField

data RollForwardField
  = RollForwardBlock BlockHeader
  | RollForwardTip ChainTip
  | RollForwardNewCost Int

data RollBackwardField
  = RollBackwardPoint ChainPoint
  | RollBackwardTip ChainTip
  | RollBackwardNewCost Int

-- | Create a new NodeClient component.
nodeClient
  :: forall r s env m
   . (MonadInjectEvent r NodeClientSelector s m, MonadUnliftIO m, WithLog env Message m)
  => Component m (NodeClientDependencies r m) (NodeClient r)
nodeClient = component "indexer-node-client" \NodeClientDependencies{..} -> do
  changesVar <- newTVar emptyChanges
  connectedVar <- newTVar False

  let getChanges :: STM (Changes r)
      getChanges = do
        changes <- readTVar changesVar
        modifyTVar changesVar toEmptyChanges
        pure changes

      pipelinedClient' :: ChainSyncClientPipelined CardanoBlock ChainPoint ChainTip m ()
      pipelinedClient' =
        mapChainSyncClientPipelined id id (blockToBlockNo &&& id) (chainTipToBlockNo &&& id) $
          pipelinedClient costModel maxCost changesVar getIntersectionPoints

      runNodeClient :: m ()
      runNodeClient = withRunInIO \runInIO -> runInIO $ connectToLocalNode \r ->
        LocalNodeClientProtocols
          { localChainSyncClient =
              let ChainSyncClientPipelined client = pipelinedClient'
               in LocalChainSyncClientPipelined $ hoistClient (runInIO . localBackend (setAncestorEventBackend r)) $ ChainSyncClientPipelined do
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

blockToBlockNo :: CardanoBlock -> BlockNo
blockToBlockNo (BlockInMode (Block header _) _) = blockHeaderToBlockNo header

chainTipToBlockNo :: ChainTip -> WithOrigin BlockNo
chainTipToBlockNo = \case
  ChainTipAtGenesis -> Origin
  ChainTip _ _ blockNo -> At blockNo

pipelinedClient
  :: forall r s m
   . (MonadInjectEvent r NodeClientSelector s m, MonadIO m)
  => CostModel
  -> Int
  -> TVar (Changes r)
  -> GetIntersectionPoints m
  -> ChainSyncClientPipelined NumberedCardanoBlock ChainPoint NumberedChainTip m ()
pipelinedClient costModel maxCost changesVar getIntersectionPoints =
  ChainSyncClientPipelined do
    (r, headers) <- withEvent Intersect \ev -> do
      headers <- sortOn (Down . fmap blockHeaderToBlockNo) <$> runGetIntersectionPoints getIntersectionPoints
      addField ev $ headerToPoint <$> headers
      pure (reference ev, headers)
    pure $
      SendMsgFindIntersect
        (headerToPoint <$> headers)
        ClientPipelinedStIntersect
          { recvMsgIntersectFound = \point tip -> do
              emitImmediateEventArgs_
                NewEventArgs
                  { newEventSelector = IntersectFound
                  , newEventParent = Nothing
                  , newEventCauses = [r]
                  , newEventInitialFields = [(point, snd tip)]
                  }
              clientStIdle tip
          , recvMsgIntersectNotFound = \tip -> do
              emitImmediateEventArgs_
                NewEventArgs
                  { newEventSelector = IntersectNotFound
                  , newEventParent = Nothing
                  , newEventCauses = [r]
                  , newEventInitialFields = [snd tip]
                  }
              clientStIdle tip
          }
  where
    clientStIdle
      :: NumberedChainTip
      -> m (ClientPipelinedStIdle 'Z NumberedCardanoBlock ChainPoint NumberedChainTip m ())
    clientStIdle nodeTip = do
      pure $ mkClientStIdle costModel maxCost changesVar pipelinePolicy Zero Origin nodeTip

    headerToPoint Origin = ChainPointAtGenesis
    headerToPoint (At (BlockHeader s h _)) = ChainPoint s h

    -- How to pipeline. If we have fewer than 50 requests in flight, send
    -- another request. When we hit 50, start collecting responses until we
    -- have 1 request in flight, then repeat. If we are caught up to tip,
    -- requests will not be pipelined.
    pipelinePolicy :: MkPipelineDecision
    pipelinePolicy = pipelineDecisionLowHighMark 1 50

mkClientStIdle
  :: forall r s m n
   . (MonadInjectEvent r NodeClientSelector s m, MonadIO m)
  => CostModel
  -> Int
  -> TVar (Changes r)
  -> MkPipelineDecision
  -> Nat n
  -> WithOrigin BlockNo
  -> NumberedChainTip
  -> ClientPipelinedStIdle n NumberedCardanoBlock ChainPoint NumberedChainTip m ()
mkClientStIdle costModel maxCost changesVar pipelineDecision n clientTip nodeTip =
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
        mkClientStIdle costModel maxCost changesVar pipelineDecision' (Succ n) clientTip nodeTip

    collect
      :: forall n'
       . MkPipelineDecision
      -> Nat n'
      -> ClientStNext n' NumberedCardanoBlock ChainPoint NumberedChainTip m ()
    collect pipelineDecision' = mkClientStNext costModel maxCost changesVar pipelineDecision'

mkClientStNext
  :: (MonadIO m, MonadInjectEvent r NodeClientSelector s m)
  => CostModel
  -> Int
  -> TVar (Changes r)
  -> MkPipelineDecision
  -> Nat n
  -> ClientStNext n NumberedCardanoBlock ChainPoint NumberedChainTip m ()
mkClientStNext costModel maxCost changesVar pipelineDecision n =
  ClientStNext
    { recvMsgRollForward = \(blockNo, block@(BlockInMode (Block header@(BlockHeader slotNo hash _) txs) _)) tip -> withEvent RollForward \ev -> do
        addField ev $ RollForwardBlock header
        addField ev $ RollForwardTip $ snd tip
        nextChanges <- atomically do
          changes <- readTVar changesVar
          let nextChanges =
                changes
                  { changesBlocks = block : changesBlocks changes
                  , changesTip = snd tip
                  , changesLocalTip = ChainTip slotNo hash blockNo
                  , changesBlockCount = changesBlockCount changes + 1
                  , changesTxCount = changesTxCount changes + length txs
                  , changesEvents = reference ev : changesEvents changes
                  }
          -- Retry unless either the current change set is empty, or the next
          -- change set would not be too expensive.
          guard $ isEmptyChanges changes || cost costModel nextChanges <= maxCost
          writeTVar changesVar nextChanges
          pure nextChanges
        addField ev $ RollForwardNewCost $ cost costModel nextChanges
        let clientTip = At blockNo
        pure $ mkClientStIdle costModel maxCost changesVar pipelineDecision n clientTip tip
    , recvMsgRollBackward = \point tip -> withEvent RollBackward \ev -> do
        addField ev $ RollBackwardPoint point
        addField ev $ RollBackwardTip $ snd tip
        newChanges <- atomically do
          Changes{..} <- readTVar changesVar
          let changesBlocks' = case point of
                ChainPointAtGenesis -> []
                ChainPoint slot _ -> dropWhile ((> slot) . blockSlot) changesBlocks
              blockTxCount (BlockInMode (Block _ txs) _) = length txs
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
                  , changesLocalTip = case point of
                      ChainPointAtGenesis -> ChainTipAtGenesis
                      ChainPoint slotNo hash -> ChainTip slotNo hash 0
                  , changesBlockCount = length changesBlocks'
                  , changesTxCount = sum $ blockTxCount <$> changesBlocks
                  , changesEvents = reference ev : changesEvents
                  }
          writeTVar changesVar newChanges
          pure newChanges
        addField ev $ RollBackwardNewCost $ cost costModel newChanges
        pure $ mkClientStIdle costModel maxCost changesVar pipelineDecision n (fst tip) tip
    }

minPoint :: ChainPoint -> ChainPoint -> ChainPoint
minPoint ChainPointAtGenesis _ = ChainPointAtGenesis
minPoint _ ChainPointAtGenesis = ChainPointAtGenesis
minPoint p1@(ChainPoint s1 _) p2@(ChainPoint s2 _)
  | s1 < s2 = p1
  | otherwise = p2

blockSlot :: CardanoBlock -> SlotNo
blockSlot (BlockInMode (Block (BlockHeader slot _ _) _) _) = slot
