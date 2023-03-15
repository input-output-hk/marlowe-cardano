{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.ChainIndexer.NodeClient
  ( Changes(..)
  , CostModel(..)
  , NodeClient(..)
  , NodeClientDependencies(..)
  , NodeClientSelector(..)
  , RollBackwardField(..)
  , RollForwardField(..)
  , isEmptyChanges
  , nodeClient
  , toEmptyChanges
  ) where

import Cardano.Api
  ( Block(..)
  , BlockHeader(..)
  , BlockInMode(..)
  , BlockNo
  , CardanoMode
  , ChainPoint(..)
  , ChainSyncClientPipelined(..)
  , ChainTip(..)
  , LocalChainSyncClient(..)
  , LocalNodeClientProtocols(..)
  , LocalNodeClientProtocolsInMode
  , SlotNo(..)
  )
import Cardano.Api.ChainSync.ClientPipelined
  ( ClientPipelinedStIdle(..)
  , ClientPipelinedStIntersect(..)
  , ClientStNext(..)
  , MkPipelineDecision
  , N(..)
  , Nat(..)
  , PipelineDecision(..)
  , mapChainSyncClientPipelined
  , pipelineDecisionLowHighMark
  , runPipelineDecision
  )
import Control.Arrow ((&&&))
import Control.Concurrent.Component
import Control.Concurrent.STM (STM, TVar, atomically, modifyTVar, newTVar, readTVar, writeTVar)
import Control.Exception (SomeException, mask, throw, try)
import Control.Monad (guard)
import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap
import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import Data.Ord (Down(..))
import Data.Void (Void)
import Language.Marlowe.Runtime.ChainIndexer.Database (CardanoBlock, GetIntersectionPoints(..))
import Observe.Event.Backend (simpleNewEventArgs)
import Observe.Event.Explicit (EventBackend, addField, finalize, newEvent, withEvent)
import Ouroboros.Network.Point (WithOrigin(..))

type NumberedCardanoBlock = (BlockNo, CardanoBlock)
type NumberedChainTip = (WithOrigin BlockNo, ChainTip)

-- | Describes a batch of chain data changes to write.
data Changes = Changes
  { changesRollback   :: !(Maybe ChainPoint) -- ^ Point to rollback to before writing any blocks.
  , changesBlocks     :: ![CardanoBlock]     -- ^ New blocks to write.
  , changesTip        :: !ChainTip           -- ^ Most recently observed tip of the local node.
  , changesLocalTip   :: !ChainTip           -- ^ Chain tip the changes will advance the local state to.
  , changesBlockCount :: !Int                -- ^ Number of blocks in the change set.
  , changesTxCount    :: !Int                -- ^ Number of transactions in the change set.
  }

-- | An empty Changes collection.
emptyChanges :: Changes
emptyChanges = Changes Nothing [] ChainTipAtGenesis ChainTipAtGenesis 0 0

-- | Make a set of changes into an empty set (preserves the tip and point fields).
toEmptyChanges :: Changes -> Changes
toEmptyChanges changes = changes
  { changesRollback = Nothing
  , changesBlocks = []
  , changesBlockCount = 0
  , changesTxCount = 0
  }

-- | Returns True if the change set is empty.
isEmptyChanges :: Changes -> Bool
isEmptyChanges (Changes Nothing [] _ _ _ _) = True
isEmptyChanges _                            = False

-- | Parameters for estimating the cost of writing a batch of changes.
data CostModel = CostModel
  { blockCost :: Int
  , txCost    :: Int
  } deriving (Show, Eq)

-- | Computes the cost of a change set. The value is a unitless heuristic.
--   Prevents large numbers of transactions and blocks being held in memory.
cost :: CostModel -> Changes -> Int
cost CostModel{..} Changes{..} = changesBlockCount * blockCost + changesTxCount * txCost

-- | The set of dependencies needed by the NodeClient component.
data NodeClientDependencies r = NodeClientDependencies
  { connectToLocalNode    :: !(LocalNodeClientProtocolsInMode CardanoMode -> IO ()) -- ^ Connect to the local node.
  , getIntersectionPoints :: !(GetIntersectionPoints IO)                            -- ^ How to load the set of initial intersection points for the chain sync client.
  -- | The maximum cost a set of changes is allowed to incur before the
  -- NodeClient blocks.
  , maxCost               :: Int
  , costModel             :: CostModel
  , eventBackend          :: !(EventBackend IO r NodeClientSelector)
  }

-- | The public API of the NodeClient component.
data NodeClient = NodeClient
  { getChanges    :: STM Changes -- ^ An STM action that atomically reads and clears the current change set.
  , connected :: STM Bool
  }

data NodeClientSelector f where
  Connect :: NodeClientSelector Void
  Intersect :: NodeClientSelector [ChainPoint]
  IntersectFound :: NodeClientSelector ChainPoint
  IntersectNotFound :: NodeClientSelector Void
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
nodeClient :: Component IO (NodeClientDependencies r) NodeClient
nodeClient = component \NodeClientDependencies{..} -> do
  changesVar <- newTVar emptyChanges
  connectedVar <- newTVar False

  let
    getChanges :: STM Changes
    getChanges = do
      changes <- readTVar changesVar
      modifyTVar changesVar toEmptyChanges
      pure changes

    pipelinedClient' :: ChainSyncClientPipelined CardanoBlock ChainPoint ChainTip IO ()
    pipelinedClient' = mapChainSyncClientPipelined id id (blockToBlockNo &&& id) (chainTipToBlockNo &&& id)
        $ pipelinedClient eventBackend costModel maxCost changesVar getIntersectionPoints

    runNodeClient :: IO ()
    runNodeClient = mask \restore -> do
      ev <- newEvent eventBackend $ simpleNewEventArgs Connect
      result <- try @SomeException $ restore $ connectToLocalNode LocalNodeClientProtocols
        { localChainSyncClient =
            let ChainSyncClientPipelined client = pipelinedClient'
              in LocalChainSyncClientPipelined $ ChainSyncClientPipelined do
                finalize ev Nothing
                atomically $ writeTVar connectedVar True
                client
        , localTxSubmissionClient = Nothing
        , localTxMonitoringClient = Nothing
        , localStateQueryClient   = Nothing
        }
      atomically $ writeTVar connectedVar False
      case result of
        Left ex -> finalize ev (Just ex) *> throw ex
        Right _ -> pure ()

    connected = readTVar connectedVar

  pure (runNodeClient, NodeClient { getChanges, connected })

blockHeaderToBlockNo :: BlockHeader -> BlockNo
blockHeaderToBlockNo (BlockHeader _ _ blockNo) = blockNo

blockToBlockNo :: CardanoBlock -> BlockNo
blockToBlockNo (BlockInMode (Block header _) _) = blockHeaderToBlockNo header

chainTipToBlockNo :: ChainTip -> WithOrigin BlockNo
chainTipToBlockNo = \case
  ChainTipAtGenesis    -> Origin
  ChainTip _ _ blockNo -> At blockNo

pipelinedClient
  :: EventBackend IO r NodeClientSelector
  -> CostModel
  -> Int
  -> TVar Changes
  -> GetIntersectionPoints IO
  -> ChainSyncClientPipelined NumberedCardanoBlock ChainPoint NumberedChainTip IO ()
pipelinedClient eventBackend costModel maxCost changesVar getIntersectionPoints =
  ChainSyncClientPipelined do
    headers <- withEvent eventBackend Intersect \ev -> do
      headers <- sortOn (Down . fmap blockHeaderToBlockNo) <$> runGetIntersectionPoints getIntersectionPoints
      addField ev $ headerToPoint <$> headers
      pure headers
    pure $ SendMsgFindIntersect (headerToPoint <$> headers) ClientPipelinedStIntersect
      { recvMsgIntersectFound = \point tip -> withEvent eventBackend IntersectFound \ev -> do
          let
            getSlotAndBlock = case point of
              ChainPointAtGenesis -> const Nothing
              ChainPoint pointSlot _ -> \case
                Origin -> Nothing
                At (BlockHeader (SlotNo s) _ b)
                  | SlotNo s <= pointSlot -> Just (fromIntegral s, b)
                  | otherwise -> Nothing
            slotNoToBlockNo = IntMap.fromList $ mapMaybe getSlotAndBlock headers
          addField ev point
          clientStIdle slotNoToBlockNo point tip
      , recvMsgIntersectNotFound = withEvent eventBackend IntersectNotFound . const . clientStIdle mempty ChainPointAtGenesis
      }
  where
    clientStIdle
      :: IntMap BlockNo
      -> ChainPoint
      -> NumberedChainTip
      -> IO (ClientPipelinedStIdle 'Z NumberedCardanoBlock ChainPoint NumberedChainTip IO ())
    clientStIdle slotNoToBlockNo point nodeTip = do
      let
        clientTip = case point of
          ChainPointAtGenesis -> Origin
          ChainPoint (SlotNo s) _ -> case IntMap.lookup (fromIntegral s) slotNoToBlockNo of
            Nothing -> error $ "Unable to find block number for chain point " <> show point
            Just b -> At b
      pure $ mkClientStIdle eventBackend costModel maxCost changesVar slotNoToBlockNo pipelinePolicy Zero clientTip nodeTip

    headerToPoint Origin = ChainPointAtGenesis
    headerToPoint (At (BlockHeader s h _)) = ChainPoint s h

    -- How to pipeline. If we have fewer than 50 requests in flight, send
    -- another request. When we hit 50, start collecting responses until we
    -- have 1 request in flight, then repeat. If we are caught up to tip,
    -- requests will not be pipelined.
    pipelinePolicy :: MkPipelineDecision
    pipelinePolicy = pipelineDecisionLowHighMark 1 50

mkClientStIdle
  :: forall r n
   . EventBackend IO r NodeClientSelector
  -> CostModel
  -> Int
  -> TVar Changes
  -> IntMap BlockNo
  -> MkPipelineDecision
  -> Nat n
  -> WithOrigin BlockNo
  -> NumberedChainTip
  -> ClientPipelinedStIdle n NumberedCardanoBlock ChainPoint NumberedChainTip IO ()
mkClientStIdle eventBackend costModel maxCost changesVar slotNoToBlockNo pipelineDecision n clientTip nodeTip =
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
      -> ClientPipelinedStIdle n NumberedCardanoBlock ChainPoint NumberedChainTip IO ()
    nextPipelineRequest pipelineDecision' = SendMsgRequestNextPipelined
      $ mkClientStIdle eventBackend costModel maxCost changesVar slotNoToBlockNo pipelineDecision' (Succ n) clientTip nodeTip

    collect
      :: forall n'
        . MkPipelineDecision
        -> Nat n'
        -> ClientStNext n' NumberedCardanoBlock ChainPoint NumberedChainTip IO ()
    collect pipelineDecision' = mkClientStNext eventBackend costModel maxCost changesVar slotNoToBlockNo pipelineDecision'

mkClientStNext
  :: EventBackend IO r NodeClientSelector
  -> CostModel
  -> Int
  -> TVar Changes
  -> IntMap BlockNo
  -> MkPipelineDecision
  -> Nat n
  -> ClientStNext n NumberedCardanoBlock ChainPoint NumberedChainTip IO ()
mkClientStNext eventBackend costModel maxCost changesVar slotNoToBlockNo pipelineDecision n = ClientStNext
  { recvMsgRollForward = \(blockNo, block@(BlockInMode (Block header@(BlockHeader slotNo hash _) txs) _)) tip -> withEvent eventBackend RollForward \ev -> do
      addField ev $ RollForwardBlock header
      addField ev $ RollForwardTip $ snd tip
      nextChanges <- atomically do
        changes <- readTVar changesVar
        let
          nextChanges = changes
            { changesBlocks = block : changesBlocks changes
            , changesTip = snd tip
            , changesLocalTip = ChainTip slotNo hash blockNo
            , changesBlockCount = changesBlockCount changes + 1
            , changesTxCount = changesTxCount changes + length txs
            }
        -- Retry unless either the current change set is empty, or the next
        -- change set would not be too expensive.
        guard $ isEmptyChanges changes || cost costModel nextChanges <= maxCost
        writeTVar changesVar nextChanges
        pure nextChanges
      addField ev $ RollForwardNewCost $ cost costModel nextChanges
      let clientTip = At blockNo
      let slotNoToInt (SlotNo s) = fromIntegral s
      let slotNoToBlockNo' = IntMap.insert (slotNoToInt slotNo) blockNo slotNoToBlockNo
      pure $ mkClientStIdle eventBackend costModel maxCost changesVar slotNoToBlockNo' pipelineDecision n clientTip tip
  , recvMsgRollBackward = \point tip -> withEvent eventBackend RollBackward \ev -> do
      addField ev $ RollBackwardPoint point
      addField ev $ RollBackwardTip $ snd tip
      let
        clientTip = case point of
          ChainPointAtGenesis -> Origin
          ChainPoint (SlotNo s) _ -> case IntMap.lookup (fromIntegral s) slotNoToBlockNo of
            Nothing -> error $ "Unable to find block number for chain point " <> show point
            Just b -> At b
      newChanges <- atomically do
        Changes{..} <- readTVar changesVar
        let
          changesBlocks' = case point of
            ChainPointAtGenesis -> []
            ChainPoint slot _   -> dropWhile ((> slot) . blockSlot) changesBlocks
          blockTxCount (BlockInMode (Block _ txs) _) = length txs
          newChanges = Changes
            { changesBlocks = changesBlocks'
            , changesRollback = case changesRollback of
                -- If there was no previous rollback, and we still have blocks
                -- in the batch after the rollback, we don't need to actually
                -- process the rollback.
                Nothing           -> point <$ guard (null changesBlocks')
                -- Otherwise, we need to process whichever rollback was to an
                -- earlier point: the previous one, or this new one.
                Just prevRollback -> Just $ minPoint point prevRollback
            , changesTip = snd tip
            , changesLocalTip = case (point, clientTip) of
                (ChainPointAtGenesis, _)             -> ChainTipAtGenesis
                (_, Origin)                          -> ChainTipAtGenesis
                (ChainPoint slotNo hash, At blockNo) -> ChainTip slotNo hash blockNo
            , changesBlockCount = length changesBlocks'
            , changesTxCount = sum $ blockTxCount <$> changesBlocks
            }
        writeTVar changesVar newChanges
        pure newChanges
      addField ev $ RollBackwardNewCost $ cost costModel newChanges
      let
        slotNoToBlockNo' = case point of
          ChainPointAtGenesis -> mempty
          ChainPoint (SlotNo s) _ -> IntMap.fromDistinctAscList
            $ reverse
            $ dropWhile ((s <) . fromIntegral . fst)
            $ IntMap.toDescList slotNoToBlockNo
      pure $ mkClientStIdle eventBackend costModel maxCost changesVar slotNoToBlockNo' pipelineDecision n clientTip tip
  }

minPoint :: ChainPoint -> ChainPoint -> ChainPoint
minPoint ChainPointAtGenesis _ = ChainPointAtGenesis
minPoint _ ChainPointAtGenesis = ChainPointAtGenesis
minPoint p1@(ChainPoint s1 _) p2@(ChainPoint s2 _)
  | s1 < s2 = p1
  | otherwise = p2

blockSlot :: CardanoBlock -> SlotNo
blockSlot (BlockInMode (Block (BlockHeader slot _ _) _) _) = slot
