{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Language.Marlowe.Runtime.ChainSync.NodeClient
  ( Changes(..)
  , CostModel(..)
  , NodeClient(..)
  , NodeClientDependencies(..)
  , isEmptyChanges
  , mkNodeClient
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
  , SlotNo
  , chainPointToSlotNo
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
import Control.Concurrent.STM (STM, TVar, atomically, modifyTVar, newTVar, readTVar, writeTVar)
import Control.Monad (guard)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.List (sortOn)
import Data.Ord (Down(..))
import Language.Marlowe.Runtime.ChainSync.Database (CardanoBlock, GetHeaderAtPoint(..), GetIntersectionPoints(..))
import Language.Marlowe.Runtime.Logging.Colog.LogIO (LogIO, askLogIOAction, runLogIO)
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
data NodeClientDependencies = NodeClientDependencies
  { connectToLocalNode    :: !(LocalNodeClientProtocolsInMode CardanoMode -> LogIO ()) -- ^ Connect to the local node.
  , getHeaderAtPoint      :: !(GetHeaderAtPoint LogIO)                                 -- ^ How to load a block header at a given point.
  , getIntersectionPoints :: !(GetIntersectionPoints LogIO)                            -- ^ How to load the set of initial intersection points for the chain sync client.
  -- | The maximum cost a set of changes is allowed to incur before the
  -- NodeClient blocks.
  , maxCost               :: Int
  , costModel             :: CostModel
  }

-- | The public API of the NodeClient component.
data NodeClient = NodeClient
  { runNodeClient :: !(LogIO ())       -- ^ Run the component in IO.
  , getChanges    :: !(STM Changes) -- ^ An STM action that atomically reads and clears the current change set.
  }

-- | Create a new NodeClient component.
mkNodeClient :: NodeClientDependencies -> STM NodeClient
mkNodeClient NodeClientDependencies{..} = do
  changesVar <- newTVar emptyChanges

  let
    getChanges :: STM Changes
    getChanges = do
      changes <- readTVar changesVar
      modifyTVar changesVar toEmptyChanges
      pure changes

    pipelinedClient' :: ChainSyncClientPipelined CardanoBlock ChainPoint ChainTip LogIO ()
    pipelinedClient' = mapChainSyncClientPipelined id id (blockToBlockNo &&& id) (chainTipToBlockNo &&& id)
        $ pipelinedClient costModel maxCost changesVar getHeaderAtPoint getIntersectionPoints

    runNodeClient :: LogIO ()
    runNodeClient = do
      logAction <- askLogIOAction
      let
        c = hoistChainSyncClientPipelined (runLogIO logAction) pipelinedClient'
      connectToLocalNode LocalNodeClientProtocols
        { localChainSyncClient = LocalChainSyncClientPipelined c
        , localTxSubmissionClient = Nothing
        , localTxMonitoringClient = Nothing
        , localStateQueryClient   = Nothing
        }

  pure NodeClient { runNodeClient, getChanges }

blockHeaderToBlockNo :: BlockHeader -> BlockNo
blockHeaderToBlockNo (BlockHeader _ _ blockNo) = blockNo

blockToBlockNo :: CardanoBlock -> BlockNo
blockToBlockNo (BlockInMode (Block header _) _) = blockHeaderToBlockNo header

chainTipToBlockNo :: ChainTip -> WithOrigin BlockNo
chainTipToBlockNo = \case
  ChainTipAtGenesis    -> Origin
  ChainTip _ _ blockNo -> At blockNo

pipelinedClient
  :: CostModel
  -> Int
  -> TVar Changes
  -> GetHeaderAtPoint LogIO
  -> GetIntersectionPoints LogIO
  -> ChainSyncClientPipelined NumberedCardanoBlock ChainPoint NumberedChainTip LogIO ()
pipelinedClient costModel maxCost changesVar getHeaderAtPoint getIntersectionPoints =
  ChainSyncClientPipelined do
    points <- sortPoints <$> runGetIntersectionPoints getIntersectionPoints
    pure $ SendMsgFindIntersect points ClientPipelinedStIntersect
      { recvMsgIntersectFound = clientStIdle
      , recvMsgIntersectNotFound = clientStIdle ChainPointAtGenesis
      }
  where
    sortPoints :: [ChainPoint] -> [ChainPoint]
    sortPoints = sortOn $ Down . chainPointToSlotNo

    clientStIdle
      :: ChainPoint
      -> NumberedChainTip
      -> LogIO (ClientPipelinedStIdle 'Z NumberedCardanoBlock ChainPoint NumberedChainTip LogIO ())
    clientStIdle point nodeTip = do
      clientTip <- fmap blockHeaderToBlockNo <$> runGetHeaderAtPoint getHeaderAtPoint point
      pure $ mkClientStIdle costModel maxCost changesVar getHeaderAtPoint pipelinePolicy Zero clientTip nodeTip

    -- How to pipeline. If we have fewer than 50 requests in flight, send
    -- another request. When we hit 50, start collecting responses until we
    -- have 1 request in flight, then repeat. If we are caught up to tip,
    -- requests will not be pipelined.
    pipelinePolicy :: MkPipelineDecision
    pipelinePolicy = pipelineDecisionLowHighMark 1 50

mkClientStIdle
  :: forall n m
   . MonadIO m
  => CostModel
  -> Int
  -> TVar Changes
  -> GetHeaderAtPoint m
  -> MkPipelineDecision
  -> Nat n
  -> WithOrigin BlockNo
  -> NumberedChainTip
  -> ClientPipelinedStIdle n NumberedCardanoBlock ChainPoint NumberedChainTip m ()
mkClientStIdle costModel maxCost changesVar getHeaderAtPoint pipelineDecision n clientTip nodeTip =
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
    nextPipelineRequest pipelineDecision' = SendMsgRequestNextPipelined
      $ mkClientStIdle costModel maxCost changesVar getHeaderAtPoint pipelineDecision' (Succ n) clientTip nodeTip

    collect
      :: forall n'
        . MkPipelineDecision
        -> Nat n'
        -> ClientStNext n' NumberedCardanoBlock ChainPoint NumberedChainTip m ()
    collect pipelineDecision' = mkClientStNext costModel maxCost changesVar getHeaderAtPoint pipelineDecision'

mkClientStNext
  :: forall m n
   . MonadIO m
  => CostModel
  -> Int
  -> TVar Changes
  -> GetHeaderAtPoint m
  -> MkPipelineDecision
  -> Nat n
  -> ClientStNext n NumberedCardanoBlock ChainPoint NumberedChainTip m ()
mkClientStNext costModel maxCost changesVar getHeaderAtPoint pipelineDecision n = ClientStNext
  { recvMsgRollForward = \(blockNo, block@(BlockInMode (Block (BlockHeader slotNo hash _) txs) _)) tip -> liftIO $ do
      atomically do
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
      let clientTip = At blockNo
      pure $ mkClientStIdle costModel maxCost changesVar getHeaderAtPoint pipelineDecision n clientTip tip
  , recvMsgRollBackward = \point tip -> do
      clientTip <- fmap blockHeaderToBlockNo <$> runGetHeaderAtPoint getHeaderAtPoint point
      liftIO $ atomically $ modifyTVar changesVar \Changes{..} ->
        let
          changesBlocks' = case point of
            ChainPointAtGenesis -> []
            ChainPoint slot _   -> dropWhile ((> slot) . blockSlot) changesBlocks
          blockTxCount (BlockInMode (Block _ txs) _) = length txs
        in
          Changes
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
      pure $ mkClientStIdle costModel maxCost changesVar getHeaderAtPoint pipelineDecision n clientTip tip
  }

minPoint :: ChainPoint -> ChainPoint -> ChainPoint
minPoint ChainPointAtGenesis _ = ChainPointAtGenesis
minPoint _ ChainPointAtGenesis = ChainPointAtGenesis
minPoint p1@(ChainPoint s1 _) p2@(ChainPoint s2 _)
  | s1 < s2 = p1
  | otherwise = p2

blockSlot :: CardanoBlock -> SlotNo
blockSlot (BlockInMode (Block (BlockHeader slot _ _) _) _) = slot

-- | Change the underlying monad with a natural transformation.
hoistChainSyncClientPipelined
  :: forall header point tip m m' a
   . Functor m
  => (forall x. m x -> m' x)
  -> ChainSyncClientPipelined header point tip m a
  -> ChainSyncClientPipelined header point tip m' a
hoistChainSyncClientPipelined f ChainSyncClientPipelined{..} =
  ChainSyncClientPipelined $ f $ hoistIdle <$> runChainSyncClientPipelined
  where
    hoistIdle :: forall n. ClientPipelinedStIdle n header point tip m a -> ClientPipelinedStIdle n header point tip m' a
    hoistIdle (SendMsgRequestNext next mNext) = SendMsgRequestNext (hoistNext next) (f $ hoistNext <$> mNext)
    hoistIdle (SendMsgRequestNextPipelined idle) = SendMsgRequestNextPipelined (hoistIdle idle)
    hoistIdle (SendMsgFindIntersect points intersect) = SendMsgFindIntersect points (hoistIntersect intersect)
    hoistIdle (CollectResponse mIdle next) = CollectResponse (f . fmap hoistIdle <$> mIdle) (hoistNext next)
    hoistIdle (SendMsgDone a) = SendMsgDone a

    hoistNext :: forall n. ClientStNext n header point tip m a -> ClientStNext n header point tip m' a
    hoistNext ClientStNext{..} = ClientStNext
      { recvMsgRollForward = \header tip -> f $ hoistIdle <$> recvMsgRollForward header tip
      , recvMsgRollBackward = \header tip -> f $ hoistIdle <$> recvMsgRollBackward header tip
      }

    hoistIntersect :: ClientPipelinedStIntersect header point tip m a -> ClientPipelinedStIntersect header point tip m' a
    hoistIntersect ClientPipelinedStIntersect{..} = ClientPipelinedStIntersect
      { recvMsgIntersectFound = \point tip -> f (hoistIdle <$> recvMsgIntersectFound point tip)
      , recvMsgIntersectNotFound = \tip -> f (hoistIdle <$> recvMsgIntersectNotFound tip)
      }

