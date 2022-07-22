{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TupleSections #-}

module Language.Marlowe.Runtime.ChainSync.NodeClient
  ( Changes(..)
  , NodeClientDependencies(..)
  , NodeClient(..)
  , isEmptyChanges
  , mkNodeClient
  ) where

import Cardano.Api (Block (..), BlockHeader (..), BlockInMode (..), BlockNo, CardanoMode, ChainPoint (..),
                    ChainSyncClientPipelined (..), ChainTip (..), LocalChainSyncClient (..),
                    LocalNodeClientProtocols (..), LocalNodeConnectInfo, SlotNo, chainPointToSlotNo, connectToLocalNode)
import Cardano.Api.ChainSync.ClientPipelined (ClientPipelinedStIdle (..), ClientPipelinedStIntersect (..),
                                              ClientStNext (..), MkPipelineDecision, N (..), Nat (..),
                                              PipelineDecision (..), mapChainSyncClientPipelined,
                                              pipelineDecisionLowHighMark, runPipelineDecision)
import Control.Arrow ((&&&))
import Control.Concurrent.STM (STM, TVar, atomically, modifyTVar, newTVar, readTVar, writeTVar)
import Control.Exception (finally)
import Control.Monad (guard)
import Data.List (sortOn)
import Data.Ord (Down (..))
import Language.Marlowe.Runtime.ChainSync.Database (CardanoBlock, GetHeaderAtPoint (..), GetIntersectionPoints (..))
import Ouroboros.Network.Point (WithOrigin (..))

type NumberedCardanoBlock = (BlockNo, CardanoBlock)
type NumberedChainTip = (WithOrigin BlockNo, ChainTip)

data Changes = Changes
  { changesRollback :: !(Maybe ChainPoint)
  , changesBlocks   :: ![CardanoBlock]
  , changesTip      :: !ChainTip
  }

emptyChanges :: Changes
emptyChanges = Changes Nothing [] ChainTipAtGenesis

toEmptyChanges :: Changes -> Changes
toEmptyChanges changes = changes { changesRollback = Nothing, changesBlocks = [] }

isEmptyChanges :: Changes -> Bool
isEmptyChanges (Changes Nothing [] _) = True
isEmptyChanges _                      = False

maxBatchSize :: Int
maxBatchSize = 50_000

isMaxBatchSize :: Changes -> Bool
isMaxBatchSize Changes{..} = length changesBlocks >= maxBatchSize

data NodeClientDependencies = NodeClientDependencies
  { localNodeConnectInfo  :: !(LocalNodeConnectInfo CardanoMode)
  , getHeaderAtPoint      :: !(GetHeaderAtPoint IO)
  , getIntersectionPoints :: !(GetIntersectionPoints IO)
  }

data NodeClient = NodeClient
  { runNodeClient :: !(IO ())
  , getChanges    :: !(STM Changes)
  , clearChanges  :: !(STM ())
  }

mkNodeClient :: NodeClientDependencies -> STM NodeClient
mkNodeClient NodeClientDependencies{..} = do
  changesVar <- newTVar emptyChanges

  let
    clearChanges :: STM ()
    clearChanges = modifyTVar changesVar toEmptyChanges

    getChanges :: STM Changes
    getChanges = readTVar changesVar

    pipelinedClient' :: ChainSyncClientPipelined CardanoBlock ChainPoint ChainTip IO ()
    pipelinedClient' = mapChainSyncClientPipelined id id (blockToBlockNo &&& id) (chainTipToBlockNo &&& id)
        $ pipelinedClient changesVar getHeaderAtPoint getIntersectionPoints

    runNodeClient :: IO ()
    runNodeClient = connectToLocalNode localNodeConnectInfo LocalNodeClientProtocols
      { localChainSyncClient = LocalChainSyncClientPipelined pipelinedClient'
      , localTxSubmissionClient = Nothing
      , localTxMonitoringClient = Nothing
      , localStateQueryClient   = Nothing
      }

  pure NodeClient
    { runNodeClient = finally runNodeClient (atomically clearChanges)
    , getChanges
    , clearChanges
    }

blockHeaderToBlockNo :: BlockHeader -> BlockNo
blockHeaderToBlockNo (BlockHeader _ _ blockNo) = blockNo

blockToBlockNo :: CardanoBlock -> BlockNo
blockToBlockNo (BlockInMode (Block header _) _) = blockHeaderToBlockNo header

chainTipToBlockNo :: ChainTip -> WithOrigin BlockNo
chainTipToBlockNo = \case
  ChainTipAtGenesis    -> Origin
  ChainTip _ _ blockNo -> At blockNo

pipelinedClient
  :: TVar Changes
  -> GetHeaderAtPoint IO
  -> GetIntersectionPoints IO
  -> ChainSyncClientPipelined NumberedCardanoBlock ChainPoint NumberedChainTip IO ()
pipelinedClient changesVar getHeaderAtPoint getIntersectionPoints =
  ChainSyncClientPipelined $ intersect ChainPointAtGenesis Nothing
  where
    intersect
      :: ChainPoint
      -> Maybe ChainPoint
      -> IO (ClientPipelinedStIdle 'Z NumberedCardanoBlock ChainPoint NumberedChainTip IO ())
    intersect lowerBound upperBound = do
      points <- sortPoints <$> runGetIntersectionPoints getIntersectionPoints lowerBound upperBound
      pure $ SendMsgFindIntersect points ClientPipelinedStIntersect
        { recvMsgIntersectFound = \point tip -> case refineBounds point points of
            Nothing                         -> clientStIdle point tip
            Just (lowerBound', upperBound') -> intersect lowerBound' $ Just upperBound'
        , recvMsgIntersectNotFound = clientStIdle ChainPointAtGenesis
        }

    sortPoints :: [ChainPoint] -> [ChainPoint]
    sortPoints = sortOn $ Down . chainPointToSlotNo

    -- INVARIANT: points are sorted in descenting order by slot
    refineBounds :: ChainPoint -> [ChainPoint] -> Maybe (ChainPoint, ChainPoint)
    refineBounds found points = (found,) <$> case takeWhile (/= found) points of
      []      -> Nothing
      points' -> Just $ last points'

    clientStIdle
      :: ChainPoint
      -> NumberedChainTip
      -> IO (ClientPipelinedStIdle 'Z NumberedCardanoBlock ChainPoint NumberedChainTip IO ())
    clientStIdle point nodeTip = do
      clientTip <- fmap blockHeaderToBlockNo <$> runGetHeaderAtPoint getHeaderAtPoint point
      pure $ mkClientStIdle changesVar getHeaderAtPoint pipelinePolicy Zero clientTip nodeTip

    pipelinePolicy :: MkPipelineDecision
    pipelinePolicy = pipelineDecisionLowHighMark 1 50

mkClientStIdle
  :: forall n
   . TVar Changes
  -> GetHeaderAtPoint IO
  -> MkPipelineDecision
  -> Nat n
  -> WithOrigin BlockNo
  -> NumberedChainTip
  -> ClientPipelinedStIdle n NumberedCardanoBlock ChainPoint NumberedChainTip IO ()
mkClientStIdle changesVar getHeaderAtPoint pipelineDecision n clientTip nodeTip =
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
      $ mkClientStIdle changesVar getHeaderAtPoint pipelineDecision' (Succ n) clientTip nodeTip

    collect
      :: forall n'
        . MkPipelineDecision
        -> Nat n'
        -> ClientStNext n' NumberedCardanoBlock ChainPoint NumberedChainTip IO ()
    collect pipelineDecision' = mkClientStNext changesVar getHeaderAtPoint pipelineDecision'

mkClientStNext
  :: TVar Changes
  -> GetHeaderAtPoint IO
  -> MkPipelineDecision
  -> Nat n
  -> ClientStNext n NumberedCardanoBlock ChainPoint NumberedChainTip IO ()
mkClientStNext changesVar getHeaderAtPoint pipelineDecision n = ClientStNext
  { recvMsgRollForward = \(blockNo, block) tip -> do
      atomically do
        changes <- readTVar changesVar
        guard $ not $ isMaxBatchSize changes
        writeTVar changesVar changes
          { changesBlocks = block : changesBlocks changes
          , changesTip = snd tip
          }
      let clientTip = At blockNo
      pure $ mkClientStIdle changesVar getHeaderAtPoint pipelineDecision n clientTip tip
  , recvMsgRollBackward = \point tip -> do
      atomically $ modifyTVar changesVar \Changes{..} -> Changes
        { changesBlocks = case point of
            ChainPointAtGenesis -> []
            ChainPoint slot _   -> filter ((<= slot) . blockSlot) changesBlocks
        , changesRollback = Just $ maybe point (minPoint point) changesRollback
        , changesTip = snd tip
        }
      clientTip <- fmap blockHeaderToBlockNo <$> runGetHeaderAtPoint getHeaderAtPoint point
      pure $ mkClientStIdle changesVar getHeaderAtPoint pipelineDecision n clientTip tip
  }

minPoint :: ChainPoint -> ChainPoint -> ChainPoint
minPoint ChainPointAtGenesis _ = ChainPointAtGenesis
minPoint _ ChainPointAtGenesis = ChainPointAtGenesis
minPoint p1@(ChainPoint s1 _) p2@(ChainPoint s2 _)
  | s1 < s2 = p1
  | otherwise = p2

blockSlot :: CardanoBlock -> SlotNo
blockSlot (BlockInMode (Block (BlockHeader slot _ _) _) _) = slot
