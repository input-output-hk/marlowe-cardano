{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TupleSections #-}

module Language.Marlowe.Runtime.ChainSync.NodeClient
  ( ChainSyncEvent(..)
  , runNodeClient
  ) where

import Cardano.Api (Block (..), BlockHeader (..), BlockInMode (..), BlockNo, CardanoMode, ChainPoint (..),
                    ChainSyncClientPipelined (..), ChainTip (..), LocalChainSyncClient (..),
                    LocalNodeClientProtocols (..), LocalNodeConnectInfo, chainPointToSlotNo, connectToLocalNode)
import Cardano.Api.ChainSync.ClientPipelined (ClientPipelinedStIdle (..), ClientPipelinedStIntersect (..),
                                              ClientStNext (..), MkPipelineDecision, N (Z), Nat (..),
                                              PipelineDecision (..), mapChainSyncClientPipelined,
                                              pipelineDecisionLowHighMark, runPipelineDecision)
import Control.Arrow ((&&&))
import Data.List (sortOn)
import Data.Ord (Down (Down))
import Ouroboros.Network.Point (WithOrigin (..))

type CardanoBlock = BlockInMode CardanoMode
type NumberedCardanoBlock = (BlockNo, CardanoBlock)
type NumberedChainTip = (WithOrigin BlockNo, ChainTip)

data ChainSyncEvent
  = RollForward CardanoBlock ChainTip
  | RollBackward ChainPoint ChainTip

runNodeClient
  :: LocalNodeConnectInfo CardanoMode
  -> (ChainPoint -> IO (WithOrigin BlockHeader))
  -> (ChainPoint -> Maybe ChainPoint -> IO [ChainPoint])
  -> (ChainSyncEvent -> IO ())
  -> IO ()
runNodeClient nodeConnectInfo getHeaderAtPoint getIntersectionPoints onEvent =
  connectToLocalNode nodeConnectInfo LocalNodeClientProtocols
    { localChainSyncClient = LocalChainSyncClientPipelined
        $ mapChainSyncClientPipelined id id (blockToBlockNo &&& id) (chainTipToBlockNo &&& id)
        $ pipelinedClient onEvent getBlockNoAtPoint getIntersectionPoints
    , localTxSubmissionClient = Nothing
    , localStateQueryClient   = Nothing
    }
  where
    blockHeaderToBlockNo :: BlockHeader -> BlockNo
    blockHeaderToBlockNo (BlockHeader _ _ blockNo) = blockNo

    blockToBlockNo :: CardanoBlock -> BlockNo
    blockToBlockNo (BlockInMode (Block header _) _) = blockHeaderToBlockNo header

    getBlockNoAtPoint :: ChainPoint -> IO (WithOrigin BlockNo)
    getBlockNoAtPoint = fmap (fmap blockHeaderToBlockNo) . getHeaderAtPoint

    chainTipToBlockNo :: ChainTip -> WithOrigin BlockNo
    chainTipToBlockNo = \case
      ChainTipAtGenesis    -> Origin
      ChainTip _ _ blockNo -> At blockNo


pipelinedClient
  :: forall m
   . Monad m
  => (ChainSyncEvent -> m ())
  -> (ChainPoint -> m (WithOrigin BlockNo))
  -> (ChainPoint -> Maybe ChainPoint -> m [ChainPoint])
  -> ChainSyncClientPipelined NumberedCardanoBlock ChainPoint NumberedChainTip m ()
pipelinedClient onEvent getBlockNoAtPoint getIntersectionPoints =
  ChainSyncClientPipelined $ intersect ChainPointAtGenesis Nothing
  where
    intersect
      :: ChainPoint
      -> Maybe ChainPoint
      -> m (ClientPipelinedStIdle 'Z NumberedCardanoBlock ChainPoint NumberedChainTip m ())
    intersect lowerBound upperBound = do
      points <- sortPoints <$> getIntersectionPoints lowerBound upperBound
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
      -> m (ClientPipelinedStIdle 'Z NumberedCardanoBlock ChainPoint NumberedChainTip m ())
    clientStIdle point nodeTip = do
      clientTip <- getBlockNoAtPoint point
      pure $ mkClientStIdle onEvent getBlockNoAtPoint pipelinePolicy Zero clientTip nodeTip

    pipelinePolicy :: MkPipelineDecision
    pipelinePolicy = pipelineDecisionLowHighMark 1 50

mkClientStIdle
  :: forall m n
   . Monad m
  => (ChainSyncEvent -> m ())
  -> (ChainPoint -> m (WithOrigin BlockNo))
  -> MkPipelineDecision
  -> Nat n
  -> WithOrigin BlockNo
  -> NumberedChainTip
  -> ClientPipelinedStIdle n NumberedCardanoBlock ChainPoint NumberedChainTip m ()
mkClientStIdle onEvent getBlockNoAtPoint pipelineDecision n clientTip nodeTip =
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
      $ mkClientStIdle onEvent getBlockNoAtPoint pipelineDecision' (Succ n) clientTip nodeTip

    collect
      :: forall n'
        . MkPipelineDecision
        -> Nat n'
        -> ClientStNext n' NumberedCardanoBlock ChainPoint NumberedChainTip m ()
    collect pipelineDecision' = mkClientStNext onEvent getBlockNoAtPoint
      . mkClientStIdle onEvent getBlockNoAtPoint pipelineDecision'

mkClientStNext
  :: Monad m
  => (ChainSyncEvent -> m ())
  -> (ChainPoint -> m (WithOrigin BlockNo))
  -> (WithOrigin BlockNo -> NumberedChainTip -> ClientPipelinedStIdle n NumberedCardanoBlock ChainPoint NumberedChainTip m ())
  -> ClientStNext n NumberedCardanoBlock ChainPoint NumberedChainTip m ()
mkClientStNext onEvent getBlockNoAtPoint next = ClientStNext
  { recvMsgRollForward = \(blockNo, block) tip -> do
      onEvent $ RollForward block $ snd tip
      let clientTip = At blockNo
      pure $ next clientTip tip
  , recvMsgRollBackward = \point tip -> do
      onEvent $ RollBackward point $ snd tip
      clientTip <- getBlockNoAtPoint point
      pure $ next clientTip tip
  }
