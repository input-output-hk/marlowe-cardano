{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TupleSections #-}

module Language.Marlowe.Runtime.ChainSync.NodeClient
  ( ChainSyncEvent(..)
  , runNodeClient
  ) where

import Cardano.Api (Block (..), BlockHeader (..), BlockInMode (..), BlockNo, CardanoMode, ChainPoint (..),
                    ChainSyncClientPipelined (..), ChainTip (..), LocalChainSyncClient (..),
                    LocalNodeClientProtocols (..), LocalNodeConnectInfo, connectToLocalNode)
import Cardano.Api.ChainSync.ClientPipelined (ClientPipelinedStIdle (..), ClientPipelinedStIntersect (..),
                                              ClientStNext (..), MkPipelineDecision, N (Z), Nat (..),
                                              PipelineDecision (..), pipelineDecisionLowHighMark, runPipelineDecision)
import Ouroboros.Network.Point (WithOrigin (..))

type CardanoBlock = BlockInMode CardanoMode

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
    { localChainSyncClient = LocalChainSyncClientPipelined $ pipelinedClient onEvent getHeaderAtPoint getIntersectionPoints
    , localTxSubmissionClient = Nothing
    , localStateQueryClient   = Nothing
    }

pipelinedClient
  :: (ChainSyncEvent -> IO ())
  -> (ChainPoint -> IO (WithOrigin BlockHeader))
  -> (ChainPoint -> Maybe ChainPoint -> IO [ChainPoint])
  -> ChainSyncClientPipelined CardanoBlock ChainPoint ChainTip IO ()
pipelinedClient onEvent getHeaderAtPoint getIntersectionPoints =
  ChainSyncClientPipelined $ intersect ChainPointAtGenesis Nothing
  where
    intersect :: ChainPoint -> Maybe ChainPoint -> IO (ClientPipelinedStIdle 'Z CardanoBlock ChainPoint ChainTip IO ())
    intersect lowerBound upperBound = do
      points <- getIntersectionPoints lowerBound upperBound
      pure $ SendMsgFindIntersect points ClientPipelinedStIntersect
        { recvMsgIntersectFound = \point tip -> case refineBounds point points of
            Nothing                         -> clientStIdle point tip
            Just (lowerBound', upperBound') -> intersect lowerBound' $ Just upperBound'
        , recvMsgIntersectNotFound = clientStIdle ChainPointAtGenesis
        }

    refineBounds :: ChainPoint -> [ChainPoint] -> Maybe (ChainPoint, ChainPoint)
    refineBounds found points = (found,) <$> case takeWhile (/= found) points of
      []      -> Nothing
      points' -> Just $ last points'

    clientStIdle :: ChainPoint -> ChainTip -> IO (ClientPipelinedStIdle 'Z CardanoBlock ChainPoint ChainTip IO ())
    clientStIdle point nodeTip = do
      blockHeader <- getHeaderAtPoint point
      let clientTip = (\(BlockHeader _ _ blockNo) -> blockNo) <$> blockHeader
      pure $ mkClientStIdle onEvent getHeaderAtPoint pipelinePolicy Zero clientTip nodeTip

    pipelinePolicy :: MkPipelineDecision
    pipelinePolicy = pipelineDecisionLowHighMark 1 50

mkClientStIdle
  :: (ChainSyncEvent -> IO ())
  -> (ChainPoint -> IO (WithOrigin BlockHeader))
  -> MkPipelineDecision
  -> Nat n
  -> WithOrigin BlockNo
  -> ChainTip
  -> ClientPipelinedStIdle n CardanoBlock ChainPoint ChainTip IO ()
mkClientStIdle onEvent getHeaderAtPoint pipelinePolicy n clientTip nodeTip =
  case (n, runPipelineDecision pipelinePolicy n clientTip nodeTip') of
    (_, (Request, nextPipelinePolicy)) ->
      SendMsgRequestNext clientStNext $ pure clientStNext
        where
          clientStNext = mkClientStNext onEvent getHeaderAtPoint
            $ mkClientStIdle onEvent getHeaderAtPoint nextPipelinePolicy n
    (_, (Pipeline, nextPipelinePolicy)) ->
      SendMsgRequestNextPipelined
        $ mkClientStIdle onEvent getHeaderAtPoint nextPipelinePolicy (Succ n) clientTip nodeTip
    (Succ n', (CollectOrPipeline, nextPipelinePolicy)) ->
      CollectResponse
        ( Just
            $ pure
            $ SendMsgRequestNextPipelined
            $ mkClientStIdle onEvent getHeaderAtPoint nextPipelinePolicy (Succ n) clientTip nodeTip
        )
        ( mkClientStNext onEvent  getHeaderAtPoint
            $ mkClientStIdle onEvent getHeaderAtPoint nextPipelinePolicy n'
        )
    (Succ n', (Collect, nextPipelinePolicy)) ->
      CollectResponse
        Nothing
        ( mkClientStNext onEvent getHeaderAtPoint
            $ mkClientStIdle onEvent getHeaderAtPoint nextPipelinePolicy n'
        )
    where
      nodeTip' = case nodeTip of
        ChainTipAtGenesis    -> Origin
        ChainTip _ _ blockNo -> At blockNo

mkClientStNext
  :: (ChainSyncEvent -> IO ())
  -> (ChainPoint -> IO (WithOrigin BlockHeader))
  -> (WithOrigin BlockNo -> ChainTip -> ClientPipelinedStIdle n CardanoBlock ChainPoint ChainTip IO ())
  -> ClientStNext n CardanoBlock ChainPoint ChainTip IO ()
mkClientStNext onEvent getHeaderAtPoint next = ClientStNext
  { recvMsgRollForward = \block tip -> do
      onEvent $ RollForward block tip
      let BlockInMode (Block (BlockHeader _ _ blockNo) _) _ = block
      let clientTip = At blockNo
      pure $ next clientTip tip
  , recvMsgRollBackward = \point tip -> do
      onEvent $ RollBackward point tip
      blockHeader <- getHeaderAtPoint point
      let clientTip = (\(BlockHeader _ _ blockNo) -> blockNo) <$> blockHeader
      pure $ next clientTip tip
  }
