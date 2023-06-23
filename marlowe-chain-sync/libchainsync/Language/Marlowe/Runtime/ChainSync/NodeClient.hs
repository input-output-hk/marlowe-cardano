{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Language.Marlowe.Runtime.ChainSync.NodeClient (
  NodeClient (..),
  NodeClientDependencies (..),
  NodeClientSelector (..),
  QueryNode,
  SubmitToNode,
  nodeClient,
) where

import Cardano.Api (
  BlockInMode,
  BlockNo (..),
  CardanoMode,
  ChainPoint,
  ChainTip (..),
  LocalChainSyncClient (LocalChainSyncClient),
  LocalNodeClientProtocols (..),
  LocalNodeClientProtocolsInMode,
  QueryInMode,
  SlotNo (..),
  TxInMode,
  TxValidationErrorInMode,
  chainTipToChainPoint,
  serialiseToRawBytes,
 )
import Cardano.Api.ChainSync.Client (ChainSyncClient (..), ClientStIdle (..), ClientStIntersect (..), ClientStNext (..))
import Cardano.Api.Shelley (AcquiringFailure (..))
import Colog (Message, WithLog)
import Control.Concurrent.Component (Component, component)
import Control.Concurrent.STM (TVar)
import Control.Concurrent.STM.TChan (TChan, newTChan, readTChan, writeTChan)
import Control.Concurrent.STM.TMVar (TMVar, putTMVar, takeTMVar)
import Control.Monad.Event.Class (MonadInjectEvent, withEvent)
import Data.Bifunctor (first)
import Language.Marlowe.Runtime.ChainSync.Api (WithGenesis (..))
import qualified Language.Marlowe.Runtime.ChainSync.Api as ChainSync
import Observe.Event (addField)
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Client as Q
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as Q
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Client as S
import UnliftIO (MonadIO, MonadUnliftIO, STM, atomically, newEmptyTMVarIO, newTVar, readTVar, writeTVar)

type SubmitToNode m =
  TxInMode CardanoMode
  -> m (S.SubmitResult (TxValidationErrorInMode CardanoMode))

type QueryNode m =
  Maybe ChainPoint
  -> forall result
   . QueryInMode CardanoMode result
  -> m (Either AcquiringFailure result)

data NodeClient m = NodeClient
  { submitTxToNode :: SubmitToNode m
  , queryNode :: QueryNode m
  , nodeTip :: STM ChainSync.ChainPoint
  }

newtype NodeClientDependencies m = NodeClientDependencies
  { connectToLocalNode :: LocalNodeClientProtocolsInMode CardanoMode -> m ()
  }

data NodeClientSelector f where
  Submit :: NodeClientSelector (TxInMode CardanoMode)
  Query :: NodeClientSelector (Q.Some (QueryInMode CardanoMode))

nodeClient
  :: (MonadInjectEvent r NodeClientSelector s m, MonadUnliftIO m, WithLog env Message m)
  => Component m (NodeClientDependencies m) (NodeClient m)
nodeClient =
  component "chain-sync-node-client" \NodeClientDependencies{..} ->
    do
      queryChannel <- newTChan
      submitChannel <- newTChan
      nodeTipVar <- newTVar Genesis
      let runNodeClient =
            connectToLocalNode
              LocalNodeClientProtocols
                { -- The chain sync client keeps the connection open.
                  -- Without this client, the node would close the connection in
                  -- about three seconds, making it impossible to submit and query.
                  localChainSyncClient = LocalChainSyncClient $ chainSyncClient nodeTipVar
                , localTxSubmissionClient = Just $ submitClient submitChannel
                , localTxMonitoringClient = Nothing
                , localStateQueryClient = Just $ queryClient queryChannel
                }
      pure
        ( runNodeClient
        , NodeClient
            { submitTxToNode = submitTxToNodeChannel submitChannel
            , queryNode = queryNodeChannel queryChannel
            , nodeTip = readTVar nodeTipVar
            }
        )

type SubmitChannel = TChan SubmitJob

data SubmitJob = SubmitJob
  { tx :: TxInMode CardanoMode
  , submitResultTMVar :: TMVar (S.SubmitResult (TxValidationErrorInMode CardanoMode))
  }

submitTxToNodeChannel
  :: (MonadIO m, MonadInjectEvent r NodeClientSelector s m)
  => SubmitChannel
  -> TxInMode CardanoMode
  -> m (S.SubmitResult (TxValidationErrorInMode CardanoMode))
submitTxToNodeChannel channel tx =
  withEvent Submit \event ->
    do
      event `addField` tx
      submitResultTMVar <- newEmptyTMVarIO
      atomically $ writeTChan channel SubmitJob{..}
      atomically $ takeTMVar submitResultTMVar

submitClient
  :: SubmitChannel
  -> S.LocalTxSubmissionClient (TxInMode CardanoMode) (TxValidationErrorInMode CardanoMode) IO ()
submitClient channel =
  let next =
        do
          SubmitJob{..} <- atomically $ readTChan channel
          pure . S.SendMsgSubmitTx tx $
            \result ->
              do
                atomically $ putTMVar submitResultTMVar result
                next
   in S.LocalTxSubmissionClient next

type QueryChannel = TChan QueryJob

data QueryJob = forall result.
  QueryJob
  { point :: Maybe ChainPoint
  , query :: QueryInMode CardanoMode result
  , queryResultTMVar :: TMVar (Either Q.AcquireFailure result)
  }

queryNodeChannel
  :: (MonadIO m, MonadInjectEvent r NodeClientSelector s m)
  => QueryChannel
  -> Maybe ChainPoint
  -> QueryInMode CardanoMode result
  -> m (Either AcquiringFailure result)
queryNodeChannel channel point query =
  withEvent Query \event ->
    do
      event `addField` Q.Some query
      let toAcquiringFailure Q.AcquireFailurePointTooOld = AFPointTooOld
          toAcquiringFailure Q.AcquireFailurePointNotOnChain = AFPointNotOnChain
      queryResultTMVar <- newEmptyTMVarIO
      atomically . writeTChan channel $ QueryJob{..}
      atomically $ first toAcquiringFailure <$> takeTMVar queryResultTMVar

queryClient
  :: QueryChannel
  -> Q.LocalStateQueryClient (BlockInMode CardanoMode) ChainPoint (QueryInMode CardanoMode) IO ()
queryClient channel =
  let next =
        do
          QueryJob{..} <- atomically $ readTChan channel
          pure . Q.SendMsgAcquire point $
            Q.ClientStAcquiring
              { recvMsgAcquired =
                  pure . Q.SendMsgQuery query $
                    Q.ClientStQuerying
                      { recvMsgResult = \result ->
                          do
                            atomically . putTMVar queryResultTMVar $ Right result
                            pure $ Q.SendMsgRelease next
                      }
              , recvMsgFailure = \failure ->
                  do
                    atomically . putTMVar queryResultTMVar $ Left failure
                    next -- Allow future queries after an acquisition failure.
              }
   in Q.LocalStateQueryClient next

chainSyncClient
  :: forall m
   . (MonadIO m)
  => TVar ChainSync.ChainPoint
  -> ChainSyncClient (BlockInMode CardanoMode) ChainPoint ChainTip m ()
chainSyncClient nodeTipVar =
  let stStart :: ClientStIdle (BlockInMode CardanoMode) ChainPoint ChainTip m ()
      stStart = SendMsgRequestNext stFirst $ pure stNext
      stFirst :: ClientStNext (BlockInMode CardanoMode) ChainPoint ChainTip m ()
      stFirst =
        ClientStNext
          { recvMsgRollForward = \_ tip -> ChainSyncClient do
              atomically $ writeTVar nodeTipVar $ fromCardanoChainTip tip
              pure $ stTip tip -- Identified the tip, now intersect with it.
          , recvMsgRollBackward = \_ tip -> ChainSyncClient do
              atomically $ writeTVar nodeTipVar $ fromCardanoChainTip tip
              pure $ stTip tip -- Identified the tip, now intersect with it.
          }
      stTip :: ChainTip -> ClientStIdle (BlockInMode CardanoMode) ChainPoint ChainTip m ()
      stTip tip =
        SendMsgFindIntersect [chainTipToChainPoint tip] $
          ClientStIntersect
            { recvMsgIntersectFound = \_ _ -> ChainSyncClient $ pure stIdle -- The tip was found, so follow it.
            , recvMsgIntersectNotFound = \tip' -> ChainSyncClient . pure $ stTip tip' -- The tip was not found, so try again.
            }
      stIdle :: ClientStIdle (BlockInMode CardanoMode) ChainPoint ChainTip m ()
      stIdle = SendMsgRequestNext stNext $ pure stNext
      stNext :: ClientStNext (BlockInMode CardanoMode) ChainPoint ChainTip m ()
      stNext =
        ClientStNext
          { recvMsgRollForward = \_ tip -> ChainSyncClient do
              atomically $ writeTVar nodeTipVar $ fromCardanoChainTip tip
              pure stIdle -- Continuing following the tip.
          , recvMsgRollBackward = \_ tip -> ChainSyncClient do
              atomically $ writeTVar nodeTipVar $ fromCardanoChainTip tip
              pure stIdle -- Continuing following the tip.
          }
   in ChainSyncClient $ pure stStart

fromCardanoChainTip :: ChainTip -> ChainSync.ChainPoint
fromCardanoChainTip = \case
  ChainTipAtGenesis -> ChainSync.Genesis
  ChainTip (SlotNo slot) hash (BlockNo block) ->
    ChainSync.At $
      ChainSync.BlockHeader
        (ChainSync.SlotNo slot)
        (ChainSync.BlockHeaderHash $ serialiseToRawBytes hash)
        (ChainSync.BlockNo block)
