{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Protocol.BulkSync.Server where

import Data.Word (Word8)
import Language.Marlowe.Protocol.BulkSync.Types
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader, ChainPoint)
import Language.Marlowe.Runtime.History.Api
import Network.Protocol.Peer.Trace
import Network.TypedProtocol

newtype MarloweBulkSyncServer m a = MarloweBulkSyncServer {runMarloweBulkSyncServer :: m (ServerStIdle m a)}
  deriving (Functor)

data ServerStIdle m a = ServerStIdle
  { recvMsgRequestNext :: Word8 -> m (ServerStNext m a)
  , recvMsgIntersect :: [BlockHeader] -> m (ServerStIntersect m a)
  , recvMsgDone :: m a
  }
  deriving (Functor)

data ServerStIntersect m a where
  SendMsgIntersectFound :: BlockHeader -> BlockHeader -> ServerStIdle m a -> ServerStIntersect m a
  SendMsgIntersectNotFound :: ChainPoint -> ServerStIdle m a -> ServerStIntersect m a

deriving instance (Functor m) => Functor (ServerStIntersect m)

data ServerStNext m a where
  SendMsgRollForward :: [MarloweBlock] -> BlockHeader -> ServerStIdle m a -> ServerStNext m a
  SendMsgRollBackward :: ChainPoint -> ChainPoint -> ServerStIdle m a -> ServerStNext m a
  SendMsgWait :: ServerStPoll m a -> ServerStNext m a

deriving instance (Functor m) => Functor (ServerStNext m)

data ServerStPoll m a = ServerStPoll
  { recvMsgPoll :: m (ServerStNext m a)
  , recvMsgCancel :: m (ServerStIdle m a)
  }
  deriving (Functor)

hoistMarloweBulkSyncServer
  :: forall m n a
   . (Functor m)
  => (forall x. m x -> n x)
  -> MarloweBulkSyncServer m a
  -> MarloweBulkSyncServer n a
hoistMarloweBulkSyncServer nat = MarloweBulkSyncServer . nat . fmap hoistIdle . runMarloweBulkSyncServer
  where
    hoistIdle :: ServerStIdle m a -> ServerStIdle n a
    hoistIdle ServerStIdle{..} =
      ServerStIdle
        { recvMsgRequestNext = nat . fmap hoistNext . recvMsgRequestNext
        , recvMsgIntersect = nat . fmap hoistIntersect . recvMsgIntersect
        , recvMsgDone = nat recvMsgDone
        }

    hoistIntersect :: ServerStIntersect m a -> ServerStIntersect n a
    hoistIntersect = \case
      SendMsgIntersectFound block tip idle -> SendMsgIntersectFound block tip $ hoistIdle idle
      SendMsgIntersectNotFound tip idle -> SendMsgIntersectNotFound tip $ hoistIdle idle

    hoistNext :: ServerStNext m a -> ServerStNext n a
    hoistNext = \case
      SendMsgRollForward blocks tip idle -> SendMsgRollForward blocks tip $ hoistIdle idle
      SendMsgRollBackward block tip idle -> SendMsgRollBackward block tip $ hoistIdle idle
      SendMsgWait wait -> SendMsgWait $ hoistWait wait

    hoistWait :: ServerStPoll m a -> ServerStPoll n a
    hoistWait ServerStPoll{..} =
      ServerStPoll
        { recvMsgPoll = nat $ fmap hoistNext recvMsgPoll
        , recvMsgCancel = nat $ fmap hoistIdle recvMsgCancel
        }

marloweBulkSyncServerPeer
  :: forall m a
   . (Functor m)
  => MarloweBulkSyncServer m a
  -> PeerTraced MarloweBulkSync 'AsServer 'StIdle m a
marloweBulkSyncServerPeer = EffectTraced . fmap peerIdle . runMarloweBulkSyncServer
  where
    peerIdle :: ServerStIdle m a -> PeerTraced MarloweBulkSync 'AsServer 'StIdle m a
    peerIdle ServerStIdle{..} = AwaitTraced (ClientAgency TokIdle) \case
      MsgDone -> Closed TokDone recvMsgDone
      MsgIntersect blocks -> Respond (ServerAgency TokIntersect) $ peerIntersect <$> recvMsgIntersect blocks
      MsgRequestNext batchSize -> Respond (ServerAgency TokNext) $ peerNext <$> recvMsgRequestNext batchSize

    peerIntersect
      :: ServerStIntersect m a
      -> Response MarloweBulkSync 'AsServer 'StIntersect m a
    peerIntersect = \case
      SendMsgIntersectFound block tip idle ->
        Response (MsgIntersectFound block tip) $ peerIdle idle
      SendMsgIntersectNotFound tip idle ->
        Response (MsgIntersectNotFound tip) $ peerIdle idle

    peerNext
      :: ServerStNext m a
      -> Response MarloweBulkSync 'AsServer 'StNext m a
    peerNext = \case
      SendMsgRollForward blocks tip idle ->
        Response (MsgRollForward blocks tip) $ peerIdle idle
      SendMsgRollBackward block tip idle ->
        Response (MsgRollBackward block tip) $ peerIdle idle
      SendMsgWait wait ->
        Response MsgWait $ peerWait wait

    peerWait :: ServerStPoll m a -> PeerTraced MarloweBulkSync 'AsServer 'StPoll m a
    peerWait ServerStPoll{..} = AwaitTraced (ClientAgency TokPoll) \case
      MsgPoll -> Respond (ServerAgency TokNext) $ peerNext <$> recvMsgPoll
      MsgCancel -> Receive $ EffectTraced $ peerIdle <$> recvMsgCancel
