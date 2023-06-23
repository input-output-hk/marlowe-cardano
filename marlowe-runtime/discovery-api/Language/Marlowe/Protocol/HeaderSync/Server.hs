{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Protocol.HeaderSync.Server where

import Language.Marlowe.Protocol.HeaderSync.Types
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader, ChainPoint)
import Language.Marlowe.Runtime.Discovery.Api (ContractHeader)
import Network.Protocol.Peer.Trace
import Network.TypedProtocol

newtype MarloweHeaderSyncServer m a = MarloweHeaderSyncServer {runMarloweHeaderSyncServer :: m (ServerStIdle m a)}
  deriving (Functor)

data ServerStIdle m a = ServerStIdle
  { recvMsgRequestNext :: m (ServerStNext m a)
  , recvMsgIntersect :: [BlockHeader] -> m (ServerStIntersect m a)
  , recvMsgDone :: m a
  }
  deriving (Functor)

data ServerStIntersect m a where
  SendMsgIntersectFound :: BlockHeader -> ServerStIdle m a -> ServerStIntersect m a
  SendMsgIntersectNotFound :: ServerStIdle m a -> ServerStIntersect m a

deriving instance (Functor m) => Functor (ServerStIntersect m)

data ServerStNext m a where
  SendMsgNewHeaders :: BlockHeader -> [ContractHeader] -> ServerStIdle m a -> ServerStNext m a
  SendMsgRollBackward :: ChainPoint -> ServerStIdle m a -> ServerStNext m a
  SendMsgWait :: ServerStWait m a -> ServerStNext m a

deriving instance (Functor m) => Functor (ServerStNext m)

data ServerStWait m a = ServerStWait
  { recvMsgPoll :: m (ServerStNext m a)
  , recvMsgCancel :: m (ServerStIdle m a)
  }
  deriving (Functor)

hoistMarloweHeaderSyncServer
  :: forall m n a
   . (Functor m)
  => (forall x. m x -> n x)
  -> MarloweHeaderSyncServer m a
  -> MarloweHeaderSyncServer n a
hoistMarloweHeaderSyncServer nat = MarloweHeaderSyncServer . nat . fmap hoistIdle . runMarloweHeaderSyncServer
  where
    hoistIdle :: ServerStIdle m a -> ServerStIdle n a
    hoistIdle ServerStIdle{..} =
      ServerStIdle
        { recvMsgRequestNext = nat $ fmap hoistNext recvMsgRequestNext
        , recvMsgIntersect = nat . fmap hoistIntersect . recvMsgIntersect
        , recvMsgDone = nat recvMsgDone
        }

    hoistIntersect :: ServerStIntersect m a -> ServerStIntersect n a
    hoistIntersect = \case
      SendMsgIntersectFound block idle -> SendMsgIntersectFound block $ hoistIdle idle
      SendMsgIntersectNotFound idle -> SendMsgIntersectNotFound $ hoistIdle idle

    hoistNext :: ServerStNext m a -> ServerStNext n a
    hoistNext = \case
      SendMsgNewHeaders block headers idle -> SendMsgNewHeaders block headers $ hoistIdle idle
      SendMsgRollBackward block idle -> SendMsgRollBackward block $ hoistIdle idle
      SendMsgWait wait -> SendMsgWait $ hoistWait wait

    hoistWait :: ServerStWait m a -> ServerStWait n a
    hoistWait ServerStWait{..} =
      ServerStWait
        { recvMsgPoll = nat $ fmap hoistNext recvMsgPoll
        , recvMsgCancel = nat $ fmap hoistIdle recvMsgCancel
        }

marloweHeaderSyncServerPeer
  :: forall m a
   . (Functor m)
  => MarloweHeaderSyncServer m a
  -> PeerTraced MarloweHeaderSync 'AsServer 'StIdle m a
marloweHeaderSyncServerPeer = EffectTraced . fmap peerIdle . runMarloweHeaderSyncServer
  where
    peerIdle :: ServerStIdle m a -> PeerTraced MarloweHeaderSync 'AsServer 'StIdle m a
    peerIdle ServerStIdle{..} = AwaitTraced (ClientAgency TokIdle) \case
      MsgDone -> Closed TokDone recvMsgDone
      MsgIntersect blocks -> Respond (ServerAgency TokIntersect) $ peerIntersect <$> recvMsgIntersect blocks
      MsgRequestNext -> Respond (ServerAgency TokNext) $ peerNext <$> recvMsgRequestNext

    peerIntersect
      :: ServerStIntersect m a
      -> Response MarloweHeaderSync 'AsServer 'StIntersect m a
    peerIntersect = \case
      SendMsgIntersectFound block idle ->
        Response (MsgIntersectFound block) $ peerIdle idle
      SendMsgIntersectNotFound idle ->
        Response MsgIntersectNotFound $ peerIdle idle

    peerNext
      :: ServerStNext m a
      -> Response MarloweHeaderSync 'AsServer 'StNext m a
    peerNext = \case
      SendMsgNewHeaders block headers idle ->
        Response (MsgNewHeaders block headers) $ peerIdle idle
      SendMsgRollBackward block idle ->
        Response (MsgRollBackward block) $ peerIdle idle
      SendMsgWait wait ->
        Response MsgWait $ peerWait wait

    peerWait :: ServerStWait m a -> PeerTraced MarloweHeaderSync 'AsServer 'StWait m a
    peerWait ServerStWait{..} = AwaitTraced (ClientAgency TokWait) \case
      MsgPoll -> Respond (ServerAgency TokNext) $ peerNext <$> recvMsgPoll
      MsgCancel -> Receive $ EffectTraced $ peerIdle <$> recvMsgCancel
