{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Protocol.HeaderSync.Server
  where

import Language.Marlowe.Protocol.HeaderSync.Types
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader, ChainPoint)
import Language.Marlowe.Runtime.Discovery.Api (ContractHeader)
import Network.TypedProtocol

newtype MarloweHeaderSyncServer m a = MarloweHeaderSyncServer { runMarloweHeaderSyncServer :: m (ServerStIdle m a) }
  deriving Functor

data ServerStIdle m a = ServerStIdle
  { recvMsgRequestNext :: m (ServerStNext m a)
  , recvMsgIntersect :: [BlockHeader] -> m (ServerStIntersect m a)
  , recvMsgDone :: m a
  } deriving Functor

data ServerStIntersect m a where
  SendMsgIntersectFound :: BlockHeader -> ServerStIdle m a -> ServerStIntersect m a
  SendMsgIntersectNotFound :: ServerStIdle m a -> ServerStIntersect m a

deriving instance Functor m => Functor (ServerStIntersect m)

data ServerStNext m a where
  SendMsgNewHeaders :: BlockHeader -> [ContractHeader] -> ServerStIdle m a -> ServerStNext m a
  SendMsgRollBackward :: ChainPoint -> ServerStIdle m a -> ServerStNext m a
  SendMsgWait :: ServerStWait m a -> ServerStNext m a

deriving instance Functor m => Functor (ServerStNext m)

data ServerStWait m a = ServerStWait
  { recvMsgPoll   :: m (ServerStNext m a)
  , recvMsgCancel :: m (ServerStIdle m a)
  } deriving Functor

hoistMarloweHeaderSyncServer
  :: forall m n a
   . Functor m
  => (forall x. m x -> n x)
  -> MarloweHeaderSyncServer m a
  -> MarloweHeaderSyncServer n a
hoistMarloweHeaderSyncServer nat = MarloweHeaderSyncServer . nat . fmap hoistIdle . runMarloweHeaderSyncServer
  where
    hoistIdle :: ServerStIdle m a -> ServerStIdle n a
    hoistIdle ServerStIdle{..} = ServerStIdle
      { recvMsgRequestNext = nat $ fmap hoistNext recvMsgRequestNext
      , recvMsgIntersect = nat . fmap hoistIntersect . recvMsgIntersect
      , recvMsgDone = nat recvMsgDone
      }

    hoistIntersect :: ServerStIntersect m a -> ServerStIntersect n a
    hoistIntersect = \case
      SendMsgIntersectFound block idle -> SendMsgIntersectFound block $ hoistIdle idle
      SendMsgIntersectNotFound idle             -> SendMsgIntersectNotFound $ hoistIdle idle

    hoistNext :: ServerStNext m a -> ServerStNext n a
    hoistNext = \case
      SendMsgNewHeaders block headers idle -> SendMsgNewHeaders block headers $ hoistIdle idle
      SendMsgRollBackward block idle      -> SendMsgRollBackward block $ hoistIdle idle
      SendMsgWait wait                          -> SendMsgWait $ hoistWait wait

    hoistWait :: ServerStWait m a -> ServerStWait n a
    hoistWait ServerStWait{..} = ServerStWait
      { recvMsgPoll = nat $ fmap hoistNext recvMsgPoll
      , recvMsgCancel = nat $ fmap hoistIdle recvMsgCancel
      }

marloweHeaderSyncServerPeer :: forall m a. Functor m => MarloweHeaderSyncServer m a -> Peer MarloweHeaderSync 'AsServer 'StIdle m a
marloweHeaderSyncServerPeer = Effect . fmap peerIdle . runMarloweHeaderSyncServer
  where
    peerIdle :: ServerStIdle m a -> Peer MarloweHeaderSync 'AsServer 'StIdle m a
    peerIdle ServerStIdle{..} = Await (ClientAgency TokIdle) $ Effect . \case
      MsgDone -> Done TokDone <$> recvMsgDone
      MsgIntersect blocks -> peerIntersect <$> recvMsgIntersect blocks
      MsgRequestNext -> peerNext <$> recvMsgRequestNext

    peerIntersect :: ServerStIntersect m a -> Peer MarloweHeaderSync 'AsServer 'StIntersect m a
    peerIntersect = \case
      SendMsgIntersectFound block idle ->
        Yield (ServerAgency TokIntersect) (MsgIntersectFound block) $
        peerIdle idle
      SendMsgIntersectNotFound idle ->
        Yield (ServerAgency TokIntersect) MsgIntersectNotFound $
        peerIdle idle

    peerNext :: ServerStNext m a -> Peer MarloweHeaderSync 'AsServer 'StNext m a
    peerNext = \case
      SendMsgNewHeaders block headers idle ->
        Yield (ServerAgency TokNext) (MsgNewHeaders block headers) $
        peerIdle idle
      SendMsgRollBackward block idle ->
        Yield (ServerAgency TokNext) (MsgRollBackward block) $
        peerIdle idle
      SendMsgWait wait ->
        Yield (ServerAgency TokNext) MsgWait $
        peerWait wait

    peerWait :: ServerStWait m a -> Peer MarloweHeaderSync 'AsServer 'StWait m a
    peerWait ServerStWait{..} = Await (ClientAgency TokWait) $ Effect . \case
      MsgPoll   -> peerNext <$> recvMsgPoll
      MsgCancel -> peerIdle <$> recvMsgCancel
