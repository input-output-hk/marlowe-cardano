{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Protocol.HeaderSync.Client where

import Control.Monad (join)
import Language.Marlowe.Protocol.HeaderSync.Server
import Language.Marlowe.Protocol.HeaderSync.Types
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader, ChainPoint)
import Language.Marlowe.Runtime.Discovery.Api (ContractHeader)
import Network.Protocol.Peer.Trace
import Network.TypedProtocol

newtype MarloweHeaderSyncClient m a = MarloweHeaderSyncClient { runMarloweHeaderSyncClient :: m (ClientStIdle m a) }
  deriving Functor

data ClientStIdle m a where
  SendMsgRequestNext :: ClientStNext m a -> ClientStIdle m a
  SendMsgIntersect :: [BlockHeader] -> ClientStIntersect m a -> ClientStIdle m a
  SendMsgDone :: a -> ClientStIdle m a

deriving instance Functor m => Functor (ClientStIdle m)

data ClientStIntersect m a = ClientStIntersect
  { recvMsgIntersectFound :: BlockHeader -> m (ClientStIdle m a)
  , recvMsgIntersectNotFound :: m (ClientStIdle m a)
  } deriving Functor

data ClientStNext m a = ClientStNext
  { recvMsgNewHeaders :: BlockHeader -> [ContractHeader] -> m (ClientStIdle m a)
  , recvMsgRollBackward :: ChainPoint -> m (ClientStIdle m a)
  , recvMsgWait :: m (ClientStWait m a)
  } deriving Functor

data ClientStWait m a where
  SendMsgPoll :: ClientStNext m a -> ClientStWait m a
  SendMsgCancel :: ClientStIdle m a -> ClientStWait m a

deriving instance Functor m => Functor (ClientStWait m)

hoistMarloweHeaderSyncClient
  :: forall m n a
   . Functor m
  => (forall x. m x -> n x)
  -> MarloweHeaderSyncClient m a
  -> MarloweHeaderSyncClient n a
hoistMarloweHeaderSyncClient nat = MarloweHeaderSyncClient . nat . fmap hoistIdle . runMarloweHeaderSyncClient
  where
    hoistIdle :: ClientStIdle m a -> ClientStIdle n a
    hoistIdle = \case
      SendMsgRequestNext next -> SendMsgRequestNext $ hoistNext next
      SendMsgIntersect blocks intersect -> SendMsgIntersect blocks $ hoistIntersect intersect
      SendMsgDone a -> SendMsgDone a

    hoistIntersect :: ClientStIntersect m a -> ClientStIntersect n a
    hoistIntersect ClientStIntersect{..} = ClientStIntersect
      { recvMsgIntersectFound = nat . fmap hoistIdle . recvMsgIntersectFound
      , recvMsgIntersectNotFound = nat $ hoistIdle <$> recvMsgIntersectNotFound
      }

    hoistNext :: ClientStNext m a -> ClientStNext n a
    hoistNext ClientStNext{..} = ClientStNext
      { recvMsgNewHeaders = fmap (nat . fmap hoistIdle) . recvMsgNewHeaders
      , recvMsgRollBackward = nat . fmap hoistIdle . recvMsgRollBackward
      , recvMsgWait = nat $ hoistWait <$> recvMsgWait
      }

    hoistWait :: ClientStWait m a -> ClientStWait n a
    hoistWait = \case
      SendMsgPoll next -> SendMsgPoll $ hoistNext next
      SendMsgCancel idle -> SendMsgCancel $ hoistIdle idle

marloweHeaderSyncClientPeer
  :: forall m a
   . Functor m
  => MarloweHeaderSyncClient m a
  -> PeerTraced MarloweHeaderSync 'AsClient 'StIdle m a
marloweHeaderSyncClientPeer = EffectTraced . fmap peerIdle . runMarloweHeaderSyncClient
  where
    peerIdle :: ClientStIdle m a -> PeerTraced MarloweHeaderSync 'AsClient 'StIdle m a
    peerIdle = \case
      SendMsgDone a -> YieldTraced (ClientAgency TokIdle) MsgDone
        $ Close TokDone a
      SendMsgIntersect blocks intersect-> YieldTraced (ClientAgency TokIdle) (MsgIntersect blocks)
        $ Call (ServerAgency TokIntersect)
        $ peerIntersect intersect
      SendMsgRequestNext next -> YieldTraced (ClientAgency TokIdle) MsgRequestNext
        $ Call (ServerAgency TokNext)
        $ peerNext next

    peerIntersect
      :: ClientStIntersect m a
      -> Message MarloweHeaderSync 'StIntersect st
      -> PeerTraced MarloweHeaderSync 'AsClient st m a
    peerIntersect ClientStIntersect{..} = EffectTraced . \case
      MsgIntersectFound block -> peerIdle <$> recvMsgIntersectFound block
      MsgIntersectNotFound -> peerIdle <$> recvMsgIntersectNotFound

    peerNext
      :: ClientStNext m a
      -> Message MarloweHeaderSync 'StNext st
      -> PeerTraced MarloweHeaderSync 'AsClient st m a
    peerNext ClientStNext{..} = EffectTraced . \case
      MsgNewHeaders block headers -> peerIdle <$> recvMsgNewHeaders block headers
      MsgRollBackward block -> peerIdle <$> recvMsgRollBackward block
      MsgWait -> peerWait <$> recvMsgWait

    peerWait :: ClientStWait m a -> PeerTraced MarloweHeaderSync 'AsClient 'StWait m a
    peerWait = \case
      SendMsgPoll next -> YieldTraced (ClientAgency TokWait) MsgPoll
        $ Call (ServerAgency TokNext)
        $ peerNext next
      SendMsgCancel idle -> YieldTraced (ClientAgency TokWait) MsgCancel
        $ Cast
        $ peerIdle idle

serveMarloweHeaderSyncClient
  :: forall m a b
   . Monad m
  => MarloweHeaderSyncServer m a
  -> MarloweHeaderSyncClient m b
  -> m (a, b)
serveMarloweHeaderSyncClient MarloweHeaderSyncServer{..} MarloweHeaderSyncClient{..} =
  join $ serveIdle <$> runMarloweHeaderSyncServer <*> runMarloweHeaderSyncClient
  where
    serveIntersect :: ClientStIntersect m b -> ServerStIntersect m a -> m (a, b)
    serveIntersect ClientStIntersect{..} = \case
      SendMsgIntersectFound block idle -> serveIdle idle =<< recvMsgIntersectFound block
      SendMsgIntersectNotFound idle -> serveIdle idle =<< recvMsgIntersectNotFound

    serveIdle :: ServerStIdle m a -> ClientStIdle m b -> m (a, b)
    serveIdle ServerStIdle{..} = \case
      SendMsgRequestNext next -> serveNext next =<< recvMsgRequestNext
      SendMsgIntersect blocks intersect -> serveIntersect intersect =<< recvMsgIntersect blocks
      SendMsgDone b -> (,b) <$> recvMsgDone

    serveNext :: ClientStNext m b -> ServerStNext m a -> m (a, b)
    serveNext ClientStNext{..} = \case
      SendMsgNewHeaders block headers idle -> serveIdle idle =<< recvMsgNewHeaders block headers
      SendMsgRollBackward point idle -> serveIdle idle =<< recvMsgRollBackward point
      SendMsgWait poll -> serveWait poll =<< recvMsgWait

    serveWait :: ServerStWait m a -> ClientStWait m b -> m (a, b)
    serveWait ServerStWait{..} = \case
      SendMsgPoll next -> serveNext next =<< recvMsgPoll
      SendMsgCancel idle -> flip serveIdle idle =<< recvMsgCancel
