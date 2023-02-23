{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Protocol.HeaderSync.Client
  where

import Language.Marlowe.Protocol.HeaderSync.Types
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader, ChainPoint)
import Language.Marlowe.Runtime.Discovery.Api (ContractHeader)
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

marloweHeaderSyncClientPeer :: forall m a. Functor m => MarloweHeaderSyncClient m a -> Peer MarloweHeaderSync 'AsClient 'StIdle m a
marloweHeaderSyncClientPeer = Effect . fmap peerIdle . runMarloweHeaderSyncClient
  where
    peerIdle :: ClientStIdle m a -> Peer MarloweHeaderSync 'AsClient 'StIdle m a
    peerIdle = \case
      SendMsgDone a -> Yield (ClientAgency TokIdle) MsgDone $ Done TokDone a
      SendMsgIntersect blocks intersect-> Yield (ClientAgency TokIdle) (MsgIntersect blocks) $ peerIntersect intersect
      SendMsgRequestNext next -> Yield (ClientAgency TokIdle) MsgRequestNext $ peerNext next

    peerIntersect :: ClientStIntersect m a -> Peer MarloweHeaderSync 'AsClient 'StIntersect m a
    peerIntersect ClientStIntersect{..} = Await (ServerAgency TokIntersect) $ Effect . \case
      MsgIntersectFound block -> peerIdle <$> recvMsgIntersectFound block
      MsgIntersectNotFound -> peerIdle <$> recvMsgIntersectNotFound

    peerNext :: ClientStNext m a -> Peer MarloweHeaderSync 'AsClient 'StNext m a
    peerNext ClientStNext{..} = Await (ServerAgency TokNext) $ Effect . \case
      MsgNewHeaders block headers -> peerIdle <$> recvMsgNewHeaders block headers
      MsgRollBackward block -> peerIdle <$> recvMsgRollBackward block
      MsgWait -> peerWait <$> recvMsgWait

    peerWait :: ClientStWait m a -> Peer MarloweHeaderSync 'AsClient 'StWait m a
    peerWait = \case
      SendMsgPoll next -> Yield (ClientAgency TokWait) MsgPoll $ peerNext next
      SendMsgCancel idle -> Yield (ClientAgency TokWait) MsgCancel $ peerIdle idle
