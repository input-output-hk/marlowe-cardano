{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Protocol.BulkSync.Client where

import Control.Monad (join)
import Data.Word (Word8)
import Language.Marlowe.Protocol.BulkSync.Server
import Language.Marlowe.Protocol.BulkSync.Types
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader, ChainPoint)
import Language.Marlowe.Runtime.History.Api
import Network.Protocol.Peer.Trace
import Network.TypedProtocol

newtype MarloweBulkSyncClient m a = MarloweBulkSyncClient {runMarloweBulkSyncClient :: m (ClientStIdle m a)}
  deriving (Functor)

data ClientStIdle m a where
  SendMsgRequestNext :: Word8 -> ClientStNext m a -> ClientStIdle m a
  SendMsgIntersect :: [BlockHeader] -> ClientStIntersect m a -> ClientStIdle m a
  SendMsgDone :: a -> ClientStIdle m a

deriving instance (Functor m) => Functor (ClientStIdle m)

data ClientStIntersect m a = ClientStIntersect
  { recvMsgIntersectFound :: BlockHeader -> BlockHeader -> m (ClientStIdle m a)
  , recvMsgIntersectNotFound :: ChainPoint -> m (ClientStIdle m a)
  }
  deriving (Functor)

data ClientStNext m a = ClientStNext
  { recvMsgRollForward :: [MarloweBlock] -> BlockHeader -> m (ClientStIdle m a)
  , recvMsgRollBackward :: ChainPoint -> ChainPoint -> m (ClientStIdle m a)
  , recvMsgWait :: m (ClientStPoll m a)
  }
  deriving (Functor)

data ClientStPoll m a where
  SendMsgPoll :: ClientStNext m a -> ClientStPoll m a
  SendMsgCancel :: ClientStIdle m a -> ClientStPoll m a

deriving instance (Functor m) => Functor (ClientStPoll m)

hoistMarloweBulkSyncClient
  :: forall m n a
   . (Functor m)
  => (forall x. m x -> n x)
  -> MarloweBulkSyncClient m a
  -> MarloweBulkSyncClient n a
hoistMarloweBulkSyncClient nat = MarloweBulkSyncClient . nat . fmap hoistIdle . runMarloweBulkSyncClient
  where
    hoistIdle :: ClientStIdle m a -> ClientStIdle n a
    hoistIdle = \case
      SendMsgRequestNext batchSize next -> SendMsgRequestNext batchSize $ hoistNext next
      SendMsgIntersect blocks intersect -> SendMsgIntersect blocks $ hoistIntersect intersect
      SendMsgDone a -> SendMsgDone a

    hoistIntersect :: ClientStIntersect m a -> ClientStIntersect n a
    hoistIntersect ClientStIntersect{..} =
      ClientStIntersect
        { recvMsgIntersectFound = fmap (nat . fmap hoistIdle) . recvMsgIntersectFound
        , recvMsgIntersectNotFound = nat . fmap hoistIdle . recvMsgIntersectNotFound
        }

    hoistNext :: ClientStNext m a -> ClientStNext n a
    hoistNext ClientStNext{..} =
      ClientStNext
        { recvMsgRollForward = fmap (nat . fmap hoistIdle) . recvMsgRollForward
        , recvMsgRollBackward = fmap (nat . fmap hoistIdle) . recvMsgRollBackward
        , recvMsgWait = nat $ hoistWait <$> recvMsgWait
        }

    hoistWait :: ClientStPoll m a -> ClientStPoll n a
    hoistWait = \case
      SendMsgPoll next -> SendMsgPoll $ hoistNext next
      SendMsgCancel idle -> SendMsgCancel $ hoistIdle idle

marloweBulkSyncClientPeer
  :: forall m a
   . (Functor m)
  => MarloweBulkSyncClient m a
  -> PeerTraced MarloweBulkSync 'AsClient 'StIdle m a
marloweBulkSyncClientPeer = EffectTraced . fmap peerIdle . runMarloweBulkSyncClient
  where
    peerIdle :: ClientStIdle m a -> PeerTraced MarloweBulkSync 'AsClient 'StIdle m a
    peerIdle = \case
      SendMsgDone a ->
        YieldTraced (ClientAgency TokIdle) MsgDone $
          Close TokDone a
      SendMsgIntersect blocks intersect ->
        YieldTraced (ClientAgency TokIdle) (MsgIntersect blocks) $
          Call (ServerAgency TokIntersect) $
            peerIntersect intersect
      SendMsgRequestNext batchSize next ->
        YieldTraced (ClientAgency TokIdle) (MsgRequestNext batchSize) $
          Call (ServerAgency TokNext) $
            peerNext next

    peerIntersect
      :: ClientStIntersect m a
      -> Message MarloweBulkSync 'StIntersect st
      -> PeerTraced MarloweBulkSync 'AsClient st m a
    peerIntersect ClientStIntersect{..} =
      EffectTraced . \case
        MsgIntersectFound block tip -> peerIdle <$> recvMsgIntersectFound block tip
        MsgIntersectNotFound tip -> peerIdle <$> recvMsgIntersectNotFound tip

    peerNext
      :: ClientStNext m a
      -> Message MarloweBulkSync 'StNext st
      -> PeerTraced MarloweBulkSync 'AsClient st m a
    peerNext ClientStNext{..} =
      EffectTraced . \case
        MsgRollForward block tip -> peerIdle <$> recvMsgRollForward block tip
        MsgRollBackward block tip -> peerIdle <$> recvMsgRollBackward block tip
        MsgWait -> peerWait <$> recvMsgWait

    peerWait :: ClientStPoll m a -> PeerTraced MarloweBulkSync 'AsClient 'StPoll m a
    peerWait = \case
      SendMsgPoll next ->
        YieldTraced (ClientAgency TokPoll) MsgPoll $
          Call (ServerAgency TokNext) $
            peerNext next
      SendMsgCancel idle ->
        YieldTraced (ClientAgency TokPoll) MsgCancel $
          Cast $
            peerIdle idle

serveMarloweBulkSyncClient
  :: forall m a b
   . (Monad m)
  => MarloweBulkSyncServer m a
  -> MarloweBulkSyncClient m b
  -> m (a, b)
serveMarloweBulkSyncClient MarloweBulkSyncServer{..} MarloweBulkSyncClient{..} =
  join $ serveIdle <$> runMarloweBulkSyncServer <*> runMarloweBulkSyncClient
  where
    serveIntersect :: ClientStIntersect m b -> ServerStIntersect m a -> m (a, b)
    serveIntersect ClientStIntersect{..} = \case
      SendMsgIntersectFound block tip idle -> serveIdle idle =<< recvMsgIntersectFound block tip
      SendMsgIntersectNotFound tip idle -> serveIdle idle =<< recvMsgIntersectNotFound tip

    serveIdle :: ServerStIdle m a -> ClientStIdle m b -> m (a, b)
    serveIdle ServerStIdle{..} = \case
      SendMsgRequestNext batchSize next -> serveNext next =<< recvMsgRequestNext batchSize
      SendMsgIntersect blocks intersect -> serveIntersect intersect =<< recvMsgIntersect blocks
      SendMsgDone b -> (,b) <$> recvMsgDone

    serveNext :: ClientStNext m b -> ServerStNext m a -> m (a, b)
    serveNext ClientStNext{..} = \case
      SendMsgRollForward block tip idle -> serveIdle idle =<< recvMsgRollForward block tip
      SendMsgRollBackward point tip idle -> serveIdle idle =<< recvMsgRollBackward point tip
      SendMsgWait poll -> serveWait poll =<< recvMsgWait

    serveWait :: ServerStPoll m a -> ClientStPoll m b -> m (a, b)
    serveWait ServerStPoll{..} = \case
      SendMsgPoll next -> serveNext next =<< recvMsgPoll
      SendMsgCancel idle -> flip serveIdle idle =<< recvMsgCancel
