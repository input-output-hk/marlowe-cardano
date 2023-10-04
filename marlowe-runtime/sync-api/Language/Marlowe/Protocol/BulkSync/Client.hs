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

-- | A CPS-style client of the bulk sync protocol.
newtype MarloweBulkSyncClient m a = MarloweBulkSyncClient {runMarloweBulkSyncClient :: m (ClientStIdle m a)}
  deriving (Functor)

-- | An action a client can take from the idle state.
data ClientStIdle m a where
  -- | Request the next batch of blocks from the server.
  SendMsgRequestNext
    :: Word8
    -- ^ The number of *extra* blocks to fetch (0 = fetch one block).
    -> ClientStNext m a
    -- ^ Handler functions for the possible responses from the server.
    -> ClientStIdle m a
  -- | Intersect the client's chain with the server's by finding the latest common point.
  SendMsgIntersect
    :: [BlockHeader]
    -- ^ Points from the client's chain to try to intersect with the server.
    -> ClientStIntersect m a
    -- ^ Handler functions for the possible responses from the server.
    -> ClientStIdle m a
  -- | End the session.
  SendMsgDone
    :: a
    -- ^ The result of running the client.
    -> ClientStIdle m a

deriving instance (Functor m) => Functor (ClientStIdle m)

-- | Responses the server can send from the intersect state.
data ClientStIntersect m a = ClientStIntersect
  { recvMsgIntersectFound :: BlockHeader -> BlockHeader -> m (ClientStIdle m a)
  -- ^ A common point was found between the client and the server. Syncing will begin from this point.
  , recvMsgIntersectNotFound :: ChainPoint -> m (ClientStIdle m a)
  -- ^ No common points were found. Syncing will resume from the client's previous point.
  }
  deriving (Functor)

-- | Responses the server can send from the next state.
data ClientStNext m a = ClientStNext
  { recvMsgRollForward :: [MarloweBlock] -> BlockHeader -> m (ClientStIdle m a)
  -- ^ Handle new blocks from the server.
  , recvMsgRollBackward :: ChainPoint -> ChainPoint -> m (ClientStIdle m a)
  -- ^ Handle a rollback.
  , recvMsgWait :: m (ClientStPoll m a)
  -- ^ Enter a polling loop.
  }
  deriving (Functor)

-- | An action a client can take from the poll state.
data ClientStPoll m a where
  -- | Poll for new headers.
  SendMsgPoll
    :: ClientStNext m a
    -- ^ Handler functions for the possible responses from the server.
    -> ClientStPoll m a
  -- | Exit the polling loop.
  SendMsgCancel
    :: ClientStIdle m a
    -- ^ The next action to perform.
    -> ClientStPoll m a

deriving instance (Functor m) => Functor (ClientStPoll m)

-- | Hoist a natural transformation over a client's base monad.
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
      SendMsgRequestNext extraBlockCount next -> SendMsgRequestNext extraBlockCount $ hoistNext next
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

-- | Transform a bulk sync client to a type-indexed peer.
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
      SendMsgRequestNext extraBlockCount next ->
        YieldTraced (ClientAgency TokIdle) (MsgRequestNext extraBlockCount) $
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

-- | Eliminate a client and a server pair by serving the client.
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
      SendMsgRequestNext extraBlockCount next -> serveNext next =<< recvMsgRequestNext extraBlockCount
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
