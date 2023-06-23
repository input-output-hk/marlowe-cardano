{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- | A view of the chain sync protocol from the point of view of the
-- client. This provides a simplified interface for implementing the client
-- role of the protocol. The types should be much easier to use than the
-- underlying typed protocol types.
module Network.Protocol.ChainSeek.Client where

import Control.Monad (join)
import Network.Protocol.ChainSeek.Server
import Network.Protocol.ChainSeek.Types
import Network.Protocol.Peer.Trace
import Network.TypedProtocol (PeerHasAgency (..))
import Network.TypedProtocol.Core (PeerRole (..))

-- | A chain seek protocol client that runs in some monad 'm'.
newtype ChainSeekClient query point tip m a = ChainSeekClient
  { runChainSeekClient :: m (ClientStIdle query point tip m a)
  }

-- | In the `StIdle` protocol state, the client has agency. It must send
-- either:
--
-- * A query next update request
-- * A termination message
data ClientStIdle query point tip m a where
  -- | Send a query and handle the response.
  SendMsgQueryNext
    :: query err result
    -- ^ The query
    -> ClientStNext query err result point tip m a
    -- ^ A handler for the server response
    -> ClientStIdle query point tip m a
  -- | Send a termination message
  SendMsgDone
    :: a -- The result of running the protocol
    -> ClientStIdle query point tip m a

-- | In the `StNext` protocol state, the client does not have agency. Instead,
-- it must be prepared to handle either:
--
-- * A query rejection response
-- * A roll forward response
-- * A roll backward response
data ClientStNext query err result point tip m a = ClientStNext
  { recvMsgQueryRejected :: err -> tip -> m (ClientStIdle query point tip m a)
  , recvMsgRollForward :: result -> point -> tip -> m (ClientStIdle query point tip m a)
  , recvMsgRollBackward :: point -> tip -> m (ClientStIdle query point tip m a)
  , recvMsgWait :: m (ClientStPoll query err result point tip m a)
  }

-- | In the `StPoll` protocol state, the client has agency. It must send
-- either:
--
-- * A poll message
-- * A cancel message
data ClientStPoll query err result point tip m a where
  -- | Send a query and handle the response.
  SendMsgPoll
    :: ClientStNext query err result point tip m a
    -- ^ A handler for the server response
    -> ClientStPoll query err result point tip m a
  -- | Send a termination message
  SendMsgCancel
    :: ClientStIdle query point tip m a
    -- ^ A handler for the server response
    -> ClientStPoll query err result point tip m a

-- | Transform the query, point, and tip types in the client.
mapChainSeekClient
  :: forall query query' point point' tip tip' m a
   . (Functor m)
  => (forall err result. query err result -> query' err result)
  -> (point' -> point)
  -> (tip' -> tip)
  -> ChainSeekClient query point tip m a
  -> ChainSeekClient query' point' tip' m a
mapChainSeekClient mapQuery cmapPoint cmapTip ChainSeekClient{..} =
  ChainSeekClient $ mapIdle <$> runChainSeekClient
  where
    mapIdle (SendMsgQueryNext q next) = SendMsgQueryNext (mapQuery q) (mapNext next)
    mapIdle (SendMsgDone a) = SendMsgDone a

    mapNext
      :: forall err result
       . ClientStNext query err result point tip m a
      -> ClientStNext query' err result point' tip' m a
    mapNext ClientStNext{..} =
      ClientStNext
        { recvMsgQueryRejected = \err tip -> mapIdle <$> recvMsgQueryRejected err (cmapTip tip)
        , recvMsgRollForward = \result point tip -> mapIdle <$> recvMsgRollForward result (cmapPoint point) (cmapTip tip)
        , recvMsgRollBackward = \point tip -> mapIdle <$> recvMsgRollBackward (cmapPoint point) (cmapTip tip)
        , recvMsgWait = mapPoll <$> recvMsgWait
        }

    mapPoll
      :: forall err result
       . ClientStPoll query err result point tip m a
      -> ClientStPoll query' err result point' tip' m a
    mapPoll = \case
      SendMsgPoll next -> SendMsgPoll $ mapNext next
      SendMsgCancel idle -> SendMsgCancel $ mapIdle idle

-- | Change the underlying monad with a natural transformation.
hoistChainSeekClient
  :: forall query point tip m n a
   . (Functor m)
  => (forall x. m x -> n x)
  -> ChainSeekClient query point tip m a
  -> ChainSeekClient query point tip n a
hoistChainSeekClient f ChainSeekClient{..} =
  ChainSeekClient $ f $ hoistIdle <$> runChainSeekClient
  where
    hoistIdle :: ClientStIdle query point tip m a -> ClientStIdle query point tip n a
    hoistIdle (SendMsgQueryNext q next) = SendMsgQueryNext q (hoistNext next)
    hoistIdle (SendMsgDone a) = SendMsgDone a

    hoistNext
      :: forall err result
       . ClientStNext query err result point tip m a
      -> ClientStNext query err result point tip n a
    hoistNext ClientStNext{..} =
      ClientStNext
        { recvMsgQueryRejected = \err tip -> f $ hoistIdle <$> recvMsgQueryRejected err tip
        , recvMsgRollForward = \result point tip -> f $ hoistIdle <$> recvMsgRollForward result point tip
        , recvMsgRollBackward = \point tip -> f $ hoistIdle <$> recvMsgRollBackward point tip
        , recvMsgWait = f $ hoistPoll <$> recvMsgWait
        }

    hoistPoll
      :: forall err result
       . ClientStPoll query err result point tip m a
      -> ClientStPoll query err result point tip n a
    hoistPoll = \case
      SendMsgPoll next -> SendMsgPoll $ hoistNext next
      SendMsgCancel idle -> SendMsgCancel $ hoistIdle idle

chainSeekClientPeer
  :: forall query point tip m a
   . (Functor m, Query query)
  => ChainSeekClient query point tip m a
  -> PeerTraced (ChainSeek query point tip) 'AsClient 'StIdle m a
chainSeekClientPeer = EffectTraced . fmap peerIdle . runChainSeekClient
  where
    peerIdle
      :: ClientStIdle query point tip m a
      -> PeerTraced (ChainSeek query point tip) 'AsClient 'StIdle m a
    peerIdle = \case
      SendMsgQueryNext query next ->
        YieldTraced (ClientAgency TokIdle) (MsgQueryNext query) $
          Call (ServerAgency $ TokNext $ tagFromQuery query) $
            peerNext (tagFromQuery query) next
      SendMsgDone a ->
        YieldTraced (ClientAgency TokIdle) MsgDone $
          Close TokDone a

    peerNext
      :: Tag query err result
      -> ClientStNext query err result point tip m a
      -> Message (ChainSeek query point tip) ('StNext err result) st
      -> PeerTraced (ChainSeek query point tip) 'AsClient st m a
    peerNext tag ClientStNext{..} =
      EffectTraced . \case
        MsgRejectQuery err tip -> peerIdle <$> recvMsgQueryRejected err tip
        MsgRollForward result point tip -> peerIdle <$> recvMsgRollForward result point tip
        MsgRollBackward point tip -> peerIdle <$> recvMsgRollBackward point tip
        MsgWait -> peerPoll tag <$> recvMsgWait

    peerPoll
      :: Tag query err result
      -> ClientStPoll query err result point tip m a
      -> PeerTraced (ChainSeek query point tip) 'AsClient ('StPoll err result) m a
    peerPoll tag = \case
      SendMsgPoll next ->
        YieldTraced (ClientAgency TokPoll) MsgPoll $
          Call (ServerAgency $ TokNext tag) $
            peerNext tag next
      SendMsgCancel idle ->
        YieldTraced (ClientAgency TokPoll) MsgCancel $
          Cast $
            peerIdle idle

serveChainSeekClient
  :: forall query point tip m a b
   . (Monad m)
  => ChainSeekServer query point tip m a
  -> ChainSeekClient query point tip m b
  -> m (a, b)
serveChainSeekClient ChainSeekServer{..} ChainSeekClient{..} =
  join $ serveIdle <$> runChainSeekServer <*> runChainSeekClient
  where
    serveIdle
      :: ServerStIdle query point tip m a
      -> ClientStIdle query point tip m b
      -> m (a, b)
    serveIdle ServerStIdle{..} = \case
      SendMsgQueryNext q next -> serveNext next =<< recvMsgQueryNext q
      SendMsgDone b -> (,b) <$> recvMsgDone

    serveNext
      :: ClientStNext query err result point tip m b
      -> ServerStNext query err result point tip m a
      -> m (a, b)
    serveNext ClientStNext{..} = \case
      SendMsgRollForward result point tip idle -> serveIdle idle =<< recvMsgRollForward result point tip
      SendMsgRollBackward point tip idle -> serveIdle idle =<< recvMsgRollBackward point tip
      SendMsgQueryRejected err tip idle -> serveIdle idle =<< recvMsgQueryRejected err tip
      SendMsgWait poll -> servePoll poll =<< recvMsgWait

    servePoll
      :: ServerStPoll query err result point tip m a
      -> ClientStPoll query err result point tip m b
      -> m (a, b)
    servePoll ServerStPoll{..} = \case
      SendMsgPoll next -> serveNext next =<< recvMsgPoll
      SendMsgCancel idle -> flip serveIdle idle =<< recvMsgCancel
