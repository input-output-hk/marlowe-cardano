{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

-- | A view of the chain sync protocol from the point of view of the
-- server. This provides a simplified interface for implementing the server
-- role of the protocol. The types should be much easier to use than the
-- underlying typed protocol types.
module Network.Protocol.ChainSeek.Server where

import Network.Protocol.ChainSeek.Types
import Network.Protocol.Peer.Trace
import Network.TypedProtocol (PeerHasAgency (..))
import Network.TypedProtocol.Core (PeerRole (..))

-- | A chain sync protocol server that runs in some monad 'm'.
newtype ChainSeekServer query point tip m a = ChainSeekServer
  { runChainSeekServer :: m (ServerStIdle query point tip m a)
  }

-- | In the `StIdle` protocol state, the server does not have agency. Instead,
-- it is waiting to handle either:
--
-- * A query next update request
-- * A termination message
--
-- It must be prepared to handle either.
data ServerStIdle query point tip m a = ServerStIdle
  { recvMsgQueryNext
      :: forall err result
       . query err result
      -> m (ServerStNext query err result point tip m a)
  , recvMsgDone :: m a
  }

-- | In the `StNext` protocol state, the server has agency. It must send
-- either:
--
-- * A query rejection response
-- * A roll forward response
-- * A roll backward response
data ServerStNext query err result point tip m a where
  -- | Reject the query with an error message.
  SendMsgQueryRejected
    :: err
    -> tip
    -> ServerStIdle query point tip m a
    -> ServerStNext query err result point tip m a
  -- | Send a query response and advance the client to a new point in the
  -- chain.
  SendMsgRollForward
    :: result
    -> point
    -> tip
    -> ServerStIdle query point tip m a
    -> ServerStNext query err result point tip m a
  -- | Roll the client back to a previous point.
  SendMsgRollBackward
    :: point
    -> tip
    -> ServerStIdle query point tip m a
    -> ServerStNext query err result point tip m a
  -- | Tell the client to wait
  SendMsgWait
    :: ServerStPoll query err result point tip m a
    -> ServerStNext query err result point tip m a

-- | In the `StPoll` protocol state, the server does not have agency. Instead,
-- it is waiting to handle either:
--
-- * A poll message
-- * A cancel poll message
--
-- It must be prepared to handle either.
data ServerStPoll query err result point tip m a = ServerStPoll
  { recvMsgPoll :: m (ServerStNext query err result point tip m a)
  , recvMsgCancel :: m (ServerStIdle query point tip m a)
  }

-- | Transform the query, point, and tip types in the server.
mapChainSeekServer
  :: forall query query' point point' tip tip' m a
   . (Functor m)
  => (forall err result. query' err result -> query err result)
  -> (point -> point')
  -> (tip -> tip')
  -> ChainSeekServer query point tip m a
  -> ChainSeekServer query' point' tip' m a
mapChainSeekServer cmapQuery mapPoint mapTip =
  ChainSeekServer . fmap mapIdle . runChainSeekServer
  where
    mapIdle :: ServerStIdle query point tip m a -> ServerStIdle query' point' tip' m a
    mapIdle ServerStIdle{..} =
      ServerStIdle
        { recvMsgQueryNext = fmap mapNext . recvMsgQueryNext . cmapQuery
        , recvMsgDone
        }

    mapNext
      :: forall err result
       . ServerStNext query err result point tip m a
      -> ServerStNext query' err result point' tip' m a
    mapNext (SendMsgQueryRejected err tip idle) = SendMsgQueryRejected err (mapTip tip) $ mapIdle idle
    mapNext (SendMsgRollForward result point tip idle) = SendMsgRollForward result (mapPoint point) (mapTip tip) $ mapIdle idle
    mapNext (SendMsgRollBackward point tip idle) = SendMsgRollBackward (mapPoint point) (mapTip tip) $ mapIdle idle
    mapNext (SendMsgWait mnext) = SendMsgWait $ mapPoll mnext

    mapPoll
      :: forall err result
       . ServerStPoll query err result point tip m a
      -> ServerStPoll query' err result point' tip' m a
    mapPoll ServerStPoll{..} =
      ServerStPoll
        { recvMsgPoll = fmap mapNext recvMsgPoll
        , recvMsgCancel = fmap mapIdle recvMsgCancel
        }

-- | Change the underlying monad with a natural transformation.
hoistChainSeekServer
  :: forall query point tip m n a
   . (Functor m)
  => (forall x. m x -> n x)
  -> ChainSeekServer query point tip m a
  -> ChainSeekServer query point tip n a
hoistChainSeekServer f =
  ChainSeekServer . f . fmap hoistIdle . runChainSeekServer
  where
    hoistIdle :: ServerStIdle query point tip m a -> ServerStIdle query point tip n a
    hoistIdle ServerStIdle{..} =
      ServerStIdle
        { recvMsgQueryNext = f . fmap hoistNext . recvMsgQueryNext
        , recvMsgDone = f recvMsgDone
        }

    hoistNext
      :: forall err result
       . ServerStNext query err result point tip m a
      -> ServerStNext query err result point tip n a
    hoistNext (SendMsgQueryRejected err tip idle) = SendMsgQueryRejected err tip $ hoistIdle idle
    hoistNext (SendMsgRollForward result point tip idle) = SendMsgRollForward result point tip $ hoistIdle idle
    hoistNext (SendMsgRollBackward point tip idle) = SendMsgRollBackward point tip $ hoistIdle idle
    hoistNext (SendMsgWait mnext) = SendMsgWait $ hoistPoll mnext

    hoistPoll
      :: forall err result
       . ServerStPoll query err result point tip m a
      -> ServerStPoll query err result point tip n a
    hoistPoll ServerStPoll{..} =
      ServerStPoll
        { recvMsgPoll = f $ fmap hoistNext recvMsgPoll
        , recvMsgCancel = f $ fmap hoistIdle recvMsgCancel
        }

chainSeekServerPeer
  :: forall query point tip m a
   . (Functor m, Query query)
  => ChainSeekServer query point tip m a
  -> PeerTraced (ChainSeek query point tip) 'AsServer 'StIdle m a
chainSeekServerPeer = EffectTraced . fmap peerIdle . runChainSeekServer
  where
    peerIdle
      :: ServerStIdle query point tip m a
      -> PeerTraced (ChainSeek query point tip) 'AsServer 'StIdle m a
    peerIdle ServerStIdle{..} = AwaitTraced (ClientAgency TokIdle) \case
      MsgQueryNext query ->
        Respond (ServerAgency $ TokNext $ tagFromQuery query) $
          peerNext (tagFromQuery query) <$> recvMsgQueryNext query
      MsgDone ->
        Closed TokDone recvMsgDone

    peerNext
      :: forall err result
       . Tag query err result
      -> ServerStNext query err result point tip m a
      -> Response (ChainSeek query point tip) 'AsServer ('StNext err result :: ChainSeek query point tip) m a
    peerNext tag = \case
      SendMsgQueryRejected err tip idle ->
        Response (MsgRejectQuery err tip) $ peerIdle idle
      SendMsgRollForward result point tip idle ->
        Response (MsgRollForward result point tip) $ peerIdle idle
      SendMsgRollBackward point tip idle ->
        Response (MsgRollBackward point tip) $ peerIdle idle
      SendMsgWait poll ->
        Response MsgWait $ peerPoll tag poll

    peerPoll
      :: forall err result
       . Tag query err result
      -> ServerStPoll query err result point tip m a
      -> PeerTraced (ChainSeek query point tip) 'AsServer ('StPoll err result) m a
    peerPoll tag ServerStPoll{..} = AwaitTraced (ClientAgency TokPoll) \case
      MsgPoll ->
        Respond (ServerAgency $ TokNext tag) $
          peerNext tag <$> recvMsgPoll
      MsgCancel ->
        Receive $
          EffectTraced $
            peerIdle <$> recvMsgCancel
