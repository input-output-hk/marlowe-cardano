{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

-- | A view of the chain sync protocol from the point of view of the
-- server. This provides a simplified interface for implementing the server
-- role of the protocol. The types should be much easier to use than the
-- underlying typed protocol types.

module Network.Protocol.ChainSeek.Server
  where

import Network.Protocol.ChainSeek.Types
import Network.TypedProtocol (Peer(..), PeerHasAgency(..))
import Network.TypedProtocol.Core (PeerRole(..))

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
    -> m (ServerStIdle query point tip m a)
    -> ServerStNext query err result point tip m a

  -- | Send a query response and advance the client to a new point in the
  -- chain.
  SendMsgRollForward
    :: result
    -> point
    -> tip
    -> m (ServerStIdle query point tip m a)
    -> ServerStNext query err result point tip m a

  -- | Roll the client back to a previous point.
  SendMsgRollBackward
    :: point
    -> tip
    -> m (ServerStIdle query point tip m a)
    -> ServerStNext query err result point tip m a

  -- | Tell the client to wait
  SendMsgWait
    :: m (ServerStPoll query err result point tip m a)
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
   . Functor m
  => (forall err result. query' err result -> query err result)
  -> (point -> point')
  -> (tip -> tip')
  -> ChainSeekServer query point tip m a
  -> ChainSeekServer query' point' tip' m a
mapChainSeekServer cmapQuery mapPoint mapTip =
  ChainSeekServer . fmap mapIdle . runChainSeekServer

  where
    mapIdle :: ServerStIdle query point tip m a -> ServerStIdle query' point' tip' m a
    mapIdle ServerStIdle{..} = ServerStIdle
      { recvMsgQueryNext = fmap mapNext . recvMsgQueryNext . cmapQuery
      , recvMsgDone
      }

    mapNext
      :: forall err result
       . ServerStNext query err result point tip m a
       -> ServerStNext query' err result point' tip' m a
    mapNext (SendMsgQueryRejected err tip idle) = SendMsgQueryRejected err (mapTip tip) $ mapIdle <$> idle
    mapNext (SendMsgRollForward result point tip idle) = SendMsgRollForward result (mapPoint point) (mapTip tip) $ mapIdle <$> idle
    mapNext (SendMsgRollBackward point tip idle) = SendMsgRollBackward (mapPoint point) (mapTip tip) $ mapIdle <$> idle
    mapNext (SendMsgWait mnext) = SendMsgWait $ mapPoll <$> mnext

    mapPoll
      :: forall err result
       . ServerStPoll query err result point tip m a
       -> ServerStPoll query' err result point' tip' m a
    mapPoll ServerStPoll{..} = ServerStPoll
      { recvMsgPoll = fmap mapNext recvMsgPoll
      , recvMsgCancel = fmap mapIdle recvMsgCancel
      }

-- | Change the underlying monad with a natural transformation.
hoistChainSeekServer
  :: forall query point tip m n a
   . Functor m
  => (forall x. m x -> n x)
  -> ChainSeekServer query point tip m a
  -> ChainSeekServer query point tip n a
hoistChainSeekServer f =
  ChainSeekServer . f . fmap hoistIdle . runChainSeekServer

  where
    hoistIdle :: ServerStIdle query point tip m a -> ServerStIdle query point tip n a
    hoistIdle ServerStIdle{..} = ServerStIdle
      { recvMsgQueryNext =  f . fmap hoistNext . recvMsgQueryNext
      , recvMsgDone = f recvMsgDone
      }

    hoistNext
      :: forall err result
       . ServerStNext query err result point tip m a
       -> ServerStNext query err result point tip n a
    hoistNext (SendMsgQueryRejected err tip idle)        = SendMsgQueryRejected err tip $ f $ hoistIdle <$> idle
    hoistNext (SendMsgRollForward result point tip idle) = SendMsgRollForward result point tip $ f $ hoistIdle <$> idle
    hoistNext (SendMsgRollBackward point tip idle)       = SendMsgRollBackward point tip $ f $ hoistIdle <$> idle
    hoistNext (SendMsgWait mnext)                        = SendMsgWait $ f $ hoistPoll <$> mnext

    hoistPoll
      :: forall err result
       . ServerStPoll query err result point tip m a
       -> ServerStPoll query err result point tip n a
    hoistPoll ServerStPoll{..} = ServerStPoll
      { recvMsgPoll =  f $ fmap hoistNext recvMsgPoll
      , recvMsgCancel = f $ fmap hoistIdle recvMsgCancel
      }

-- | Interpret the server as a 'typed-protocols' 'Peer'.
chainSeekServerPeer
  :: forall query point tip m a
   . (Monad m, Query query)
  => ChainSeekServer query point tip m a
  -> Peer (ChainSeek query point tip) 'AsServer 'StIdle m a
chainSeekServerPeer (ChainSeekServer mClient) = peerIdle mClient
  where
  peerIdle
    :: m (ServerStIdle query point tip m a)
    -> Peer (ChainSeek query point tip) 'AsServer 'StIdle m a
  peerIdle = Effect . fmap peerIdle_

  peerIdle_
    :: ServerStIdle query point tip m a
    -> Peer (ChainSeek query point tip) 'AsServer 'StIdle m a
  peerIdle_ ServerStIdle{..} =
    Await (ClientAgency TokIdle) \case
      MsgQueryNext query -> Effect
        $ fmap (peerNext query)
        $ recvMsgQueryNext query
      MsgDone -> Effect $ Done TokDone <$> recvMsgDone

  peerNext
    :: forall err result
     . query err result
    -> ServerStNext query err result point tip m a
    -> Peer (ChainSeek query point tip) 'AsServer ('StNext err result) m a
  peerNext query = \case
    SendMsgQueryRejected err tip mIdle       -> yield (MsgRejectQuery err tip) mIdle
    SendMsgRollForward result point tip mIdle -> yield (MsgRollForward result point tip) mIdle
    SendMsgRollBackward point tip mIdle       -> yield (MsgRollBackward point tip) mIdle
    SendMsgWait mPoll                    ->
      Yield (ServerAgency (TokNext (tagFromQuery query))) MsgWait $
      Effect $ peerPoll query <$> mPoll
    where
      yield msg = Yield (ServerAgency (TokNext (tagFromQuery query))) msg . peerIdle

  peerPoll
    :: forall err result
     . query err result
    -> ServerStPoll query err result point tip m a
    -> Peer (ChainSeek query point tip) 'AsServer ('StPoll err result) m a
  peerPoll query ServerStPoll{..} =
    Await (ClientAgency TokPoll) \case
      MsgPoll -> Effect $ fmap (peerNext query) recvMsgPoll
      MsgCancel -> Effect $ fmap peerIdle_ recvMsgCancel
