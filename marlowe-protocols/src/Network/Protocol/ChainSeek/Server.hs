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
  { runChainSeekServer :: m (ServerStInit query point tip m a)
  }

-- | In the 'StInit' protocol state, the server does not have agency. Instead,
-- it is waiting to handle a handshake request from the client, which it must
-- handle.
newtype ServerStInit query point tip m a = ServerStInit
  { recvMsgRequestHandshake :: SchemaVersion -> m (ServerStHandshake query point tip m a)
  }

-- | In the 'StHandshake' protocol state, the server has agency. It must send
-- either:
--
-- * a handshake rejection message
-- * a handshake confirmation message
data ServerStHandshake query point tip m a where

  -- | Reject the handshake request
  SendMsgHandshakeRejected
    :: [SchemaVersion] -- ^ A list of supported schema versions.
    -> a               -- ^ The result of running the protocol.
    -> ServerStHandshake query point tip m a

  -- | Accept the handshake request
  SendMsgHandshakeConfirmed
    :: m (ServerStIdle query point tip m a) -- ^ An action that computes the idle handlers.
    -> ServerStHandshake query point tip m a


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
  ChainSeekServer . fmap mapInit . runChainSeekServer

  where
    mapInit = ServerStInit . (fmap . fmap) mapHandshake . recvMsgRequestHandshake

    mapHandshake :: ServerStHandshake query point tip m a -> ServerStHandshake query' point' tip' m a
    mapHandshake (SendMsgHandshakeRejected vs m)  = SendMsgHandshakeRejected vs m
    mapHandshake (SendMsgHandshakeConfirmed idle) = SendMsgHandshakeConfirmed $ mapIdle <$> idle

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
  ChainSeekServer . f . fmap hoistInit . runChainSeekServer

  where
    hoistInit :: ServerStInit query point tip m a -> ServerStInit query point tip n a
    hoistInit = ServerStInit . fmap (f . fmap hoistHandshake) . recvMsgRequestHandshake

    hoistHandshake :: ServerStHandshake query point tip m a -> ServerStHandshake query point tip n a
    hoistHandshake (SendMsgHandshakeRejected vs a)  = SendMsgHandshakeRejected vs a
    hoistHandshake (SendMsgHandshakeConfirmed idle) = SendMsgHandshakeConfirmed $ f $ hoistIdle <$> idle

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
  => point
  -> ChainSeekServer query point tip m a
  -> Peer (ChainSeek query point tip) 'AsServer 'StInit m a
chainSeekServerPeer initialPoint (ChainSeekServer mClient) =
  Effect $ peerInit <$> mClient
  where
  peerInit
    :: ServerStInit query point tip m a
    -> Peer (ChainSeek query point tip) 'AsServer 'StInit m a
  peerInit ServerStInit{..} = Await (ClientAgency TokInit) \case
    MsgRequestHandshake version -> Effect $ peerHandshake <$> recvMsgRequestHandshake version

  peerHandshake
    :: ServerStHandshake query point tip m a
    -> Peer (ChainSeek query point tip) 'AsServer 'StHandshake m a
  peerHandshake = \case
    SendMsgHandshakeRejected versions ma ->
      Yield (ServerAgency TokHandshake) (MsgRejectHandshake versions) $
      Done TokDone ma
    SendMsgHandshakeConfirmed mIdle ->
      Yield (ServerAgency TokHandshake) MsgConfirmHandshake $
      peerIdle initialPoint mIdle

  peerIdle
    :: point
    -> m (ServerStIdle query point tip m a)
    -> Peer (ChainSeek query point tip) 'AsServer 'StIdle m a
  peerIdle pos = Effect . fmap (peerIdle_ pos)

  peerIdle_
    :: point
    -> ServerStIdle query point tip m a
    -> Peer (ChainSeek query point tip) 'AsServer 'StIdle m a
  peerIdle_ pos ServerStIdle{..} =
    Await (ClientAgency TokIdle) \case
      MsgQueryNext query -> Effect
        $ fmap (peerNext pos query)
        $ recvMsgQueryNext query
      MsgDone -> Effect $ Done TokDone <$> recvMsgDone

  peerNext
    :: forall err result
     . point
    -> query err result
    -> ServerStNext query err result point tip m a
    -> Peer (ChainSeek query point tip) 'AsServer ('StNext err result) m a
  peerNext pos query = \case
    SendMsgQueryRejected err tip mIdle       -> yield pos (MsgRejectQuery err tip) mIdle
    SendMsgRollForward result pos' tip mIdle -> yield pos' (MsgRollForward result pos' tip) mIdle
    SendMsgRollBackward pos' tip mIdle       -> yield pos' (MsgRollBackward pos' tip) mIdle
    SendMsgWait mPoll                    ->
      Yield (ServerAgency (TokNext (tagFromQuery query))) MsgWait $
      Effect $ peerPoll pos query <$> mPoll
    where
      yield pos' msg = Yield (ServerAgency (TokNext (tagFromQuery query))) msg . peerIdle pos'

  peerPoll
    :: forall err result
     . point
    -> query err result
    -> ServerStPoll query err result point tip m a
    -> Peer (ChainSeek query point tip) 'AsServer ('StPoll err result) m a
  peerPoll pos query ServerStPoll{..} =
    Await (ClientAgency TokPoll) \case
      MsgPoll -> Effect $ fmap (peerNext pos query) recvMsgPoll
      MsgCancel -> Effect $ fmap (peerIdle_ pos) recvMsgCancel
