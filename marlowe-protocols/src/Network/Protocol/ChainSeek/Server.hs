{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

-- | A view of the chain seek protocol from the point of view of the
-- server. This provides a simplified interface for implementing the server
-- role of the protocol. The types should be much easier to use than the
-- underlying typed protocol types.

module Network.Protocol.ChainSeek.Server where

import Network.Protocol.ChainSeek.Types
import Network.TypedProtocol (Peer(..), PeerHasAgency(..))
import Network.TypedProtocol.Core (PeerRole(..))

-- | A chain seek protocol server that runs in some monad 'm'.
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
    -> m (ServerStNext query err result point tip 'StCanAwait m a)
  , recvMsgDone :: m a
  }

-- | In the `StNext` protocol state, the server has agency. It must send
-- either:
--
-- * A query rejection response
-- * A roll forward response
-- * A roll backward response
data ServerStNext query err result point tip (k :: StNextKind) m a where

  -- | Reject the query with an error message.
  SendMsgQueryRejected
    :: err
    -> tip
    -> m (ServerStIdle query point tip m a)
    -> ServerStNext query err result point tip k m a

  -- | Send a query response and advance the client to a new point in the
  -- chain.
  SendMsgRollForward
    :: result
    -> point
    -> tip
    -> m (ServerStIdle query point tip m a)
    -> ServerStNext query err result point tip k m a

  -- | Roll the client back to a previous point.
  SendMsgRollBackward
    :: point
    -> tip
    -> m (ServerStIdle query point tip m a)
    -> ServerStNext query err result point tip k m a

  -- | Tell the client to wait
  SendMsgWait
    :: m (ServerStNext query err result point tip 'StMustReply m a)
    -> ServerStNext query err result point tip 'StCanAwait m a

  -- | Check if the client is still waiting
  SendMsgPing
    :: m (ServerStNext query err result point tip 'StMustReply m a)
    -> ServerStNext query err result point tip 'StMustReply m a

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
      :: forall err result k
       . ServerStNext query err result point tip k m a
       -> ServerStNext query' err result point' tip' k m a
    mapNext (SendMsgQueryRejected err tip idle) = SendMsgQueryRejected err (mapTip tip) $ mapIdle <$> idle
    mapNext (SendMsgRollForward result point tip idle) = SendMsgRollForward result (mapPoint point) (mapTip tip) $ mapIdle <$> idle
    mapNext (SendMsgRollBackward point tip idle) = SendMsgRollBackward (mapPoint point) (mapTip tip) $ mapIdle <$> idle
    mapNext (SendMsgWait mnext) = SendMsgWait $ mapNext <$> mnext
    mapNext (SendMsgPing mnext) = SendMsgPing $ mapNext <$> mnext

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
      :: forall err result k
       . ServerStNext query err result point tip k m a
       -> ServerStNext query err result point tip k n a
    hoistNext (SendMsgQueryRejected err tip idle)        = SendMsgQueryRejected err tip $ f $ hoistIdle <$> idle
    hoistNext (SendMsgRollForward result point tip idle) = SendMsgRollForward result point tip $ f $ hoistIdle <$> idle
    hoistNext (SendMsgRollBackward point tip idle)       = SendMsgRollBackward point tip $ f $ hoistIdle <$> idle
    hoistNext (SendMsgWait mnext)                        = SendMsgWait $ f $ hoistNext <$> mnext
    hoistNext (SendMsgPing mnext)                        = SendMsgPing $ f $ hoistNext <$> mnext

-- | Interpret the server as a 'typed-protocols' 'Peer'.
chainSeekServerPeer
  :: forall query point tip m a
   . (Monad m, Query query)
  => point
  -> ChainSeekServer query point tip m a
  -> Peer (ChainSeek query point tip) 'AsServer 'StInit m a
chainSeekServerPeer initialPoint (ChainSeekServer mclient) =
  Effect $ peerInit <$> mclient
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
      Done TokFault ma
    SendMsgHandshakeConfirmed midle ->
      Yield (ServerAgency TokHandshake) MsgConfirmHandshake $
      peerIdle initialPoint midle

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
        $ fmap (peerNext TokCanAwait pos query)
        $ recvMsgQueryNext query
      MsgDone -> Effect $ Done TokDone <$> recvMsgDone

  peerNext
    :: forall err result wait
     . TokNextKind wait
    -> point
    -> query err result
    -> ServerStNext query err result point tip wait m a
    -> Peer (ChainSeek query point tip) 'AsServer ('StNext err result wait) m a
  peerNext tok pos query = \case
    SendMsgQueryRejected err tip midle       -> yield pos (MsgRejectQuery err tip) midle
    SendMsgRollForward result pos' tip midle -> yield pos' (MsgRollForward result pos' tip) midle
    SendMsgRollBackward pos' tip midle       -> yield pos' (MsgRollBackward pos' tip) midle
    SendMsgWait mnext                    ->
      Yield (ServerAgency (TokNext (tagFromQuery query) TokCanAwait)) MsgWait $
      Effect $ peerNext TokMustReply pos query <$> mnext
    SendMsgPing mnext                        ->
      Yield (ServerAgency (TokNext (tagFromQuery query) TokMustReply)) MsgPing $
      Await (ClientAgency TokPing) \MsgPong ->
      Effect $ peerNext TokMustReply pos query <$> mnext
    where
      yield pos' msg = Yield (ServerAgency (TokNext (tagFromQuery query) tok)) msg . peerIdle pos'
