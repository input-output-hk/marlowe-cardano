{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- | A view of the filtered chain sync protocol from the point of view of the
-- client. This provides a simplified interface for implementing the client
-- role of the protocol. The types should be much easier to use than the
-- underlying typed protocol types.

module Network.Protocol.ChainSeek.Client
  where

import Network.Protocol.ChainSeek.Types
import Network.TypedProtocol (Peer(..), PeerHasAgency(..))
import Network.TypedProtocol.Core (PeerRole(..))

-- | A filtered chain sync protocol client that runs in some monad 'm'.
newtype ChainSeekClient query point tip m a = ChainSeekClient
  { runChainSeekClient :: m (ClientStInit query point tip m a)
  }

-- | In the 'StInit' protocol state, the client has agency. It must send a
-- handshake request message.
data ClientStInit query point tip m a
  = SendMsgRequestHandshake SchemaVersion (ClientStHandshake query point tip m a)

-- | In the 'StHandshake' protocol state, the client does not have agency.
-- Instead, it must be prepared to handle either:
--
-- * a handshake rejection message
-- * a handshake confirmation message
data ClientStHandshake query point tip m a = ClientStHandshake
  { recvMsgHandshakeRejected  :: [SchemaVersion] -> m a
  , recvMsgHandshakeConfirmed :: m (ClientStIdle query point tip m a)
  }

-- | In the `StIdle` protocol state, the client has agency. It must send
-- either:
--
-- * A query next update request
-- * A termination message
data ClientStIdle query point tip m a where

  -- | Send a query and handle the response.
  SendMsgQueryNext
    :: query err result                                -- ^ The query
    -> ClientStNext query err result point tip m a     -- ^ A handler for the server response
    -> ClientStIdle query point tip m a

  -- | Send a termination message
  SendMsgDone
    :: a                                -- The result of running the protocol
    -> ClientStIdle query point tip m a

-- | In the `StNext` protocol state, the client does not have agency. Instead,
-- it must be prepared to handle either:
--
-- * A query rejection response
-- * A roll forward response
-- * A roll backward response
data ClientStNext query err result point tip m a = ClientStNext
  { recvMsgQueryRejected :: err -> tip -> m (ClientStIdle query point tip m a)
  , recvMsgRollForward   :: result -> point -> tip -> m (ClientStIdle query point tip m a)
  , recvMsgRollBackward  :: point -> tip -> m (ClientStIdle query point tip m a)
  , recvMsgWait          :: m (ClientStPoll query err result point tip m a)
  }

-- | In the `StPoll` protocol state, the client has agency. It must send
-- either:
--
-- * A poll message
-- * A cancel message
data ClientStPoll query err result point tip m a where

  -- | Send a query and handle the response.
  SendMsgPoll
    :: ClientStNext query err result point tip m a     -- ^ A handler for the server response
    -> ClientStPoll query err result point tip m a

  -- | Send a termination message
  SendMsgCancel
    :: ClientStIdle query point tip m a     -- ^ A handler for the server response
    -> ClientStPoll query err result point tip m a

-- | Transform the query, point, and tip types in the client.
mapChainSeekClient
  :: forall query query' point point' tip tip' m a
   . Functor m
  => (forall err result. query err result -> query' err result)
  -> (point' -> point)
  -> (tip' -> tip)
  -> ChainSeekClient query point tip m a
  -> ChainSeekClient query' point' tip' m a
mapChainSeekClient mapQuery cmapPoint cmapTip ChainSeekClient{..} =
  ChainSeekClient $ mapInit <$> runChainSeekClient

  where
    mapInit (SendMsgRequestHandshake v handshake) = SendMsgRequestHandshake v $ mapHandshake handshake

    mapHandshake ClientStHandshake{..} = ClientStHandshake
      { recvMsgHandshakeRejected
      , recvMsgHandshakeConfirmed = mapIdle <$> recvMsgHandshakeConfirmed
      }

    mapIdle (SendMsgQueryNext q next) = SendMsgQueryNext (mapQuery q) (mapNext next)
    mapIdle (SendMsgDone a) = SendMsgDone a

    mapNext
      :: forall err result
       . ClientStNext query err result point tip m a
       -> ClientStNext query' err result point' tip' m a
    mapNext ClientStNext{..} = ClientStNext
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
   . Functor m
  => (forall x. m x -> n x)
  -> ChainSeekClient query point tip m a
  -> ChainSeekClient query point tip n a
hoistChainSeekClient f ChainSeekClient{..} =
  ChainSeekClient $ f $ hoistInit <$> runChainSeekClient

  where
    hoistInit :: ClientStInit query point tip m a -> ClientStInit query point tip n a
    hoistInit (SendMsgRequestHandshake v idle) = SendMsgRequestHandshake v $ hoistHandshake idle

    hoistHandshake :: ClientStHandshake query point tip m a -> ClientStHandshake query point tip n a
    hoistHandshake ClientStHandshake{..} = ClientStHandshake
      { recvMsgHandshakeRejected = f <$> recvMsgHandshakeRejected
      , recvMsgHandshakeConfirmed = f $ hoistIdle <$> recvMsgHandshakeConfirmed
      }

    hoistIdle :: ClientStIdle query point tip m a -> ClientStIdle query point tip n a
    hoistIdle (SendMsgQueryNext q next) = SendMsgQueryNext q (hoistNext next)
    hoistIdle (SendMsgDone a) = SendMsgDone a

    hoistNext
      :: forall err result
       . ClientStNext query err result point tip m a
       -> ClientStNext query err result point tip n a
    hoistNext ClientStNext{..} = ClientStNext
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

-- | Interpret the client as a 'typed-protocols' 'Peer'.
chainSeekClientPeer
  :: forall query point tip m a
   . (Monad m, Query query)
  => point
  -> ChainSeekClient query point tip m a
  -> Peer (ChainSeek query point tip) 'AsClient 'StInit m a
chainSeekClientPeer initialPoint (ChainSeekClient mClient) =
  Effect $ peerInit <$> mClient
  where
  peerInit
    :: ClientStInit query point tip m a
    -> Peer (ChainSeek query point tip) 'AsClient 'StInit m a
  peerInit (SendMsgRequestHandshake schemaVersion handshake) =
    Yield (ClientAgency TokInit) (MsgRequestHandshake schemaVersion) $ peerHandshake handshake

  peerHandshake
    :: ClientStHandshake query point tip m a
    -> Peer (ChainSeek query point tip) 'AsClient 'StHandshake m a
  peerHandshake ClientStHandshake{..} =
    Await (ServerAgency TokHandshake) \case
      MsgRejectHandshake versions -> Effect $ Done TokDone <$> recvMsgHandshakeRejected versions
      MsgConfirmHandshake         -> peerIdle initialPoint recvMsgHandshakeConfirmed

  peerIdle
    :: point
    -> m (ClientStIdle query point tip m a)
    -> Peer (ChainSeek query point tip) 'AsClient 'StIdle m a
  peerIdle pos = Effect . fmap (peerIdle_ pos)

  peerIdle_
    :: point
    -> ClientStIdle query point tip m a
    -> Peer (ChainSeek query point tip) 'AsClient 'StIdle m a
  peerIdle_ pos = \case
    SendMsgQueryNext query next ->
      Yield (ClientAgency TokIdle) (MsgQueryNext query) $ peerNext pos query next
    SendMsgDone a -> Yield (ClientAgency TokIdle) MsgDone (Done TokDone a)

  peerNext
    :: point
    -> query err result
    -> ClientStNext query err result point tip m a
    -> Peer (ChainSeek query point tip) 'AsClient ('StNext err result) m a
  peerNext pos query ClientStNext{..} =
    Await (ServerAgency (TokNext (tagFromQuery query))) \case
      MsgRejectQuery err tip         -> peerIdle pos $ recvMsgQueryRejected err tip
      MsgRollForward result pos' tip -> peerIdle pos' $ recvMsgRollForward result pos' tip
      MsgRollBackward pos' tip       -> peerIdle pos' $ recvMsgRollBackward pos' tip
      MsgWait                        -> peerPoll pos query recvMsgWait

  peerPoll
    :: forall err result
     . point
    -> query err result
    -> m (ClientStPoll query err result point tip m a)
    -> Peer (ChainSeek query point tip) 'AsClient ('StPoll err result) m a
  peerPoll pos query mnext = Effect do
    poll <- mnext
    pure case poll of
      SendMsgPoll next -> Yield (ClientAgency TokPoll) MsgPoll $ peerNext pos query next
      SendMsgCancel idle -> Yield (ClientAgency TokPoll) MsgCancel $ peerIdle_ pos idle
