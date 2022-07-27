{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

-- | A view of the filtered chain sync protocol from the point of view of the
-- client. This provides a simplified interface for implementing the client
-- role of the protocol. The types should be much easier to use than the
-- underlying typed protocol types.

module Network.Protocol.FilteredChainSync.Client where

import Network.Protocol.FilteredChainSync.Types (ClientHasAgency (..), FilteredChainSync (..), Message (..),
                                                 NobodyHasAgency (..), SchemaVersion, ServerHasAgency (..),
                                                 StNextKind (..), TokNextKind (..))
import Network.TypedProtocol (Peer (..), PeerHasAgency (..))
import Network.TypedProtocol.Core (PeerRole (..))

-- | A filtered chain sync protocol client that runs in some monad 'm'.
newtype FilteredChainSyncClient query point tip m a = FilteredChainSyncClient
  { runFilteredChainSyncClient :: m (ClientStInit query point tip m a)
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
    -> ClientStNext query err result point tip m a     -- ^ A handler for when the server responds immediately
    -> m (ClientStNext query err result point tip m a) -- ^ A handler for when the server indicates the client will need to wait for a response
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
  }

-- | Transform the query, point, and tip types in the client.
mapFilteredChainSyncClient
  :: forall query query' point point' tip tip' m a
   . Functor m
  => (forall err result. query err result -> query' err result)
  -> (point' -> point)
  -> (tip' -> tip)
  -> FilteredChainSyncClient query point tip m a
  -> FilteredChainSyncClient query' point' tip' m a
mapFilteredChainSyncClient mapQuery cmapPoint cmapTip FilteredChainSyncClient{..} =
  FilteredChainSyncClient $ mapInit <$> runFilteredChainSyncClient

  where
    mapInit (SendMsgRequestHandshake v handshake) = SendMsgRequestHandshake v $ mapHandshake handshake

    mapHandshake ClientStHandshake{..} = ClientStHandshake
      { recvMsgHandshakeRejected
      , recvMsgHandshakeConfirmed = mapIdle <$> recvMsgHandshakeConfirmed
      }

    mapIdle (SendMsgQueryNext q next next') = SendMsgQueryNext (mapQuery q) (mapNext next) (mapNext <$> next')
    mapIdle (SendMsgDone a)                 = SendMsgDone a

    mapNext
      :: forall err result
       . ClientStNext query err result point tip m a
       -> ClientStNext query' err result point' tip' m a
    mapNext ClientStNext{..} = ClientStNext
      { recvMsgQueryRejected = \err tip -> mapIdle <$> recvMsgQueryRejected err (cmapTip tip)
      , recvMsgRollForward = \result point tip -> mapIdle <$> recvMsgRollForward result (cmapPoint point) (cmapTip tip)
      , recvMsgRollBackward = \point tip -> mapIdle <$> recvMsgRollBackward (cmapPoint point) (cmapTip tip)
      }

-- | Change the underlying monad with a natural transformation.
hoistFilteredChainSyncClient
  :: forall query point tip m n a
   . Functor m
  => (forall x. m x -> n x)
  -> FilteredChainSyncClient query point tip m a
  -> FilteredChainSyncClient query point tip n a
hoistFilteredChainSyncClient f FilteredChainSyncClient{..} =
  FilteredChainSyncClient $ f $ hoistInit <$> runFilteredChainSyncClient

  where
    hoistInit :: ClientStInit query point tip m a -> ClientStInit query point tip n a
    hoistInit (SendMsgRequestHandshake v idle) = SendMsgRequestHandshake v $ hoistHandshake idle

    hoistHandshake :: ClientStHandshake query point tip m a -> ClientStHandshake query point tip n a
    hoistHandshake ClientStHandshake{..} = ClientStHandshake
      { recvMsgHandshakeRejected = f <$> recvMsgHandshakeRejected
      , recvMsgHandshakeConfirmed = f $ hoistIdle <$> recvMsgHandshakeConfirmed
      }

    hoistIdle :: ClientStIdle query point tip m a -> ClientStIdle query point tip n a
    hoistIdle (SendMsgQueryNext q next next') = SendMsgQueryNext q (hoistNext next) (f $ hoistNext <$> next')
    hoistIdle (SendMsgDone a)                 = SendMsgDone a

    hoistNext
      :: forall err result
       . ClientStNext query err result point tip m a
       -> ClientStNext query err result point tip n a
    hoistNext ClientStNext{..} = ClientStNext
      { recvMsgQueryRejected = \err tip -> f $ hoistIdle <$> recvMsgQueryRejected err tip
      , recvMsgRollForward = \result point tip -> f $ hoistIdle <$> recvMsgRollForward result point tip
      , recvMsgRollBackward = \point tip -> f $ hoistIdle <$> recvMsgRollBackward point tip
      }

-- | Interpret the client as a 'typed-protocols' 'Peer'.
filteredChainSyncClientPeer
  :: forall query point tip m a
   . Monad m
  => point
  -> FilteredChainSyncClient query point tip m a
  -> Peer (FilteredChainSync query point tip) 'AsClient 'StInit m a
filteredChainSyncClientPeer initialPoint (FilteredChainSyncClient mclient) =
  Effect $ peerInit <$> mclient
  where
  peerInit
    :: ClientStInit query point tip m a
    -> Peer (FilteredChainSync query point tip) 'AsClient 'StInit m a
  peerInit (SendMsgRequestHandshake schemaVersion handshake) =
    Yield (ClientAgency TokInit) (MsgRequestHandshake schemaVersion) $ peerHandshake handshake

  peerHandshake
    :: ClientStHandshake query point tip m a
    -> Peer (FilteredChainSync query point tip) 'AsClient 'StHandshake m a
  peerHandshake ClientStHandshake{..} =
    Await (ServerAgency TokHandshake) \case
      MsgRejectHandshake versions -> Effect $ Done TokFault <$> recvMsgHandshakeRejected versions
      MsgConfirmHandshake         -> peerIdle initialPoint recvMsgHandshakeConfirmed

  peerIdle
    :: point
    -> m (ClientStIdle query point tip m a)
    -> Peer (FilteredChainSync query point tip) 'AsClient 'StIdle m a
  peerIdle pos = Effect . fmap (peerIdle_ pos)

  peerIdle_
    :: point
    -> ClientStIdle query point tip m a
    -> Peer (FilteredChainSync query point tip) 'AsClient 'StIdle m a
  peerIdle_ pos = \case
    SendMsgQueryNext query ClientStNext{..} waitNext ->
      Yield (ClientAgency TokIdle) (MsgQueryNext query) $
      Await (ServerAgency (TokNext query TokCanAwait)) \case
        MsgRejectQuery err tip         -> peerIdle pos $ recvMsgQueryRejected err tip
        MsgRollForward result pos' tip -> peerIdle pos' $ recvMsgRollForward result pos' tip
        MsgRollBackward pos' tip       -> peerIdle pos' $ recvMsgRollBackward pos' tip
        MsgWait                        -> peerWait pos query waitNext

    SendMsgDone a -> Yield (ClientAgency TokIdle) MsgDone (Done TokDone a)

  peerWait
    :: forall err result
     . point
    -> query err result
    -> m (ClientStNext query err result point tip m a)
    -> Peer (FilteredChainSync query point tip) 'AsClient ('StNext err result 'StMustReply) m a
  peerWait pos query mnext = Effect do
    ClientStNext{..} <- mnext
    pure $ Await (ServerAgency (TokNext query TokMustReply)) \case
      MsgRejectQuery err tip         -> peerIdle pos $ recvMsgQueryRejected err tip
      MsgRollForward result pos' tip -> peerIdle pos' $ recvMsgRollForward result pos' tip
      MsgRollBackward pos' tip       -> peerIdle pos' $ recvMsgRollBackward pos' tip
