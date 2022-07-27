{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

module Network.Protocol.FilteredChainSync.Client where

import Network.Protocol.FilteredChainSync.Types (ClientHasAgency (..), FilteredChainSync (..), Message (..),
                                                 NobodyHasAgency (..), SchemaVersion, ServerHasAgency (..),
                                                 TokNextKind (..))
import Network.TypedProtocol (Peer (..), PeerHasAgency (..))
import Network.TypedProtocol.Core (PeerRole (..))

newtype FilteredChainSyncClient query point tip m a = FilteredChainSyncClient
  { runFilteredChainSyncClient :: m (ClientStInit query point tip m a)
  }

data ClientStInit query point tip m a
  = SendMsgRequestHandshake SchemaVersion (ClientStHandshake query point tip m a)

data ClientStHandshake query point tip m a = ClientStHandshake
  { recvMsgHandshakeRejected  :: [SchemaVersion] -> m a
  , recvMsgHandshakeConfirmed :: m (ClientStIdle query point tip m a)
  }

data ClientStIdle query point tip m a where
  SendMsgQueryNext
    :: query err result
    -> ClientStNext query err result point tip m a
    -> m (ClientStNext query err result point tip m a)
    -> ClientStIdle query point tip m a

  SendMsgDone
    :: a
    -> ClientStIdle query point tip m a

data ClientStNext query err result point tip m a = ClientStNext
  { recvMsgQueryRejected :: err -> tip -> m (ClientStIdle query point tip m a)
  , recvMsgRollForward   :: result -> point -> tip -> m (ClientStIdle query point tip m a)
  , recvMsgRollBackward  :: point -> tip -> m (ClientStIdle query point tip m a)
  }

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
  peerInit (SendMsgRequestHandshake schemaVersion ClientStHandshake{..}) =
    Yield (ClientAgency TokInit) (MsgRequestHandshake schemaVersion) $
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
  peerIdle_ pos (SendMsgQueryNext query next waitNext) =
    Yield (ClientAgency TokIdle) (MsgQueryNext pos query) $
    Await (ServerAgency (TokNext query TokCanAwait)) \case
      MsgRejectQuery err tip          -> peerIdle pos $ recvMsgQueryRejected next err tip
      MsgRollForward result pos' tip -> peerIdle pos' $ recvMsgRollForward next result pos' tip
      MsgRollBackward pos' tip -> peerIdle pos' $ recvMsgRollBackward next pos' tip
      MsgWait -> Effect do
        ClientStNext{..} <- waitNext
        pure $ Await (ServerAgency (TokNext query TokMustReply)) \case
          MsgRejectQuery err tip         -> peerIdle pos $ recvMsgQueryRejected err tip
          MsgRollForward result pos' tip -> peerIdle pos' $ recvMsgRollForward result pos' tip
          MsgRollBackward pos' tip       -> peerIdle pos' $ recvMsgRollBackward pos' tip

  peerIdle_ _ (SendMsgDone a) = Yield (ClientAgency TokIdle) MsgDone (Done TokDone a)
