{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

module Network.Protocol.FilteredChainSync.Server where

import Data.Bifunctor (Bifunctor (bimap))
import Network.Protocol.FilteredChainSync.Types (ClientHasAgency (..), FilteredChainSync (..), Message (..),
                                                 NobodyHasAgency (..), SchemaVersion, ServerHasAgency (..),
                                                 StNextKind (..), TokNextKind (..))
import Network.TypedProtocol (Peer (..), PeerHasAgency (..))
import Network.TypedProtocol.Core (PeerRole (..))

newtype FilteredChainSyncServer query point tip m a = FilteredChainSyncServer
  { runFilteredChainSyncServer :: m (ServerStInit query point tip m a)
  }

newtype ServerStInit query point tip m a = ServerStInit
  { recvMsgRequestHandshake :: SchemaVersion -> m (ServerStHandshake query point tip m a)
  }

data ServerStHandshake query point tip m a where
  SendMsgHandshakeRejected  :: [SchemaVersion] -> m a -> ServerStHandshake query point tip m a
  SendMsgHandshakeConfirmed :: m (ServerStIdle query point tip m a) -> ServerStHandshake query point tip m a


data ServerStIdle query point tip m a = ServerStIdle
  { recvMsgQueryNext
    :: forall err result
     . query err result
    -> m ( Either
            (ServerStNext query err result point tip m a)
            (m (ServerStNext query err result point tip m a))
         )
  , recvMsgDone :: m a
  }

data ServerStNext query err result point tip m a where
  SendMsgQueryRejected
    :: err
    -> tip
    -> m (ServerStIdle query point tip m a)
    -> ServerStNext query err result point tip m a
  SendMsgRollForward
    :: result
    -> point
    -> tip
    -> m (ServerStIdle query point tip m a)
    -> ServerStNext query err result point tip m a
  SendMsgRollBackward
    :: point
    -> tip
    -> m (ServerStIdle query point tip m a)
    -> ServerStNext query err result point tip m a

mapFilteredChainSyncServer
  :: forall query query' point point' tip tip' m a
   . Functor m
  => (forall err result. query' err result -> query err result)
  -> (point -> point')
  -> (tip -> tip')
  -> FilteredChainSyncServer query point tip m a
  -> FilteredChainSyncServer query' point' tip' m a
mapFilteredChainSyncServer cmapQuery mapPoint mapTip =
  FilteredChainSyncServer . fmap mapInit . runFilteredChainSyncServer

  where
    mapInit = ServerStInit . (fmap . fmap) mapHandshake . recvMsgRequestHandshake

    mapHandshake :: ServerStHandshake query point tip m a -> ServerStHandshake query' point' tip' m a
    mapHandshake (SendMsgHandshakeRejected vs m)  = SendMsgHandshakeRejected vs m
    mapHandshake (SendMsgHandshakeConfirmed idle) = SendMsgHandshakeConfirmed $ mapIdle <$> idle

    mapIdle :: ServerStIdle query point tip m a -> ServerStIdle query' point' tip' m a
    mapIdle ServerStIdle{..} = ServerStIdle
      { recvMsgQueryNext = fmap (bimap mapNext (fmap mapNext)) . recvMsgQueryNext . cmapQuery
      , recvMsgDone
      }

    mapNext
      :: forall err result
       . ServerStNext query err result point tip m a
       -> ServerStNext query' err result point' tip' m a
    mapNext (SendMsgQueryRejected err tip idle) = SendMsgQueryRejected err (mapTip tip) $ mapIdle <$> idle
    mapNext (SendMsgRollForward result point tip idle) = SendMsgRollForward result (mapPoint point) (mapTip tip) $ mapIdle <$> idle
    mapNext (SendMsgRollBackward point tip idle) = SendMsgRollBackward (mapPoint point) (mapTip tip) $ mapIdle <$> idle

hoistFilteredChainSyncServer
  :: forall query point tip m n a
   . Functor m
  => (forall x. m x -> n x)
  -> FilteredChainSyncServer query point tip m a
  -> FilteredChainSyncServer query point tip n a
hoistFilteredChainSyncServer f =
  FilteredChainSyncServer . f . fmap hoistInit . runFilteredChainSyncServer

  where
    hoistInit :: ServerStInit query point tip m a -> ServerStInit query point tip n a
    hoistInit = ServerStInit . fmap (f . fmap hoistHandshake) . recvMsgRequestHandshake

    hoistHandshake :: ServerStHandshake query point tip m a -> ServerStHandshake query point tip n a
    hoistHandshake (SendMsgHandshakeRejected vs m)  = SendMsgHandshakeRejected vs $ f m
    hoistHandshake (SendMsgHandshakeConfirmed idle) = SendMsgHandshakeConfirmed $ f $ hoistIdle <$> idle

    hoistIdle :: ServerStIdle query point tip m a -> ServerStIdle query point tip n a
    hoistIdle ServerStIdle{..} = ServerStIdle
      { recvMsgQueryNext =  f . fmap (bimap hoistNext (f . fmap hoistNext)) . recvMsgQueryNext
      , recvMsgDone = f recvMsgDone
      }

    hoistNext
      :: forall err result
       . ServerStNext query err result point tip m a
       -> ServerStNext query err result point tip n a
    hoistNext (SendMsgQueryRejected err tip idle)        = SendMsgQueryRejected err tip $ f $ hoistIdle <$> idle
    hoistNext (SendMsgRollForward result point tip idle) = SendMsgRollForward result point tip $ f $ hoistIdle <$> idle
    hoistNext (SendMsgRollBackward point tip idle)       = SendMsgRollBackward point tip $ f $ hoistIdle <$> idle

filteredChainSyncServerPeer
  :: forall query point tip m a
   . Monad m
  => point
  -> FilteredChainSyncServer query point tip m a
  -> Peer (FilteredChainSync query point tip) 'AsServer 'StInit m a
filteredChainSyncServerPeer initialPoint (FilteredChainSyncServer mclient) =
  Effect $ peerInit <$> mclient
  where
  peerInit
    :: ServerStInit query point tip m a
    -> Peer (FilteredChainSync query point tip) 'AsServer 'StInit m a
  peerInit ServerStInit{..} = Await (ClientAgency TokInit) \case
    MsgRequestHandshake version -> Effect $ peerHandshake <$> recvMsgRequestHandshake version

  peerHandshake
    :: ServerStHandshake query point tip m a
    -> Peer (FilteredChainSync query point tip) 'AsServer 'StHandshake m a
  peerHandshake = \case
    SendMsgHandshakeRejected versions ma ->
      Yield (ServerAgency TokHandshake) (MsgRejectHandshake versions) $
      Effect $ Done TokFault <$> ma
    SendMsgHandshakeConfirmed midle ->
      Yield (ServerAgency TokHandshake) MsgConfirmHandshake $
      peerIdle initialPoint midle

  peerIdle
    :: point
    -> m (ServerStIdle query point tip m a)
    -> Peer (FilteredChainSync query point tip) 'AsServer 'StIdle m a
  peerIdle pos = Effect . fmap (peerIdle_ pos)

  peerIdle_
    :: point
    -> ServerStIdle query point tip m a
    -> Peer (FilteredChainSync query point tip) 'AsServer 'StIdle m a
  peerIdle_ pos ServerStIdle{..} =
    Await (ClientAgency TokIdle) \case
      MsgQueryNext query -> Effect
        $ fmap (either (peerNext TokCanAwait pos query) (peerWait pos query))
        $ recvMsgQueryNext query
      MsgDone -> Effect $ Done TokDone <$> recvMsgDone

  peerWait
    :: forall err result
     . point
    -> query err result
    -> m (ServerStNext query err result point tip m a)
    -> Peer (FilteredChainSync query point tip) 'AsServer ('StNext err result 'StCanAwait) m a
  peerWait pos query mnext =
    Yield (ServerAgency (TokNext query TokCanAwait)) MsgWait $
    Effect $ peerNext TokMustReply pos query <$> mnext


  peerNext
    :: forall err result wait
     . TokNextKind wait
    -> point
    -> query err result
    -> ServerStNext query err result point tip m a
    -> Peer (FilteredChainSync query point tip) 'AsServer ('StNext err result wait) m a
  peerNext tok pos query = \case
    SendMsgQueryRejected err tip midle       -> yield pos (MsgRejectQuery err tip) midle
    SendMsgRollForward result pos' tip midle -> yield pos' (MsgRollForward result pos' tip) midle
    SendMsgRollBackward pos' tip midle       -> yield pos' (MsgRollBackward pos' tip) midle
    where
      yield pos' msg = Yield (ServerAgency (TokNext query tok)) msg . peerIdle pos'
