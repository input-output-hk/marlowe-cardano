{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

-- | A view of the chain sync protocol from the point of view of the
-- server. This provides a simplified interface for implementing the server
-- role of the protocol. The types should be much easier to use than the
-- underlying typed protocol types.
module Network.Protocol.ChainSeek.Server where

import Data.Bifunctor (Bifunctor (..))
import Network.Protocol.ChainSeek.Types
import Network.Protocol.Peer.Trace
import Network.TypedProtocol (PeerHasAgency (..))
import Network.TypedProtocol.Core (PeerRole (..))

-- | A chain sync protocol server that runs in some monad 'm'.
newtype ChainSeekServer query point tip m a = ChainSeekServer
  { runChainSeekServer :: m (ServerStIdle query point tip m a)
  }
  deriving (Functor)

-- | In the `StIdle` protocol state, the server does not have agency. Instead,
-- it is waiting to handle either:
--
-- * A query next update request
-- * A scan request
-- * A termination message
--
-- It must be prepared to handle either.
data ServerStIdle query point tip m a = ServerStIdle
  { recvMsgQueryNext
      :: forall err result
       . query err result
      -> m (ServerStNext query err result point tip m a)
  , recvMsgScan
      :: forall err result
       . query err result
      -> m (ServerStScan query err result point tip m a)
  , recvMsgDone :: m a
  }
  deriving (Functor)

-- | In the `StNext` protocol state, the server has agency. It must send
-- either:
--
-- * A query rejection response
-- * A roll forward response
-- * A roll backward response
-- * A wait response
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

deriving instance (Functor m) => Functor (ServerStNext query err result point tip m)

-- | In the `StCollect` protocol state, the server has agency. It must send
-- either:
--
-- * A collect failed response
-- * A collected response
-- * A roll backward response
-- * A wait response
data ServerStCollect query err result point tip m a where
  -- | Reject the scan with an error message.
  SendMsgCollectFailed
    :: err
    -> tip
    -> ServerStIdle query point tip m a
    -> ServerStCollect query err result point tip m a
  -- | Send a sequence of query responses and advance the client to the latest point in the results.
  SendMsgCollected
    :: [(point, result)]
    -> tip
    -> ServerStScan query err result point tip m a
    -> ServerStCollect query err result point tip m a
  -- | Roll the client back to a previous point.
  SendMsgCollectRollBackward
    :: point
    -> tip
    -> ServerStIdle query point tip m a
    -> ServerStCollect query err result point tip m a
  -- | Tell the client to wait
  SendMsgCollectWait
    :: tip
    -> ServerStPoll query err result point tip m a
    -> ServerStCollect query err result point tip m a

deriving instance (Functor m) => Functor (ServerStCollect query err result point tip m)

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
  deriving (Functor)

-- | In the `StScan` protocol state, the server does not have agency. Instead,
-- it is waiting to handle either:
--
-- * A collect message
-- * A cancel collect message
--
-- It must be prepared to handle either.
data ServerStScan query err result point tip m a = ServerStScan
  { recvMsgCollect :: m (ServerStCollect query err result point tip m a)
  , recvMsgCancelScan :: m (ServerStIdle query point tip m a)
  }
  deriving (Functor)

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
        , recvMsgScan = fmap mapScan . recvMsgScan . cmapQuery
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

    mapScan
      :: forall err result
       . ServerStScan query err result point tip m a
      -> ServerStScan query' err result point' tip' m a
    mapScan ServerStScan{..} =
      ServerStScan
        { recvMsgCollect = fmap mapCollect recvMsgCollect
        , recvMsgCancelScan = fmap mapIdle recvMsgCancelScan
        }

    mapCollect
      :: forall err result
       . ServerStCollect query err result point tip m a
      -> ServerStCollect query' err result point' tip' m a
    mapCollect (SendMsgCollectFailed err tip idle) = SendMsgCollectFailed err (mapTip tip) $ mapIdle idle
    mapCollect (SendMsgCollected results tip scan) = SendMsgCollected (first mapPoint <$> results) (mapTip tip) $ mapScan scan
    mapCollect (SendMsgCollectRollBackward point tip idle) = SendMsgCollectRollBackward (mapPoint point) (mapTip tip) $ mapIdle idle
    mapCollect (SendMsgCollectWait tip poll) = SendMsgCollectWait (mapTip tip) $ mapPoll poll

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
        , recvMsgScan = f . fmap hoistScan . recvMsgScan
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

    hoistCollect
      :: forall err result
       . ServerStCollect query err result point tip m a
      -> ServerStCollect query err result point tip n a
    hoistCollect (SendMsgCollectFailed err tip idle) = SendMsgCollectFailed err tip $ hoistIdle idle
    hoistCollect (SendMsgCollected results tip scan) = SendMsgCollected results tip $ hoistScan scan
    hoistCollect (SendMsgCollectRollBackward point tip idle) = SendMsgCollectRollBackward point tip $ hoistIdle idle
    hoistCollect (SendMsgCollectWait tip mnext) = SendMsgCollectWait tip $ hoistPoll mnext

    hoistPoll
      :: forall err result
       . ServerStPoll query err result point tip m a
      -> ServerStPoll query err result point tip n a
    hoistPoll ServerStPoll{..} =
      ServerStPoll
        { recvMsgPoll = f $ fmap hoistNext recvMsgPoll
        , recvMsgCancel = f $ fmap hoistIdle recvMsgCancel
        }

    hoistScan
      :: forall err result
       . ServerStScan query err result point tip m a
      -> ServerStScan query err result point tip n a
    hoistScan ServerStScan{..} =
      ServerStScan
        { recvMsgCollect = f $ fmap hoistCollect recvMsgCollect
        , recvMsgCancelScan = f $ fmap hoistIdle recvMsgCancelScan
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
      MsgScan query ->
        Receive $ EffectTraced $ peerScan (tagFromQuery query) <$> recvMsgScan query
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

    peerScan
      :: forall err result
       . Tag query err result
      -> ServerStScan query err result point tip m a
      -> PeerTraced (ChainSeek query point tip) 'AsServer ('StScan err result :: ChainSeek query point tip) m a
    peerScan tag ServerStScan{..} = AwaitTraced (ClientAgency TokScan) \case
      MsgCollect ->
        Respond (ServerAgency $ TokCollect tag) $
          peerCollect tag <$> recvMsgCollect
      MsgCancelScan ->
        Receive $
          EffectTraced $
            peerIdle <$> recvMsgCancelScan

    peerCollect
      :: forall err result
       . Tag query err result
      -> ServerStCollect query err result point tip m a
      -> Response (ChainSeek query point tip) 'AsServer ('StCollect err result :: ChainSeek query point tip) m a
    peerCollect tag = \case
      SendMsgCollectFailed err tip idle ->
        Response (MsgCollectFailed err tip) $ peerIdle idle
      SendMsgCollected results tip scan ->
        Response (MsgCollected results tip) $ peerScan tag scan
      SendMsgCollectRollBackward point tip idle ->
        Response (MsgCollectRollBackward point tip) $ peerIdle idle
      SendMsgCollectWait tip poll ->
        Response (MsgCollectWait tip) $ peerPoll tag poll
