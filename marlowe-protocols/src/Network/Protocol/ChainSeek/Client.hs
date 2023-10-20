{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- | A view of the chain sync protocol from the point of view of the
-- client. This provides a simplified interface for implementing the client
-- role of the protocol. The types should be much easier to use than the
-- underlying typed protocol types.
module Network.Protocol.ChainSeek.Client where

import Control.Monad (join)
import Data.Bifunctor (Bifunctor (..))
import Network.Protocol.ChainSeek.Server
import Network.Protocol.ChainSeek.Types
import Network.Protocol.Peer.Trace
import Network.TypedProtocol (PeerHasAgency (..))
import Network.TypedProtocol.Core (PeerRole (..))

-- | A chain seek protocol client that runs in some monad 'm'.
newtype ChainSeekClient query point tip m a = ChainSeekClient
  { runChainSeekClient :: m (ClientStIdle query point tip m a)
  }
  deriving (Functor)

-- | In the `StIdle` protocol state, the client has agency. It must send
-- either:
--
-- * A query next update request
-- * A scan request
-- * A termination message
data ClientStIdle query point tip m a where
  -- | Send a query and handle the response.
  SendMsgQueryNext
    :: query err result
    -- ^ The query
    -> ClientStNext query err result point tip m a
    -- ^ A handler for the server response
    -> ClientStIdle query point tip m a
  -- | Start a scan.
  SendMsgScan
    :: query err result
    -- ^ The query
    -> ClientStScan query err result point tip m a
    -- ^ The next client
    -> ClientStIdle query point tip m a
  -- | Send a termination message
  SendMsgDone
    :: a -- The result of running the protocol
    -> ClientStIdle query point tip m a

deriving instance (Functor m) => Functor (ClientStIdle query point tip m)

-- | In the `StNext` protocol state, the client does not have agency. Instead,
-- it must be prepared to handle either:
--
-- * A query rejection response
-- * A roll forward response
-- * A roll backward response
-- * A wait response
data ClientStNext query err result point tip m a = ClientStNext
  { recvMsgQueryRejected :: err -> tip -> m (ClientStIdle query point tip m a)
  , recvMsgRollForward :: result -> point -> tip -> m (ClientStIdle query point tip m a)
  , recvMsgRollBackward :: point -> tip -> m (ClientStIdle query point tip m a)
  , recvMsgWait :: m (ClientStPoll query err result point tip m a)
  }
  deriving (Functor)

-- | In the `StPoll` protocol state, the client has agency. It must send
-- either:
--
-- * A poll message
-- * A cancel message
data ClientStPoll query err result point tip m a where
  -- | Check if new results are available.
  SendMsgPoll
    :: ClientStNext query err result point tip m a
    -- ^ A handler for the server response
    -> ClientStPoll query err result point tip m a
  -- | Send a termination message
  SendMsgCancel
    :: ClientStIdle query point tip m a
    -- ^ A handler for the server response
    -> ClientStPoll query err result point tip m a

deriving instance (Functor m) => Functor (ClientStPoll query err result point tip m)

-- | In the `StScan` protocol state, the client has agency. It must send
-- either:
--
-- * A scan message
-- * A cancel message
data ClientStScan query err result point tip m a where
  -- | Collect the next query results
  SendMsgCollect
    :: ClientStCollect query err result point tip m a
    -- ^ A handler for the server response
    -> ClientStScan query err result point tip m a
  -- | Cancel the scan.
  SendMsgCancelScan
    :: ClientStIdle query point tip m a
    -- ^ A handler for the server response
    -> ClientStScan query err result point tip m a

deriving instance (Functor m) => Functor (ClientStScan query err result point tip m)

-- | In the `StCollect` protocol state, the client does not have agency. Instead,
-- it must be prepared to handle either:
--
-- * A query rejection response
-- * A collected response
-- * A roll backward response
-- * A wait response
data ClientStCollect query err result point tip m a = ClientStCollect
  { recvMsgCollectFailed :: err -> tip -> m (ClientStIdle query point tip m a)
  , recvMsgCollected :: [(point, result)] -> tip -> m (ClientStScan query err result point tip m a)
  , recvMsgCollectRollBackward :: point -> tip -> m (ClientStIdle query point tip m a)
  , recvMsgCollectWait :: tip -> m (ClientStPoll query err result point tip m a)
  }
  deriving (Functor)

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
    mapIdle (SendMsgScan q scan) = SendMsgScan (mapQuery q) (mapScan scan)
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

    mapScan
      :: forall err result
       . ClientStScan query err result point tip m a
      -> ClientStScan query' err result point' tip' m a
    mapScan = \case
      SendMsgCollect collect -> SendMsgCollect $ mapCollect collect
      SendMsgCancelScan idle -> SendMsgCancelScan $ mapIdle idle

    mapCollect
      :: forall err result
       . ClientStCollect query err result point tip m a
      -> ClientStCollect query' err result point' tip' m a
    mapCollect ClientStCollect{..} =
      ClientStCollect
        { recvMsgCollectFailed = \err tip -> mapIdle <$> recvMsgCollectFailed err (cmapTip tip)
        , recvMsgCollected = \results tip -> mapScan <$> recvMsgCollected (first cmapPoint <$> results) (cmapTip tip)
        , recvMsgCollectRollBackward = \point tip -> mapIdle <$> recvMsgCollectRollBackward (cmapPoint point) (cmapTip tip)
        , recvMsgCollectWait = fmap mapPoll . recvMsgCollectWait . cmapTip
        }

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
    hoistIdle (SendMsgScan q scan) = SendMsgScan q (hoistScan scan)
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

    hoistScan
      :: forall err result
       . ClientStScan query err result point tip m a
      -> ClientStScan query err result point tip n a
    hoistScan = \case
      SendMsgCollect collect -> SendMsgCollect $ hoistCollect collect
      SendMsgCancelScan idle -> SendMsgCancelScan $ hoistIdle idle

    hoistCollect
      :: forall err result
       . ClientStCollect query err result point tip m a
      -> ClientStCollect query err result point tip n a
    hoistCollect ClientStCollect{..} =
      ClientStCollect
        { recvMsgCollectFailed = \err tip -> f $ hoistIdle <$> recvMsgCollectFailed err tip
        , recvMsgCollected = \results tip -> f $ hoistScan <$> recvMsgCollected results tip
        , recvMsgCollectRollBackward = \point tip -> f $ hoistIdle <$> recvMsgCollectRollBackward point tip
        , recvMsgCollectWait = f . fmap hoistPoll . recvMsgCollectWait
        }

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
      SendMsgScan query scan ->
        YieldTraced (ClientAgency TokIdle) (MsgScan query) $
          Cast $
            peerScan (tagFromQuery query) scan
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

    peerScan
      :: Tag query err result
      -> ClientStScan query err result point tip m a
      -> PeerTraced (ChainSeek query point tip) 'AsClient ('StScan err result) m a
    peerScan tag = \case
      SendMsgCollect next ->
        YieldTraced (ClientAgency TokScan) MsgCollect $
          Call (ServerAgency $ TokCollect tag) $
            peerCollect tag next
      SendMsgCancelScan idle ->
        YieldTraced (ClientAgency TokScan) MsgCancelScan $
          Cast $
            peerIdle idle

    peerCollect
      :: Tag query err result
      -> ClientStCollect query err result point tip m a
      -> Message (ChainSeek query point tip) ('StCollect err result) st
      -> PeerTraced (ChainSeek query point tip) 'AsClient st m a
    peerCollect tag ClientStCollect{..} =
      EffectTraced . \case
        MsgCollectFailed err tip -> peerIdle <$> recvMsgCollectFailed err tip
        MsgCollected results tip -> peerScan tag <$> recvMsgCollected results tip
        MsgCollectRollBackward point tip -> peerIdle <$> recvMsgCollectRollBackward point tip
        MsgCollectWait tip -> peerPoll tag <$> recvMsgCollectWait tip

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
      SendMsgScan q scan -> flip serveScan scan =<< recvMsgScan q
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

    serveScan
      :: ServerStScan query err result point tip m a
      -> ClientStScan query err result point tip m b
      -> m (a, b)
    serveScan ServerStScan{..} = \case
      SendMsgCollect collect -> serveCollect collect =<< recvMsgCollect
      SendMsgCancelScan idle -> flip serveIdle idle =<< recvMsgCancelScan

    serveCollect
      :: ClientStCollect query err result point tip m b
      -> ServerStCollect query err result point tip m a
      -> m (a, b)
    serveCollect ClientStCollect{..} = \case
      SendMsgCollected results tip scan -> serveScan scan =<< recvMsgCollected results tip
      SendMsgCollectRollBackward point tip idle -> serveIdle idle =<< recvMsgCollectRollBackward point tip
      SendMsgCollectFailed err tip idle -> serveIdle idle =<< recvMsgCollectFailed err tip
      SendMsgCollectWait tip poll -> servePoll poll =<< recvMsgCollectWait tip
