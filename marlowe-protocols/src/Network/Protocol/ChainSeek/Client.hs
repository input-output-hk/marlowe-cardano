{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}

-- | A view of the chain sync protocol from the point of view of the
-- client. This provides a simplified interface for implementing the client
-- role of the protocol. The types should be much easier to use than the
-- underlying typed protocol types.
module Network.Protocol.ChainSeek.Client where

import Control.Monad (join)
import Control.Monad.Event.Class (MonadEvent)
import Data.Bifunctor (Bifunctor (..))
import Data.Text (Text)
import Network.Protocol.ChainSeek.Server hiding (CancelScan, Collect, Done, Scan)
import Network.Protocol.ChainSeek.Types
import Network.Protocol.Driver.Trace (HasSpanContext, context)
import qualified Network.Protocol.Peer.Monad as P
import Observe.Event (Event, InjectSelector, NewEventArgs (..), addField, finalize, reference)
import Observe.Event.Backend (simpleNewEventArgs)
import Observe.Event.Render.OpenTelemetry (OTelRendered (..), RenderSelectorOTel)
import OpenTelemetry.Attributes (Attribute, ToAttribute (..))
import OpenTelemetry.Trace.Core (SpanKind (..))
import UnliftIO (MonadIO)

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

data ChainSeekClientSelector query point tip f where
  QueryNext
    :: query err result
    -> ChainSeekClientSelector query point tip (GetNextBlockField query err result point tip)
  Scan
    :: query err result
    -> ChainSeekClientSelector query point tip (query err result)
  CancelScan :: ChainSeekClientSelector query point tip ()
  Collect
    :: query err result
    -> ChainSeekClientSelector query point tip (CollectField query err result point tip)
  Done :: ChainSeekClientSelector query point tip ()

chainSeekClientPeer
  :: forall query point tip r s m a
   . (MonadIO m, MonadEvent r s m, Query query, HasSpanContext r)
  => point
  -> InjectSelector (ChainSeekClientSelector query point tip) s
  -> ChainSeekClient query point tip m a
  -> P.ClientT (ChainSeek query point tip) 'StIdle 'StDone m a
chainSeekClientPeer initialPoint inj client = P.do
  idle <- P.lift $ runChainSeekClient client
  peerIdle initialPoint idle
  where
    peerIdle
      :: point
      -> ClientStIdle query point tip m a
      -> P.ClientT (ChainSeek query point tip) 'StIdle 'StDone m a
    peerIdle pos = \case
      SendMsgQueryNext query next -> P.do
        let tag = tagFromQuery query
        let fields = [GetNextBlockStartPoint pos, GetNextBlockQuery query]
        P.join $ P.withInjectEventFields inj (QueryNext query) fields \ev -> P.do
          ctx <- context $ reference ev
          P.yield' TokIdle $ MsgQueryNext (Just ctx) query
          peerNext pos ev tag next
      SendMsgScan query scan -> P.do
        r <- P.withInjectEventFields inj (Scan query) [query] \ev -> P.do
          let r = reference ev
          ctx <- context r
          P.yield' TokIdle $ MsgScan (Just ctx) query
          pure r
        P.join $ peerScan pos r query scan
      SendMsgDone a -> P.withInjectEventFields inj Done [()] \ev -> P.do
        ctx <- context $ reference ev
        P.yield' TokIdle $ MsgDone $ Just ctx
        pure a

    peerNext
      :: point
      -> Event m r (GetNextBlockField query err result point tip)
      -> Tag query err result
      -> ClientStNext query err result point tip m a
      -> P.ClientT
          (ChainSeek query point tip)
          ('StNext err result)
          'StIdle
          m
          (P.ClientT (ChainSeek query point tip) 'StIdle 'StDone m a)
    peerNext pos ev tag ClientStNext{..} = P.await' (TokNext tag) \case
      MsgRejectQuery err tip -> P.do
        P.lift $ addField ev $ GetNextBlockEndPoint pos
        P.lift $ addField ev $ GetNextBlockError err
        P.lift $ addField ev $ GetNextBlockEndTip tip
        idle <- P.lift $ recvMsgQueryRejected err tip
        pure $ peerIdle pos idle
      MsgRollForward result point tip -> P.do
        P.lift $ addField ev $ GetNextBlockResult result
        P.lift $ addField ev $ GetNextBlockEndPoint point
        P.lift $ addField ev $ GetNextBlockEndTip tip
        idle <- P.lift $ recvMsgRollForward result point tip
        pure $ peerIdle point idle
      MsgRollBackward point tip -> P.do
        P.lift $ addField ev $ GetNextBlockEndPoint point
        P.lift $ addField ev $ GetNextBlockEndTip tip
        idle <- P.lift $ recvMsgRollBackward point tip
        pure $ peerIdle point idle
      MsgWait -> P.do
        poll <- P.lift recvMsgWait
        peerPoll pos ev tag poll

    peerPoll
      :: point
      -> Event m r (GetNextBlockField query err result point tip)
      -> Tag query err result
      -> ClientStPoll query err result point tip m a
      -> P.ClientT
          (ChainSeek query point tip)
          ('StPoll err result)
          'StIdle
          m
          (P.ClientT (ChainSeek query point tip) 'StIdle 'StDone m a)
    peerPoll pos ev tag = \case
      SendMsgPoll next -> P.do
        P.yield' TokPoll MsgPoll
        peerNext pos ev tag next
      SendMsgCancel idle -> P.do
        P.yield' TokPoll MsgCancel
        pure $ peerIdle pos idle

    peerScan
      :: point
      -> r
      -> query err result
      -> ClientStScan query err result point tip m a
      -> P.ClientT
          (ChainSeek query point tip)
          ('StScan err result)
          'StIdle
          m
          (P.ClientT (ChainSeek query point tip) 'StIdle 'StDone m a)
    peerScan pos scanEvRef query = \case
      SendMsgCollect collect -> P.do
        let args =
              (simpleNewEventArgs $ Collect query)
                { newEventParent = Just scanEvRef
                , newEventInitialFields =
                    [ CollectQuery query
                    , CollectStartPoint pos
                    ]
                }
        P.withInjectEventArgs inj args \ev -> P.do
          ctx <- context $ reference ev
          P.yield' TokScan $ MsgCollect $ Just ctx
          peerCollect pos scanEvRef ev query collect
      SendMsgCancelScan idle -> P.do
        let args =
              (simpleNewEventArgs CancelScan)
                { newEventParent = Just scanEvRef
                , newEventInitialFields = [()]
                }
        P.withInjectEventArgs inj args \ev -> P.do
          ctx <- P.lift $ context $ reference ev
          P.yield' TokScan $ MsgCancelScan $ Just ctx
          pure $ peerIdle pos idle

    peerCollect
      :: point
      -> r
      -> Event m r (CollectField query err result point tip)
      -> query err result
      -> ClientStCollect query err result point tip m a
      -> P.ClientT
          (ChainSeek query point tip)
          ('StCollect err result)
          'StIdle
          m
          (P.ClientT (ChainSeek query point tip) 'StIdle 'StDone m a)
    peerCollect pos scanEvRef ev query ClientStCollect{..} = P.await' (TokCollect $ tagFromQuery query) \case
      MsgCollectFailed err tip -> P.do
        P.lift $ addField ev $ CollectError err
        P.lift $ addField ev $ CollectEndTip tip
        idle <- P.lift $ recvMsgCollectFailed err tip
        pure $ peerIdle pos idle
      MsgCollected results tip -> P.do
        P.lift $ addField ev $ CollectResults $ snd <$> results
        let endPoint = fst $ last results
        P.lift $ addField ev $ CollectEndPoint endPoint
        P.lift $ addField ev $ CollectEndTip tip
        scan <- P.lift $ recvMsgCollected results tip
        P.lift $ finalize ev Nothing
        peerScan endPoint scanEvRef query scan
      MsgCollectRollBackward point tip -> P.do
        P.lift $ addField ev $ CollectEndPoint point
        P.lift $ addField ev $ CollectEndTip tip
        idle <- P.lift $ recvMsgCollectRollBackward point tip
        pure $ peerIdle pos idle
      MsgCollectWait tip -> P.do
        poll <- P.lift $ recvMsgCollectWait tip
        peerPoll pos (cmapEvent nextToCollect ev) (tagFromQuery query) poll

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

renderChainSeekClientSelectorOTel
  :: (Bool -> point -> [(Text, Attribute)])
  -> (tip -> [(Text, Attribute)])
  -> RenderChainSeekQueryOTel query
  -> RenderSelectorOTel (ChainSeekClientSelector query point tip)
renderChainSeekClientSelectorOTel pointAttributes tipAttributes renderQuery = \case
  QueryNext query -> case renderQuery query of
    qr@ChainSeekQueryOTelRendered{..} ->
      OTelRendered
        { eventName = "chain_seek/client/getNextBlock " <> queryName
        , eventKind = Client
        , renderField = renderGetNextBlockField pointAttributes tipAttributes qr
        }
  Scan qr -> case renderQuery qr of
    ChainSeekQueryOTelRendered{..} ->
      OTelRendered
        { eventName = "chain_seek/client/scan " <> queryName
        , eventKind = Client
        , renderField = \_ -> ("chain-seek.query", toAttribute queryName) : queryAttributes
        }
  Collect query -> case renderQuery query of
    qr@ChainSeekQueryOTelRendered{..} ->
      OTelRendered
        { eventName = "chain_seek/client/collect " <> queryName
        , eventKind = Internal
        , renderField = renderCollectField pointAttributes tipAttributes qr
        }
  CancelScan ->
    OTelRendered
      { eventName = "chain_seek/client/scan/cancel"
      , eventKind = Internal
      , renderField = const []
      }
  Done ->
    OTelRendered
      { eventName = "chain_seek/client/done"
      , eventKind = Client
      , renderField = const []
      }
