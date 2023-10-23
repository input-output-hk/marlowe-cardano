{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}

-- | A view of the chain sync protocol from the point of view of the
-- server. This provides a simplified interface for implementing the server
-- role of the protocol. The types should be much easier to use than the
-- underlying typed protocol types.
module Network.Protocol.ChainSeek.Server where

import Control.Monad.Event.Class (MonadEvent)
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Text (Text)
import Network.Protocol.ChainSeek.Types
import Network.Protocol.Driver.Trace (HasSpanContext (..))
import qualified Network.Protocol.Peer.Monad as P
import Observe.Event (InjectSelector, NewEventArgs (..))
import Observe.Event.Backend (Event (..), simpleNewEventArgs)
import Observe.Event.Render.OpenTelemetry (OTelRendered (..), RenderSelectorOTel)
import OpenTelemetry.Attributes (Attribute, ToAttribute (..))
import OpenTelemetry.Trace.Core (SpanKind (..))

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

data ChainSeekServerSelector query point tip f where
  GetNextBlock
    :: query err result
    -> ChainSeekServerSelector query point tip (GetNextBlockField query err result point tip)
  Wait :: ChainSeekServerSelector query point tip Int
  CancelWait :: ChainSeekServerSelector query point tip ()
  Scan
    :: query err result
    -> ChainSeekServerSelector query point tip (query err result)
  CancelScan :: ChainSeekServerSelector query point tip ()
  Collect
    :: query err result
    -> ChainSeekServerSelector query point tip (CollectField query err result point tip)
  Done :: ChainSeekServerSelector query point tip ()

data GetNextBlockField query err result point tip
  = GetNextBlockQuery (query err result)
  | GetNextBlockError err
  | GetNextBlockResult result
  | GetNextBlockStartPoint point
  | GetNextBlockEndPoint point
  | GetNextBlockEndTip tip

data CollectField query err result point tip
  = CollectQuery (query err result)
  | CollectError err
  | CollectResults [result]
  | CollectStartPoint point
  | CollectEndPoint point
  | CollectEndTip tip

chainSeekServerPeer
  :: forall query point tip r s m a
   . (Query query, MonadEvent r s m, HasSpanContext r)
  => point
  -> InjectSelector (ChainSeekServerSelector query point tip) s
  -> ChainSeekServer query point tip m a
  -> P.ServerT (ChainSeek query point tip) 'StIdle 'StDone m a
chainSeekServerPeer initialPoint inj server = P.do
  idle <- P.lift $ runChainSeekServer server
  peerIdle initialPoint idle
  where
    peerIdle
      :: point
      -> ServerStIdle query point tip m a
      -> P.ServerT (ChainSeek query point tip) 'StIdle 'StDone m a
    peerIdle pos ServerStIdle{..} = P.await' TokIdle \case
      MsgQueryNext ctx query -> P.do
        let tag = tagFromQuery query
            args =
              (simpleNewEventArgs $ GetNextBlock query)
                { newEventInitialFields =
                    [ GetNextBlockQuery query
                    , GetNextBlockStartPoint pos
                    ]
                , newEventParent = wrapContext <$> ctx
                }
        P.join $ P.withInjectEventArgs inj args \ev -> P.do
          next <- P.lift $ recvMsgQueryNext query
          peerNext pos 0 ev Nothing tag next
      MsgScan ctx query -> P.do
        let args =
              (simpleNewEventArgs $ Scan query)
                { newEventInitialFields = [query]
                , newEventParent = wrapContext <$> ctx
                }
        scan <- P.withInjectEventArgs inj args \_ -> P.lift $ recvMsgScan query
        P.join $ peerScan pos query scan
      MsgDone ctx -> P.do
        let args =
              (simpleNewEventArgs Done)
                { newEventParent = wrapContext <$> ctx
                , newEventInitialFields = [()]
                }
        P.withInjectEventArgs inj args \_ -> P.lift recvMsgDone

    peerNext
      :: forall err result
       . point
      -> Int
      -> Event m r (GetNextBlockField query err result point tip)
      -> Maybe (Event m r Int)
      -> Tag query err result
      -> ServerStNext query err result point tip m a
      -> P.ServerT
          (ChainSeek query point tip)
          ('StNext err result)
          'StIdle
          m
          (P.ServerT (ChainSeek query point tip) 'StIdle 'StDone m a)
    peerNext pos waitCount ev mWaitEv tag = \case
      SendMsgQueryRejected err tip idle -> P.do
        P.lift $ traverse_ (flip addField waitCount) mWaitEv
        P.lift $ addField ev $ GetNextBlockEndPoint pos
        P.lift $ addField ev $ GetNextBlockError err
        P.lift $ addField ev $ GetNextBlockEndTip tip
        P.yield' (TokNext tag) $ MsgRejectQuery err tip
        pure $ peerIdle pos idle
      SendMsgRollForward result point tip idle -> P.do
        P.lift $ traverse_ (flip addField waitCount) mWaitEv
        P.lift $ addField ev $ GetNextBlockResult result
        P.lift $ addField ev $ GetNextBlockEndPoint point
        P.lift $ addField ev $ GetNextBlockEndTip tip
        P.yield' (TokNext tag) $ MsgRollForward result point tip
        pure $ peerIdle point idle
      SendMsgRollBackward point tip idle -> P.do
        P.lift $ traverse_ (flip addField waitCount) mWaitEv
        P.lift $ addField ev $ GetNextBlockEndPoint point
        P.lift $ addField ev $ GetNextBlockEndTip tip
        P.yield' (TokNext tag) $ MsgRollBackward point tip
        pure $ peerIdle point idle
      SendMsgWait poll -> maybe (P.withInjectEventFields inj Wait [0]) (&) mWaitEv \waitEv -> P.do
        P.yield' (TokNext tag) MsgWait
        peerPoll pos (waitCount + 1) ev waitEv tag poll

    peerPoll
      :: forall err result
       . point
      -> Int
      -> Event m r (GetNextBlockField query err result point tip)
      -> Event m r Int
      -> Tag query err result
      -> ServerStPoll query err result point tip m a
      -> P.ServerT
          (ChainSeek query point tip)
          ('StPoll err result)
          'StIdle
          m
          (P.ServerT (ChainSeek query point tip) 'StIdle 'StDone m a)
    peerPoll pos waitCount ev waitEv tag ServerStPoll{..} = P.await' TokPoll \case
      MsgPoll -> P.do
        next <- P.lift recvMsgPoll
        peerNext pos waitCount ev (Just waitEv) tag next
      MsgCancel -> P.withInjectEventFields inj CancelWait [()] \_ -> P.do
        P.lift $ addField waitEv waitCount
        idle <- P.lift recvMsgCancel
        pure $ peerIdle pos idle

    peerScan
      :: forall err result
       . point
      -> query err result
      -> ServerStScan query err result point tip m a
      -> P.ServerT
          (ChainSeek query point tip)
          ('StScan err result)
          'StIdle
          m
          (P.ServerT (ChainSeek query point tip) 'StIdle 'StDone m a)
    peerScan pos query ServerStScan{..} = P.await' TokScan \case
      MsgCollect ctx -> P.do
        let args =
              (simpleNewEventArgs $ Collect query)
                { newEventInitialFields =
                    [ CollectQuery query
                    , CollectStartPoint pos
                    ]
                , newEventParent = wrapContext <$> ctx
                }
        P.withInjectEventArgs inj args \ev -> P.do
          collect <- P.lift recvMsgCollect
          peerCollect pos ev query collect
      MsgCancelScan ctx -> P.do
        let args =
              (simpleNewEventArgs CancelScan)
                { newEventInitialFields = [()]
                , newEventParent = wrapContext <$> ctx
                }
        P.withInjectEventArgs inj args \_ -> P.do
          idle <- P.lift recvMsgCancelScan
          pure $ peerIdle pos idle

    peerCollect
      :: forall err result
       . point
      -> Event m r (CollectField query err result point tip)
      -> query err result
      -> ServerStCollect query err result point tip m a
      -> P.ServerT
          (ChainSeek query point tip)
          ('StCollect err result)
          'StIdle
          m
          (P.ServerT (ChainSeek query point tip) 'StIdle 'StDone m a)
    peerCollect pos ev query = \case
      SendMsgCollectFailed err tip idle -> P.do
        P.lift $ addField ev $ CollectError err
        P.lift $ addField ev $ CollectEndTip tip
        P.yield' (TokCollect $ tagFromQuery query) $ MsgCollectFailed err tip
        pure $ peerIdle pos idle
      SendMsgCollected results tip scan -> P.do
        P.lift $ addField ev $ CollectResults $ snd <$> results
        let endPoint = fst $ last results
        P.lift $ addField ev $ CollectEndPoint endPoint
        P.lift $ addField ev $ CollectEndTip tip
        P.yield' (TokCollect $ tagFromQuery query) $ MsgCollected results tip
        P.lift $ finalize ev Nothing
        peerScan endPoint query scan
      SendMsgCollectRollBackward point tip idle -> P.do
        P.lift $ addField ev $ CollectEndPoint point
        P.lift $ addField ev $ CollectEndTip tip
        P.yield' (TokCollect $ tagFromQuery query) $ MsgCollectRollBackward point tip
        pure $ peerIdle pos idle
      SendMsgCollectWait tip poll -> P.withInjectEventFields inj Wait [0] \waitEv -> P.do
        P.yield' (TokCollect $ tagFromQuery query) $ MsgCollectWait tip
        peerPoll pos 1 (cmapEvent nextToCollect ev) waitEv (tagFromQuery query) poll

nextToCollect
  :: GetNextBlockField query err result point tip
  -> CollectField query err result point tip
nextToCollect = \case
  GetNextBlockEndPoint point -> CollectEndPoint point
  GetNextBlockEndTip tip -> CollectEndTip tip
  GetNextBlockError err -> CollectError err
  GetNextBlockQuery q -> CollectQuery q
  GetNextBlockResult result -> CollectResults [result]
  GetNextBlockStartPoint q -> CollectStartPoint q

cmapEvent :: (g -> f) -> Event m r f -> Event m r g
cmapEvent f Event{..} =
  Event
    { reference
    , addField = addField . f
    , finalize
    }

data ChainSeekQueryOTelRendered query err result = ChainSeekQueryOTelRendered
  { queryName :: Text
  , queryAttributes :: [(Text, Attribute)]
  , errorAttributes :: err -> [(Text, Attribute)]
  , resultAttributes :: result -> [(Text, Attribute)]
  }

type RenderChainSeekQueryOTel query =
  forall err result. query err result -> ChainSeekQueryOTelRendered query err result

renderGetNextBlockField
  :: (Bool -> point -> [(Text, Attribute)])
  -> (tip -> [(Text, Attribute)])
  -> ChainSeekQueryOTelRendered query err result
  -> GetNextBlockField query err result point tip
  -> [(Text, Attribute)]
renderGetNextBlockField pointAttributes tipAttributes ChainSeekQueryOTelRendered{..} = \case
  GetNextBlockQuery _ -> ("chain-seek.query", toAttribute queryName) : queryAttributes
  GetNextBlockError err -> errorAttributes err
  GetNextBlockResult result -> resultAttributes result
  GetNextBlockStartPoint point -> pointAttributes False point
  GetNextBlockEndPoint point -> pointAttributes True point
  GetNextBlockEndTip tip -> tipAttributes tip

renderCollectField
  :: (Bool -> point -> [(Text, Attribute)])
  -> (tip -> [(Text, Attribute)])
  -> ChainSeekQueryOTelRendered query err result
  -> CollectField query err result point tip
  -> [(Text, Attribute)]
renderCollectField pointAttributes tipAttributes ChainSeekQueryOTelRendered{..} = \case
  CollectQuery _ -> ("chain-seek.query", toAttribute queryName) : queryAttributes
  CollectError err -> errorAttributes err
  CollectResults results ->
    ("chain-seek.collect.block-count", toAttribute $ length results)
      : resultAttributes (last results)
  CollectStartPoint point -> pointAttributes False point
  CollectEndPoint point -> pointAttributes True point
  CollectEndTip tip -> tipAttributes tip

renderChainSeekServerSelectorOTel
  :: (Bool -> point -> [(Text, Attribute)])
  -> (tip -> [(Text, Attribute)])
  -> RenderChainSeekQueryOTel query
  -> RenderSelectorOTel (ChainSeekServerSelector query point tip)
renderChainSeekServerSelectorOTel pointAttributes tipAttributes renderQuery = \case
  GetNextBlock query -> case renderQuery query of
    qr@ChainSeekQueryOTelRendered{..} ->
      OTelRendered
        { eventName = "chain_seek/server/getNextBlock " <> queryName
        , eventKind = Server
        , renderField = renderGetNextBlockField pointAttributes tipAttributes qr
        }
  Scan qr -> case renderQuery qr of
    ChainSeekQueryOTelRendered{..} ->
      OTelRendered
        { eventName = "chain_seek/server/scan " <> queryName
        , eventKind = Server
        , renderField = \_ -> ("chain-seek.query", toAttribute queryName) : queryAttributes
        }
  Collect query -> case renderQuery query of
    qr@ChainSeekQueryOTelRendered{..} ->
      OTelRendered
        { eventName = "chain_seek/server/collect " <> queryName
        , eventKind = Internal
        , renderField = renderCollectField pointAttributes tipAttributes qr
        }
  Wait ->
    OTelRendered
      { eventName = "chain_seek/server/wait"
      , eventKind = Internal
      , renderField = \count -> [("chain-seek.poll-count", toAttribute count)]
      }
  CancelWait ->
    OTelRendered
      { eventName = "chain_seek/server/wait/cancel"
      , eventKind = Internal
      , renderField = const []
      }
  CancelScan ->
    OTelRendered
      { eventName = "chain_seek/server/scan/cancel"
      , eventKind = Internal
      , renderField = const []
      }
  Done ->
    OTelRendered
      { eventName = "chain_seek/server/done"
      , eventKind = Server
      , renderField = const []
      }
