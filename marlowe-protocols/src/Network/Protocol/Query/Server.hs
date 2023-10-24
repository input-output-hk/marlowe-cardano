{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}

-- | A generic server for the query protocol. Includes a function for
-- interpreting a server as a typed-protocols peer that can be executed with a
-- driver and a codec.
module Network.Protocol.Query.Server where

import Control.Arrow ((***))
import Control.Monad.Event.Class (MonadEvent)
import Data.Text (Text)
import Network.Protocol.Driver.Trace (HasSpanContext (wrapContext))
import qualified Network.Protocol.Peer.Monad as P
import Network.Protocol.Peer.Trace
import Network.Protocol.Query.Types
import Network.TypedProtocol
import Observe.Event (InjectSelector, NewEventArgs (newEventInitialFields, newEventParent), addField)
import Observe.Event.Backend (simpleNewEventArgs)
import Observe.Event.Render.OpenTelemetry (OTelRendered (..), RenderSelectorOTel)
import OpenTelemetry.Attributes (Attribute)
import OpenTelemetry.Trace.Core (SpanKind (..))

-- | A generic server for the query protocol.
newtype QueryServer req m a = QueryServer {runQueryServer :: m (ServerStReq req m a)}
  deriving (Functor)

data ServerStReq req m a = ServerStReq
  { recvMsgRequest :: forall x. ReqTree req x -> m (x, ServerStReq req m a)
  , recvMsgDone :: m a
  }
  deriving (Functor)

-- | Change the underlying monad type a server runs in with a natural
-- transformation.
hoistQueryServer
  :: forall req m n a
   . (Functor m)
  => (forall x. m x -> n x)
  -> QueryServer req m a
  -> QueryServer req n a
hoistQueryServer f = QueryServer . f . fmap hoistReq . runQueryServer
  where
    hoistReq ServerStReq{..} =
      ServerStReq
        { recvMsgRequest = f . (fmap . fmap) hoistReq . recvMsgRequest
        , recvMsgDone = f recvMsgDone
        }

-- | Interpret a server as a typed-protocols peer.
queryServerPeer
  :: forall req m a
   . (Functor m, Request req)
  => QueryServer req m a
  -> PeerTraced (Query req) 'AsServer 'StReq m a
queryServerPeer QueryServer{..} =
  EffectTraced $ peerReq <$> runQueryServer
  where
    peerReq :: ServerStReq req m a -> PeerTraced (Query req) 'AsServer 'StReq m a
    peerReq ServerStReq{..} =
      AwaitTraced (ClientAgency TokReq) \case
        MsgRequest _ req ->
          Respond (ServerAgency $ TokRes $ tagFromReq req) $
            uncurry Response . (MsgRespond *** peerReq) <$> recvMsgRequest req
        MsgDone _ -> Closed TokDone recvMsgDone

data QueryServerSelector req f where
  RespondSel :: ReqTree req a -> QueryServerSelector req (RequestField req a)
  DoneSel :: QueryServerSelector req ()

data RequestField req a
  = RequestReq (ReqTree req a)
  | RequestRes a

-- | Interpret a server as a peerT monad transformer.
queryServerPeerT
  :: forall req r s m a
   . (Request req, HasSpanContext r, MonadEvent r s m)
  => InjectSelector (QueryServerSelector req) s
  -> QueryServer req m a
  -> P.ServerT (Query req) 'StReq 'StDone m a
queryServerPeerT inj QueryServer{..} = P.do
  server <- P.lift runQueryServer
  peerReq server
  where
    peerReq
      :: ServerStReq req m a
      -> P.ServerT (Query req) 'StReq 'StDone m a
    peerReq ServerStReq{..} = P.await' TokReq \case
      MsgRequest ctx req -> do
        let args =
              (simpleNewEventArgs $ RespondSel req)
                { newEventInitialFields = [RequestReq req]
                , newEventParent = wrapContext <$> ctx
                }
        P.join $ P.withInjectEventArgs inj args \ev -> P.do
          (response, next) <- P.lift $ recvMsgRequest req
          P.lift $ addField ev $ RequestRes response
          P.yield' (TokRes $ tagFromReq req) $ MsgRespond response
          pure $ peerReq next
      MsgDone ctx -> do
        let args =
              (simpleNewEventArgs DoneSel)
                { newEventInitialFields = [()]
                , newEventParent = wrapContext <$> ctx
                }
        P.withInjectEventArgs inj args \_ -> P.lift recvMsgDone

respond
  :: forall req m
   . (Applicative m)
  => (forall a b. m a -> m b -> m (a, b))
  -> (forall a. req a -> m a)
  -> QueryServer req m ()
respond merge handle = QueryServer $ pure server
  where
    server =
      ServerStReq
        { recvMsgDone = pure ()
        , recvMsgRequest = fmap (,server) . handle'
        }
    handle' :: ReqTree req a -> m a
    handle' = \case
      ReqLeaf req -> handle req
      ReqBin l r -> handle' l `merge` handle' r

type RenderRequestOTel req = forall a. req a -> RequestRenderedOTel req a

data RequestRenderedOTel req a = RequestRenderedOTel
  { requestName :: Text
  , requestAttributes :: [(Text, Attribute)]
  , responseAttributes :: a -> [(Text, Attribute)]
  }

renderReqTreeOTel :: RenderRequestOTel req -> ReqTree req a -> RequestRenderedOTel req a
renderReqTreeOTel renderRequest = \case
  ReqBin l r -> case renderReqTreeOTel renderRequest l of
    lRendered -> case renderReqTreeOTel renderRequest r of
      rRendered ->
        RequestRenderedOTel
          { requestName = requestName lRendered <> "; " <> requestName rRendered
          , requestAttributes = requestAttributes lRendered <> requestAttributes rRendered
          , responseAttributes = \(a, b) -> responseAttributes lRendered a <> responseAttributes rRendered b
          }
  ReqLeaf req -> case renderRequest req of
    RequestRenderedOTel{..} ->
      RequestRenderedOTel
        { requestName
        , requestAttributes = do
            (attrName, attr) <- requestAttributes
            pure ("query.request." <> requestName <> "." <> attrName, attr)
        , responseAttributes = \a -> do
            (attrName, attr) <- responseAttributes a
            pure ("query.response." <> requestName <> "." <> attrName, attr)
        }

renderQueryServerSelectorOTel :: RenderRequestOTel req -> RenderSelectorOTel (QueryServerSelector req)
renderQueryServerSelectorOTel renderRequest = \case
  DoneSel ->
    OTelRendered
      { eventName = "query/server/done"
      , eventKind = Server
      , renderField = const []
      }
  RespondSel req -> case renderReqTreeOTel renderRequest req of
    RequestRenderedOTel{..} ->
      OTelRendered
        { eventName = "query/server/respond " <> requestName
        , eventKind = Server
        , renderField = \case
            RequestReq _ -> requestAttributes
            RequestRes a -> responseAttributes a
        }
