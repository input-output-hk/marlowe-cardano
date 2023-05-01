{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Network.Protocol.Peer.Trace
  where

import Control.Monad (join, replicateM)
import Control.Monad.Event.Class (MonadEvent, emitImmediateInjectEventArgs_, withInjectEventArgs)
import Control.Monad.IO.Class (MonadIO)
import Data.Binary (Binary, get, getWord8, put)
import Data.Binary.Put (putWord8, runPut)
import qualified Data.ByteString as B
import Data.ByteString.Lazy (ByteString)
import Data.Foldable (traverse_)
import Data.Void (Void)
import Network.Channel
import Network.Protocol.Codec (BinaryMessage, DeserializeError, binaryCodec, decodeGet)
import Network.TypedProtocol
import Network.TypedProtocol.Codec
import Observe.Event (Event(addField), InjectSelector, NewEventArgs(..), reference)
import Observe.Event.Backend (simpleNewEventArgs)
import OpenTelemetry.Trace.Core
import OpenTelemetry.Trace.Id (SpanId, TraceId, bytesToSpanId, bytesToTraceId, spanIdBytes, traceIdBytes)
import OpenTelemetry.Trace.TraceState (Key(..), TraceState, Value(..), empty, insert, toList)
import UnliftIO (throwIO)

-- | A peer in a protocol session which is suitable for tracing.
data PeerTraced ps (pr :: PeerRole) (st :: ps) r m a where
  -- | The client is sending a message to the server and expects a response. The
  -- entire call will occur inside a call span.
  Call
    :: ClientHasAgency st
    -- ^ Witness of the proof that the client is allowed to act in this state.
    -> ServerHasAgency st'
    -- ^ Witness of the proof that the server is allowed to act in the next state.
    -> Message ps st st'
    -- ^ Message to send to the server
    -> (forall (st'' :: ps). Message ps st' st'' -> m (PeerTraced ps 'AsClient st'' r m a))
    -- ^ Handler for the response from the server. The action will run inside a
    -- process span, the parent of which will be the respond span from the
    -- server.
    -> PeerTraced ps 'AsClient st r m a

  -- | The peer is sending a message to the other peer.
  -- The sending will occur inside a publish span.
  Publish
    :: WeHaveAgency pr st
    -- ^ Witness of the proof that the peer is allowed to act in this state.
    -> Message ps st st'
    -- ^ Message to send to the server
    -> m (PeerTraced ps pr st' r m a)
    -- ^ Next peer to run. Runs inside a process span, the parent of which will
    -- be the publish span.
    -> PeerTraced ps pr st r m a

  -- | The peer is ready to handle a message from the other peer. The receipt
  -- of the message will run inside a receive span.
  Receive
    :: TheyHaveAgency pr st
    -- ^ Witness of the proof that the other peer is allowed to act in this state.
    -> (forall (st' :: ps). Message ps st st' -> HandleMessage ps pr st' r m a)
    -- ^ Handler for the message from the server. The action will run inside a
    -- process span, the parent of which will be the span carried by the
    -- message.
    -> PeerTraced ps pr st r m a

  -- | The session is complete and the connection will be terminated.
  DoneTraced
    :: NobodyHasAgency st
    -- ^ Witness of the proof that neither peer is allowed to act in this state.
    -> a
    -- ^ The result of the session.
    -> PeerTraced ps pr st r m a

deriving instance Functor m => Functor (PeerTraced ps pr st r m)

data HandleMessage ps pr st r m a where
  Respond
    :: ServerHasAgency st
    -> m (ResponseContinuation ps st r m a)
    -> HandleMessage ps 'AsServer st r m a
  Process
    :: m (PeerTraced ps pr st r m a)
    -> HandleMessage ps pr st r m a

deriving instance Functor m => Functor (HandleMessage ps pr st r m)

data ResponseContinuation ps st r m a where
  ResponseContinuation
    :: Message ps st st'
    -> PeerTraced ps 'AsServer st' r m a
    -> ResponseContinuation ps st r m a

deriving instance Functor m => Functor (ResponseContinuation ps st r m)

data DriverTraced ps dState r m = DriverTraced
  { sendMessageTraced
      :: forall pr st st'
       . r
      -> PeerHasAgency pr st
      -> Message ps st st'
      -> m ()
  , recvMessageTraced
      :: forall pr (st :: ps)
       . PeerHasAgency pr st
      -> dState
      -> m (r, SomeMessage st, dState)
  , startDStateTraced :: dState
  }

data TypedProtocolsSelector ps f where
  CallSelector :: Message ps st st' -> TypedProtocolsSelector ps (SomeMessage st')
  RespondSelector :: Message ps st st' -> Message ps st' st'' -> TypedProtocolsSelector ps ()
  PublishSelector :: Message ps st st' -> TypedProtocolsSelector ps ()
  ReceiveSelector :: TypedProtocolsSelector ps ()
  ProcessSelector :: Message ps st st' -> TypedProtocolsSelector ps ()
  DoneSelector :: TypedProtocolsSelector ps Void

runPeerWithDriverTraced
  :: MonadEvent r s m
  => InjectSelector (TypedProtocolsSelector ps) s
  -> r
  -> DriverTraced ps dState r m
  -> PeerTraced ps pr st r m a
  -> dState
  -> m a
runPeerWithDriverTraced inj parent driver = \case
  Call tok1 tok2 msg k -> runCallPeerWithDriverTraced inj driver parent tok1 tok2 msg k
  Receive tok k -> runReceivePeerWithDriverTraced inj driver parent tok k
  Publish tok msg m -> runPublishPeerWithDriverTraced inj driver parent tok msg m
  DoneTraced _ a -> \_ -> do
    emitImmediateInjectEventArgs_ inj doneArgs
    pure a
  where
    doneArgs = (simpleNewEventArgs DoneSelector) { newEventParent = Just parent }

runCallPeerWithDriverTraced
  :: MonadEvent r s m
  => InjectSelector (TypedProtocolsSelector ps) s
  -> DriverTraced ps dState r m
  -> r
  -> ClientHasAgency st
  -> ServerHasAgency st'
  -> Message ps st st'
  -> (forall st''. Message ps st' st'' -> m (PeerTraced ps 'AsClient st'' r m a))
  -> dState
  -> m a
runCallPeerWithDriverTraced inj driver parent tok1 tok2 msg k dState = do
  (respondRef, SomeMessage msg', dState') <- withInjectEventArgs inj callArgs \ev -> do
    sendMessageTraced driver (reference ev) (ClientAgency tok1) msg
    (respondRef, msg', dState') <- recvMessageTraced driver (ServerAgency tok2) dState
    addField ev msg'
    pure (respondRef, msg', dState')
  join $ withInjectEventArgs inj (processArgs respondRef msg') \ev -> do
    nextPeer <- k msg'
    pure $ runPeerWithDriverTraced inj (reference ev) driver nextPeer dState'
  where
    callArgs = (simpleNewEventArgs (CallSelector msg))
      { newEventParent = Just parent
      }
    processArgs respondRef msg' = (simpleNewEventArgs (ProcessSelector msg'))
      { newEventParent = Just respondRef
      , newEventInitialFields = [()]
      }

runPublishPeerWithDriverTraced
  :: MonadEvent r s m
  => InjectSelector (TypedProtocolsSelector ps) s
  -> DriverTraced ps dState r m
  -> r
  -> WeHaveAgency pr st
  -> Message ps st st'
  -> m (PeerTraced ps pr st' r m a)
  -> dState
  -> m a
runPublishPeerWithDriverTraced inj driver parent tok msg m dState =
  join $ withInjectEventArgs inj publishArgs \publishEv -> do
    sendMessageTraced driver (reference publishEv) tok msg
    pure $ join $ withInjectEventArgs inj (processArgs (reference publishEv) msg) \processEv -> do
      nextPeer <- m
      pure $ runPeerWithDriverTraced inj (reference processEv) driver nextPeer dState
  where
    publishArgs = (simpleNewEventArgs (PublishSelector msg))
      { newEventParent = Just parent
      }
    processArgs respondRef msg' = (simpleNewEventArgs (ProcessSelector msg'))
      { newEventParent = Just respondRef
      , newEventInitialFields = [()]
      }

runReceivePeerWithDriverTraced
  :: MonadEvent r s m
  => InjectSelector (TypedProtocolsSelector ps) s
  -> DriverTraced ps dState r m
  -> r
  -> TheyHaveAgency pr st
  -> (forall (st' :: ps). Message ps st st' -> HandleMessage ps pr st' r m a)
  -> dState
  -> m a
runReceivePeerWithDriverTraced inj driver parent tok k dState = do
  (senderRef, SomeMessage msg, dState') <- withInjectEventArgs inj receiveArgs
    $ const
    $ recvMessageTraced driver tok dState
  join $ withInjectEventArgs inj (processArgs senderRef msg) \processEv -> do
    case k msg of
      Respond tok' m -> do
        ResponseContinuation msg' nextPeer <- m
        pure $ join $ withInjectEventArgs inj (respondArgs (reference processEv) msg msg') \respondEv -> do
          sendMessageTraced driver (reference respondEv) (ServerAgency tok') msg'
          pure $ runPeerWithDriverTraced inj (reference respondEv) driver nextPeer dState'
      Process m -> do
        nextPeer <- m
        pure $ runPeerWithDriverTraced inj (reference processEv) driver nextPeer dState'
  where
    receiveArgs = (simpleNewEventArgs ReceiveSelector)
      { newEventParent = Just parent
      , newEventInitialFields = [()]
      }
    processArgs senderRef msg' = (simpleNewEventArgs (ProcessSelector msg'))
      { newEventParent = Just senderRef
      , newEventInitialFields = [()]
      }
    respondArgs senderRef msg msg' = (simpleNewEventArgs (RespondSelector msg msg'))
      { newEventParent = Just senderRef
      , newEventInitialFields = [()]
      }

class HasSpanContext r where
  context :: MonadIO m => r -> m SpanContext
  wrapContext :: SpanContext -> r

instance HasSpanContext Span where
  context = getSpanContext
  wrapContext = wrapSpanContext

instance (Monoid b, HasSpanContext a) => HasSpanContext (a, b) where
  context = context . fst
  wrapContext = (,mempty) . wrapContext

mkDriverTraced
  :: forall ps r m
   . (MonadIO m, BinaryMessage ps, HasSpanContext r)
  => Channel m ByteString
  -> DriverTraced ps (Maybe ByteString) r m
mkDriverTraced Channel{..} = DriverTraced{..}
  where
    Codec{..} = binaryCodec
    sendMessageTraced
      :: forall (pr :: PeerRole) (st :: ps) (st' :: ps)
       . r
      -> PeerHasAgency pr st
      -> Message ps st st'
      -> m ()
    sendMessageTraced r tok msg = do
      spanContext <- context r
      send $ runPut (put spanContext) <> encode tok msg

    recvMessageTraced
      :: forall (pr :: PeerRole) (st :: ps)
       . PeerHasAgency pr st
      -> Maybe ByteString
      -> m (r, SomeMessage st, Maybe ByteString)
    recvMessageTraced tok trailing = do
      (r, trailing') <- decodeChannel trailing =<< decodeGet (wrapContext <$> get)
      (msg, trailing'') <- decodeChannel trailing' =<< decode tok
      pure (r, msg, trailing'')

    decodeChannel
      :: Maybe ByteString
      -> DecodeStep ByteString DeserializeError m a
      -> m (a, Maybe ByteString)
    decodeChannel _ (DecodeDone a trailing)     = pure (a, trailing)
    decodeChannel _ (DecodeFail failure)        = throwIO failure
    decodeChannel Nothing (DecodePartial next)  = recv >>= next >>= decodeChannel Nothing
    decodeChannel trailing (DecodePartial next) = next trailing >>= decodeChannel Nothing

    startDStateTraced :: Maybe ByteString
    startDStateTraced = Nothing

instance Binary TraceFlags where
  put = putWord8 . traceFlagsValue
  get = traceFlagsFromWord8 <$> getWord8

instance Binary TraceId where
  put = traverse_ putWord8 . B.unpack . traceIdBytes
  get = either fail pure . bytesToTraceId . B.pack =<< replicateM 16 getWord8

instance Binary SpanId where
  put = traverse_ putWord8 . B.unpack . spanIdBytes
  get = either fail pure . bytesToSpanId . B.pack =<< replicateM 8 getWord8

instance Binary TraceState where
  put = put . toList
  get = fromList <$> get
    where
      fromList :: [(Key, Value)] -> TraceState
      fromList = foldr (uncurry insert) empty

instance Binary Key where
  put (Key t) = put t
  get = Key <$> get

instance Binary Value where
  put (Value t) = put t
  get = Value <$> get

instance Binary SpanContext where
  put SpanContext{..} = do
    put traceFlags
    put traceId
    put spanId
    put traceState
  get = do
    traceFlags <- get
    let isRemote = True
    traceId <- get
    spanId <- get
    traceState <- get
    pure SpanContext{..}
