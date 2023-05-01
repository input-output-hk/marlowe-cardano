{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Network.Protocol.Peer.Trace
  where

import Control.Monad (join, replicateM)
import Control.Monad.Event.Class (MonadEvent, withInjectEventArgs)
import Control.Monad.IO.Class (MonadIO)
import Data.Binary (Binary, get, getWord8, put)
import Data.Binary.Put (putWord8, runPut)
import qualified Data.ByteString as B
import Data.ByteString.Lazy (ByteString)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
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
  -- | A peer sends a message to the other peer.
  YieldTraced
    :: WeHaveAgency pr st
    -> Message ps st st'
    -> YieldTraced ps pr st' r m a
    -> PeerTraced ps pr st r m a

  -- | A peer awaits a message from the other peer.
  AwaitTraced
    :: TheyHaveAgency pr st
    -> (forall (st' :: ps). Message ps st st' -> AwaitTraced ps pr st' r m a)
    -> PeerTraced ps pr st r m a

  -- | The session is done.
  DoneTraced
    :: NobodyHasAgency st
    -> a
    -> PeerTraced ps pr st r m a

deriving instance Functor m => Functor (PeerTraced ps pr st r m)

data YieldTraced ps (pr :: PeerRole) (st :: ps) r m a where
  Call
    :: TheyHaveAgency pr st
    -> (forall st'. Message ps st st' -> m (PeerTraced ps pr st' r m a))
    -> YieldTraced ps pr st r m a
  Cast
    :: PeerTraced ps pr st' r m a
    -> YieldTraced ps pr st r m a
  CastDo
    :: m (PeerTraced ps pr st' r m a)
    -> YieldTraced ps pr st r m a
  Close
    :: NobodyHasAgency st
    -> a
    -> YieldTraced ps pr st r m a

deriving instance Functor m => Functor (YieldTraced ps pr st r m)

data AwaitTraced ps pr st r m a where
  Respond
    :: WeHaveAgency pr st
    -> m (Response ps pr st r m a)
    -> AwaitTraced ps 'AsServer st r m a
  Receive
    :: m (PeerTraced ps pr st r m a)
    -> AwaitTraced ps pr st r m a
  Closed
    :: NobodyHasAgency st
    -> m a
    -> AwaitTraced ps pr st r m a

deriving instance Functor m => Functor (AwaitTraced ps pr st r m)

data Response ps pr st r m a where
  Response
    :: Message ps st st'
    -> PeerTraced ps pr st' r m a
    -> Response ps pr st r m a

deriving instance Functor m => Functor (Response ps pr st r m)

data DriverTraced ps dState r m = DriverTraced
  { sendMessageTraced
      :: forall pr st st'
       . Maybe r
      -> PeerHasAgency pr st
      -> Message ps st st'
      -> m ()
  , recvMessageTraced
      :: forall pr (st :: ps)
       . PeerHasAgency pr st
      -> dState
      -> m (Maybe r, SomeMessage st, dState)
  , startDStateTraced :: dState
  }

data TypedProtocolsSelector ps f where
  ProcessSelector :: TypedProtocolsSelector ps ()
  AwaitSelector :: TypedProtocolsSelector ps (Maybe (SomeMessage st'))
  CallSelector :: Message ps st st' -> TypedProtocolsSelector ps (Maybe (SomeMessage st'))
  RespondSelector :: Message ps st st' -> TypedProtocolsSelector ps (Maybe (SomeMessage st'))
  CastSelector :: Message ps st st' -> TypedProtocolsSelector ps ()
  CloseSelector :: Message ps st st' -> TypedProtocolsSelector ps ()

runPeerWithDriverTraced
  :: MonadEvent r s m
  => InjectSelector (TypedProtocolsSelector ps) s
  -> r
  -> DriverTraced ps dState r m
  -> Either (PeerTraced ps pr st r m a) (m (PeerTraced ps pr st r m a))
  -> dState
  -> m a
runPeerWithDriverTraced inj parent driver mPeer dState = do
  (peer, parent') <- case mPeer of
    Left peer -> pure (peer, parent)
    Right m -> withInjectEventArgs inj processArgs \ev -> do
      peer <- m
      pure (peer, reference ev)
  case peer of
    YieldTraced tok msg yield ->
      runYieldPeerWithDriverTraced inj parent' driver tok msg dState yield
    AwaitTraced tok k ->
      runAwaitPeerWithDriverTraced inj parent' driver tok k dState
    DoneTraced _ a -> pure a
  where
    processArgs = (simpleNewEventArgs ProcessSelector)
      { newEventParent = Just parent
      , newEventInitialFields = [()]
      }

runYieldPeerWithDriverTraced
  :: MonadEvent r s m
  => InjectSelector (TypedProtocolsSelector ps) s
  -> r
  -> DriverTraced ps dState r m
  -> WeHaveAgency pr st
  -> Message ps st st'
  -> dState
  -> YieldTraced ps pr st' r m a
  -> m a
runYieldPeerWithDriverTraced inj parent driver tok msg dState = \case
  Call tok' k -> join $ withInjectEventArgs inj callArgs \callEv -> do
    sendMessageTraced driver (Just $ reference callEv) tok msg
    (_, SomeMessage msg', dState') <- recvMessageTraced driver tok' dState
    addField callEv $ Just $ SomeMessage msg'
    pure $ runPeerWithDriverTraced inj (reference callEv) driver (Right $ k msg') dState'
  Cast m -> join $ withInjectEventArgs inj castArgs \castEv -> do
    sendMessageTraced driver (Just $ reference castEv) tok msg
    pure $ runPeerWithDriverTraced inj (reference castEv) driver (Left m) dState
  CastDo m -> join $ withInjectEventArgs inj castArgs \castEv -> do
    sendMessageTraced driver (Just $ reference castEv) tok msg
    pure $ runPeerWithDriverTraced inj (reference castEv) driver (Right m) dState
  Close _ a -> withInjectEventArgs inj closeArgs \closeEv -> do
    sendMessageTraced driver (Just $ reference closeEv) tok msg
    pure a
  where
    callArgs = (simpleNewEventArgs (CallSelector msg))
      { newEventParent = Just parent
      , newEventInitialFields = [Nothing]
      }
    castArgs = (simpleNewEventArgs (CastSelector msg))
      { newEventParent = Just parent
      , newEventInitialFields = [()]
      }
    closeArgs = (simpleNewEventArgs (CloseSelector msg))
      { newEventParent = Just parent
      , newEventInitialFields = [()]
      }

runAwaitPeerWithDriverTraced
  :: MonadEvent r s m
  => InjectSelector (TypedProtocolsSelector ps) s
  -> r
  -> DriverTraced ps dState r m
  -> TheyHaveAgency pr st
  -> (forall (st' :: ps). Message ps st st' -> AwaitTraced ps pr st' r m a)
  -> dState
  -> m a
runAwaitPeerWithDriverTraced inj parent driver tok k dState =
  join $ withInjectEventArgs inj awaitArgs \awaitEv -> do
    (mSendRef, SomeMessage msg, dState') <- recvMessageTraced driver tok dState
    let parent' = fromMaybe (reference awaitEv) mSendRef
    addField awaitEv $ Just $ SomeMessage msg
    pure case k msg of
      Respond tok' m -> join $ withInjectEventArgs inj (respondArgs parent' msg) \respondEv -> do
        Response msg' nextPeer <- m
        addField respondEv $ Just $ SomeMessage msg'
        sendMessageTraced driver (Just $ reference respondEv) tok' msg'
        pure $ runPeerWithDriverTraced inj (reference respondEv) driver (Left nextPeer) dState'
      Receive nextPeer ->
        runPeerWithDriverTraced inj (reference awaitEv) driver (Right nextPeer) dState'
      Closed _ ma -> withInjectEventArgs inj (closeArgs parent' msg) $ const ma
  where
    respondArgs parent' msg = (simpleNewEventArgs (RespondSelector msg))
      { newEventParent = Just parent'
      , newEventInitialFields = [Nothing]
      }
    awaitArgs = (simpleNewEventArgs AwaitSelector)
      { newEventParent = Just parent
      , newEventInitialFields = [Nothing]
      }
    closeArgs parent' msg = (simpleNewEventArgs (CloseSelector msg))
      { newEventParent = Just parent'
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

mkDriverCompat
  :: forall ps m
   . (MonadIO m, BinaryMessage ps)
  => Channel m ByteString
  -> Driver ps (Maybe ByteString) m
mkDriverCompat Channel{..} = Driver{..}
  where
    Codec{..} = binaryCodec
    sendMessage
      :: forall (pr :: PeerRole) (st :: ps) (st' :: ps)
       . PeerHasAgency pr st
      -> Message ps st st'
      -> m ()
    sendMessage tok msg = send $ runPut (put $ Nothing @SpanContext) <> encode tok msg

    recvMessage
      :: forall (pr :: PeerRole) (st :: ps)
       . PeerHasAgency pr st
      -> Maybe ByteString
      -> m (SomeMessage st, Maybe ByteString)
    recvMessage tok trailing = do
      (_ :: Maybe SpanContext, trailing') <- decodeChannel trailing =<< decodeGet get
      (msg, trailing'') <- decodeChannel trailing' =<< decode tok
      pure (msg, trailing'')

    decodeChannel
      :: Maybe ByteString
      -> DecodeStep ByteString DeserializeError m a
      -> m (a, Maybe ByteString)
    decodeChannel _ (DecodeDone a trailing)     = pure (a, trailing)
    decodeChannel _ (DecodeFail failure)        = throwIO failure
    decodeChannel Nothing (DecodePartial next)  = recv >>= next >>= decodeChannel Nothing
    decodeChannel trailing (DecodePartial next) = next trailing >>= decodeChannel Nothing

    startDState :: Maybe ByteString
    startDState = Nothing

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
       . Maybe r
      -> PeerHasAgency pr st
      -> Message ps st st'
      -> m ()
    sendMessageTraced r tok msg = do
      spanContext <- traverse context r
      send $ runPut (put spanContext) <> encode tok msg

    recvMessageTraced
      :: forall (pr :: PeerRole) (st :: ps)
       . PeerHasAgency pr st
      -> Maybe ByteString
      -> m (Maybe r, SomeMessage st, Maybe ByteString)
    recvMessageTraced tok trailing = do
      (r, trailing') <- decodeChannel trailing =<< decodeGet (fmap wrapContext <$> get)
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
