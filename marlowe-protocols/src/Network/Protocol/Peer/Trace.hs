{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Network.Protocol.Peer.Trace
  where

import Data.Functor ((<&>))
import Data.Proxy
import Data.Text (Text)
import Network.TypedProtocol
import Network.TypedProtocol.Codec
import Observe.Event.Render.OpenTelemetry
import OpenTelemetry.Trace.Core
import qualified OpenTelemetry.Trace.TraceState as TraceState

-- | A peer in a protocol session which is suitable for tracing.
data PeerTraced ps (pr :: PeerRole) (st :: ps) m a where
  -- | A peer sends a message to the other peer.
  YieldTraced
    :: WeHaveAgency pr st
    -> Message ps st st'
    -> YieldTraced ps pr st' m a
    -> PeerTraced ps pr st m a

  -- | A peer awaits a message from the other peer.
  AwaitTraced
    :: TheyHaveAgency pr st
    -> (forall (st' :: ps). Message ps st st' -> AwaitTraced ps pr st' m a)
    -> PeerTraced ps pr st m a

  -- | A peer is processing information.
  EffectTraced
    :: m (PeerTraced ps pr st m a)
    -> PeerTraced ps pr st m a

  -- | The session is done.
  DoneTraced
    :: NobodyHasAgency st
    -> a
    -> PeerTraced ps pr st m a

deriving instance Functor m => Functor (PeerTraced ps pr st m)

data YieldTraced ps (pr :: PeerRole) (st :: ps) m a where
  Call
    :: TheyHaveAgency pr st
    -> (forall st'. Message ps st st' -> PeerTraced ps pr st' m a)
    -> YieldTraced ps pr st m a
  Cast
    :: PeerTraced ps pr st m a
    -> YieldTraced ps pr st m a
  Close
    :: NobodyHasAgency st
    -> a
    -> YieldTraced ps pr st m a

deriving instance Functor m => Functor (YieldTraced ps pr st m)

data AwaitTraced ps pr st m a where
  Respond
    :: WeHaveAgency pr st
    -> m (Response ps pr st m a)
    -> AwaitTraced ps pr st m a
  Receive
    :: PeerTraced ps pr st m a
    -> AwaitTraced ps pr st m a
  Closed
    :: NobodyHasAgency st
    -> m a
    -> AwaitTraced ps pr st m a

deriving instance Functor m => Functor (AwaitTraced ps pr st m)

data Response ps pr st m a where
  Response
    :: Message ps st st'
    -> PeerTraced ps pr st' m a
    -> Response ps pr st m a

deriving instance Functor m => Functor (Response ps pr st m)

hoistPeerTraced
  :: Functor m
  => (forall x. m x -> n x)
  -> PeerTraced ps pr st m a
  -> PeerTraced ps pr st n a
hoistPeerTraced f = \case
  YieldTraced tok msg yield -> YieldTraced tok msg case yield of
    Call tok' k -> Call tok' $ hoistPeerTraced f . k
    Cast next -> Cast $ hoistPeerTraced f next
    Close tok' a -> Close tok' a
  AwaitTraced tok k -> AwaitTraced tok \msg -> case k msg of
    Respond tok' m -> Respond tok' $ f $ m <&> \case
      Response msg' next -> Response msg' $ hoistPeerTraced f next
    Receive next -> Receive $ hoistPeerTraced f next
    Closed tok' ma -> Closed tok' $ f ma
  DoneTraced tok a -> DoneTraced tok a
  EffectTraced m -> EffectTraced $ f $ hoistPeerTraced f <$> m

peerTracedToPeer :: Functor m => PeerTraced ps pr st m a -> Peer ps pr st m a
peerTracedToPeer = \case
  YieldTraced tok msg yield -> Yield tok msg case yield of
    Call tok' k -> Await tok' $ peerTracedToPeer . k
    Cast next -> peerTracedToPeer next
    Close tok' a -> Done tok' a
  AwaitTraced tok k -> Await tok \msg -> case k msg of
    Respond tok' m -> Effect $ m <&> \case
      Response msg' next -> Yield tok' msg' $ peerTracedToPeer next
    Receive next -> peerTracedToPeer next
    Closed tok' ma -> Effect $ Done tok' <$> ma
  DoneTraced tok a -> Done tok a
  EffectTraced m -> Effect $ peerTracedToPeer <$> m

data TypedProtocolsSelector ps f where
  ReceiveSelector :: PeerHasAgency pr st -> Message ps st st' -> TypedProtocolsSelector ps ()
  CallSelector :: PeerHasAgency pr st -> Message ps st st' -> TypedProtocolsSelector ps (Maybe (AnyMessageAndAgency ps))
  RespondSelector :: PeerHasAgency pr st -> Message ps st st' -> TypedProtocolsSelector ps (Maybe (AnyMessageAndAgency ps))
  CastSelector :: PeerHasAgency pr st -> Message ps st st' -> TypedProtocolsSelector ps ()
  CloseSelector :: PeerHasAgency pr st -> Message ps st st' -> TypedProtocolsSelector ps ()

defaultSpanContext :: SpanContext
defaultSpanContext = SpanContext
  defaultTraceFlags
  False
  "00000000000000000000000000000000"
  "0000000000000000"
  TraceState.empty

data MessageAttributes = MessageAttributes
  { messageType :: Text
  , messageParameters :: [PrimitiveAttribute]
  }

class Protocol ps => OTelProtocol ps where
  protocolName :: Proxy ps -> Text
  messageAttributes :: PeerHasAgency pr st -> Message ps st st' -> MessageAttributes

renderTypedProtocolSelectorOTel
  :: forall ps. OTelProtocol ps => RenderSelectorOTel (TypedProtocolsSelector ps)
renderTypedProtocolSelectorOTel = \case
  ReceiveSelector tok msg -> OTelRendered
    { eventName = "receive " <> protocolName (Proxy @ps) <> ":" <> messageType (messageAttributes tok msg)
    , eventKind = Consumer
    , renderField = const []
    }
  CallSelector tok msg -> OTelRendered
    { eventName = "call " <> protocolName (Proxy @ps) <> ":" <> messageType (messageAttributes tok msg)
    , eventKind = Client
    , renderField = \case
        Nothing -> messageToAttributes "send" $ AnyMessageAndAgency tok msg
        Just msg' -> messageToAttributes "recv" msg'
    }
  RespondSelector tok msg -> OTelRendered
    { eventName = "respond " <> protocolName (Proxy @ps) <> ":" <> messageType (messageAttributes tok msg)
    , eventKind = Server
    , renderField = \case
        Nothing -> messageToAttributes "recv" $ AnyMessageAndAgency tok msg
        Just msg' -> messageToAttributes "send" msg'
    }
  CastSelector tok msg -> OTelRendered
    { eventName = "cast " <> protocolName (Proxy @ps) <> ":" <> messageType (messageAttributes tok msg)
    , eventKind = Producer
    , renderField = const []
    }
  CloseSelector tok msg -> OTelRendered
    { eventName = "close " <> protocolName (Proxy @ps) <> ":" <> messageType (messageAttributes tok msg)
    , eventKind = Producer
    , renderField = const []
    }

messageToAttributes :: Text -> OTelProtocol ps => AnyMessageAndAgency ps -> [(Text, Attribute)]
messageToAttributes prefix (AnyMessageAndAgency tok msg) = case messageAttributes tok msg of
  MessageAttributes{..} ->
    [ ("typed-protocols.message." <> prefix <> ".type", toAttribute messageType)
    , ("typed-protocols.message." <> prefix <> ".parameters", toAttribute messageParameters)
    ]
