{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

-- | A generic client for the handshake protocol. Includes a function for
-- interpreting a client as a typed-protocols peer that can be executed with a
-- driver and a codec.
module Network.Protocol.Handshake.Client where

import Data.Functor ((<&>))
import Data.Text (Text)
import Network.Protocol.Handshake.Types
import Network.Protocol.Peer.Trace
import Network.TypedProtocol

-- | A generic client for the handshake protocol.
data HandshakeClient client m a = HandshakeClient
  { handshake :: m Text
  , recvMsgReject :: m a
  , recvMsgAccept :: m (client m a)
  }
  deriving (Functor)

simpleHandshakeClient :: (MonadFail m) => Text -> client m a -> HandshakeClient client m a
simpleHandshakeClient sig client =
  HandshakeClient
    { handshake = pure sig
    , recvMsgReject = fail "Handshake rejected by server"
    , recvMsgAccept = pure client
    }

hoistHandshakeClient
  :: (Functor m)
  => (forall x. (forall y. m y -> n y) -> client m x -> client n x)
  -> (forall x. m x -> n x)
  -> HandshakeClient client m a
  -> HandshakeClient client n a
hoistHandshakeClient hoistClient f HandshakeClient{..} =
  HandshakeClient
    { handshake = f handshake
    , recvMsgReject = f recvMsgReject
    , recvMsgAccept = f $ hoistClient f <$> recvMsgAccept
    }

handshakeClientPeer
  :: forall client m ps st a
   . (Functor m)
  => (forall x. client m x -> PeerTraced ps 'AsClient st m x)
  -> HandshakeClient client m a
  -> PeerTraced (Handshake ps) 'AsClient ('StInit st) m a
handshakeClientPeer clientPeer HandshakeClient{..} =
  EffectTraced $ peerInit <$> handshake
  where
    peerInit :: Text -> PeerTraced (Handshake ps) 'AsClient ('StInit st) m a
    peerInit sig =
      YieldTraced (ClientAgency TokInit) (MsgHandshake sig) $
        Call (ServerAgency TokHandshake) \case
          MsgReject -> EffectTraced $ DoneTraced TokDone <$> recvMsgReject
          MsgAccept -> EffectTraced $ liftPeer . clientPeer <$> recvMsgAccept

    liftPeer :: forall st'. PeerTraced ps 'AsClient st' m a -> PeerTraced (Handshake ps) 'AsClient ('StLift st') m a
    liftPeer = \case
      EffectTraced m -> EffectTraced $ liftPeer <$> m
      DoneTraced tok a -> DoneTraced (TokLiftNobody tok) a
      YieldTraced (ClientAgency tok) msg yield -> YieldTraced (ClientAgency $ TokLiftClient tok) (MsgLift msg) case yield of
        Call (ServerAgency tok') next -> Call (ServerAgency $ TokLiftServer tok') \(MsgLift msg') -> liftPeer $ next msg'
        Cast next -> Cast $ liftPeer next
        Close tok' a -> Close (TokLiftNobody tok') a
      AwaitTraced (ServerAgency tok) k -> AwaitTraced (ServerAgency $ TokLiftServer tok) \(MsgLift msg) -> case k msg of
        Respond (ClientAgency tok') next ->
          Respond (ClientAgency $ TokLiftClient tok') $
            next <&> \case
              Response msg' next' -> Response (MsgLift msg') $ liftPeer next'
        Receive next -> Receive $ liftPeer next
        Closed tok' ma -> Closed (TokLiftNobody tok') ma
