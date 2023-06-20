{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

-- | A generic server for the handshake protocol. Includes a function for
-- interpreting a server as a typed-protocols peer that can be executed with a
-- driver and a codec.

module Network.Protocol.Handshake.Server where

import Data.Bifunctor (Bifunctor(bimap))
import Data.Functor ((<&>))
import Data.Text (Text)
import Network.Protocol.Handshake.Types
import Network.Protocol.Peer.Trace
import Network.TypedProtocol

-- | A generic server for the handshake protocol.
newtype HandshakeServer server m a = HandshakeServer
  { recvMsgHandshake :: Text -> m (Either (m a) (server m a))
  }

instance (Functor m, Functor (server m)) => Functor (HandshakeServer server m) where
  fmap f HandshakeServer{..} = HandshakeServer
    { recvMsgHandshake = fmap (bimap (fmap f) $ fmap f) . recvMsgHandshake
    }

simpleHandshakeServer :: MonadFail m => Text -> server m a -> HandshakeServer server m a
simpleHandshakeServer expected server = HandshakeServer
  { recvMsgHandshake = \sig -> if sig == expected
      then pure $ Right server
      else pure $ Left $ fail $ "Rejecting handshake, " <> show sig <> " /= " <> show expected
  }

hoistHandshakeServer
  :: Functor m
  => (forall x. (forall y. m y -> n y) -> server m x -> server n x)
  -> (forall x. m x -> n x)
  -> HandshakeServer server m a
  -> HandshakeServer server n a
hoistHandshakeServer hoistServer f HandshakeServer{..} = HandshakeServer
  { recvMsgHandshake = f . (fmap . bimap f) (hoistServer f) . recvMsgHandshake
  }

handshakeServerPeer
  :: forall client m ps st a
   . Functor m
  => (forall x. client m x -> PeerTraced ps 'AsServer st m x)
  -> HandshakeServer client m a
  -> PeerTraced (Handshake ps) 'AsServer ('StInit st) m a
handshakeServerPeer serverPeer HandshakeServer{..} =
  AwaitTraced (ClientAgency TokInit) \case
    MsgHandshake sig -> Respond (ServerAgency TokHandshake) $ recvMsgHandshake sig <&> \case
      Left a ->
        Response MsgReject $ EffectTraced $ DoneTraced TokDone <$> a
      Right server ->
        Response MsgAccept $ liftPeer $ serverPeer server
  where
    liftPeer :: forall st'. PeerTraced ps 'AsServer st' m a -> PeerTraced (Handshake ps) 'AsServer ('StLift st') m a
    liftPeer = \case
      EffectTraced m -> EffectTraced $ liftPeer <$> m
      DoneTraced tok a -> DoneTraced (TokLiftNobody tok) a
      YieldTraced (ServerAgency tok) msg yield -> YieldTraced (ServerAgency $ TokLiftServer tok) (MsgLift msg) case yield of
        Call (ClientAgency tok') next -> Call (ClientAgency $ TokLiftClient tok') \(MsgLift msg') -> liftPeer $ next msg'
        Cast next -> Cast $ liftPeer next
        Close tok' a -> Close (TokLiftNobody tok') a
      AwaitTraced (ClientAgency tok) k -> AwaitTraced (ClientAgency $ TokLiftClient tok) \(MsgLift msg) -> case k msg of
        Respond (ServerAgency tok') next -> Respond (ServerAgency $ TokLiftServer tok') $ next <&> \case
          Response msg' next' -> Response (MsgLift msg') $ liftPeer next'
        Receive next -> Receive $ liftPeer next
        Closed tok' ma -> Closed (TokLiftNobody tok') ma
