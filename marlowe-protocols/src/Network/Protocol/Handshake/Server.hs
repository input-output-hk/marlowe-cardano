{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

-- | A generic server for the handshake protocol. Includes a function for
-- interpreting a server as a typed-protocols peer that can be executed with a
-- driver and a codec.

module Network.Protocol.Handshake.Server
  where

import Data.Bifunctor (Bifunctor(bimap))
import Data.Functor ((<&>))
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Network.Protocol.Connection
  ( ClientServerPair(..)
  , Connection(..)
  , ConnectionSource(..)
  , ConnectionSourceTraced(..)
  , ConnectionTraced(..)
  , Connector(..)
  , ConnectorTraced(..)
  )
import Network.Protocol.Handshake.Client (handshakeClientConnector)
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

handshakeConnectionSource
  :: forall ps server m
   . (HasSignature ps, MonadFail m)
  => ConnectionSource ps server m
  -> ConnectionSource (Handshake ps) server m
handshakeConnectionSource = ConnectionSource . fmap handshakeServerConnector . acceptConnector

handshakeServerConnector
  :: forall ps server m
   . (HasSignature ps, MonadFail m)
  => Connector ps 'AsServer server m
  -> Connector (Handshake ps) 'AsServer server m
handshakeServerConnector Connector{..} = Connector $ handshakeServerConnection <$> openConnection

handshakeServerConnection
  :: forall ps peer m
   . (HasSignature ps, MonadFail m)
  => Connection ps 'AsServer peer m
  -> Connection (Handshake ps) 'AsServer peer m
handshakeServerConnection Connection{..} = Connection
  { toPeer = handshakeServerPeer id . simpleHandshakeServer (signature $ Proxy @ps) . toPeer
  , ..
  }

handshakeConnectionSourceTraced
  :: forall ps server r s m
   . (HasSignature ps, MonadFail m)
  => ConnectionSourceTraced ps server r s m
  -> ConnectionSourceTraced (Handshake ps) server r s m
handshakeConnectionSourceTraced = ConnectionSourceTraced . fmap handshakeServerConnectorTraced . acceptConnectorTraced

handshakeServerConnectorTraced
  :: forall ps server r s m
   . (HasSignature ps, MonadFail m)
  => ConnectorTraced ps 'AsServer server r s m
  -> ConnectorTraced (Handshake ps) 'AsServer server r s m
handshakeServerConnectorTraced ConnectorTraced{..} = ConnectorTraced $ handshakeServerConnectionTraced <$> openConnectionTraced

handshakeServerConnectionTraced
  :: forall ps peer r s m
   . (HasSignature ps, MonadFail m)
  => ConnectionTraced ps 'AsServer peer r s m
  -> ConnectionTraced (Handshake ps) 'AsServer peer r s m
handshakeServerConnectionTraced ConnectionTraced{..} = ConnectionTraced
  { toPeer = handshakeServerPeerTraced id . simpleHandshakeServer (signature $ Proxy @ps) . toPeer
  , ..
  }

handshakeClientServerPair
  :: forall ps client server m
   . (HasSignature ps, MonadFail m)
  => ClientServerPair ps client server m
  -> ClientServerPair (Handshake ps) client server m
handshakeClientServerPair ClientServerPair{..} = ClientServerPair
  { connectionSource = handshakeConnectionSource connectionSource
  , clientConnector = handshakeClientConnector clientConnector
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
  => (forall x. client m x -> Peer ps 'AsServer st m x)
  -> HandshakeServer client m a
  -> Peer (Handshake ps) 'AsServer ('StInit st) m a
handshakeServerPeer serverPeer HandshakeServer{..} =
  Await (ClientAgency TokInit) \case
    MsgHandshake sig -> Effect $ recvMsgHandshake sig <&> \case
      Left a ->
        Yield (ServerAgency TokHandshake) MsgReject $ Effect $ Done TokDone <$> a
      Right server ->
        Yield (ServerAgency TokHandshake) MsgAccept $ liftPeer $ serverPeer server
  where
    liftPeer :: forall st'. Peer ps 'AsServer st' m a -> Peer (Handshake ps) 'AsServer ('StLift st') m a
    liftPeer = \case
      Effect m -> Effect $ liftPeer <$> m
      Done tok a -> Done (TokLiftNobody tok) a
      Yield (ServerAgency tok) msg next -> Yield (ServerAgency $ TokLiftServer tok) (MsgLift msg) $ liftPeer next
      Await (ClientAgency tok) next -> Await (ClientAgency $ TokLiftClient tok) \(MsgLift msg) -> liftPeer $ next msg

handshakeServerPeerTraced
  :: forall client r m ps st a
   . Functor m
  => (forall x. client m x -> PeerTraced ps 'AsServer st r m x)
  -> HandshakeServer client m a
  -> PeerTraced (Handshake ps) 'AsServer ('StInit st) r m a
handshakeServerPeerTraced serverPeer HandshakeServer{..} =
  AwaitTraced (ClientAgency TokInit) \case
    MsgHandshake sig -> Respond (ServerAgency TokHandshake) $ recvMsgHandshake sig <&> \case
      Left a ->
        Response MsgReject $ EffectTraced $ DoneTraced TokDone <$> a
      Right server ->
        Response MsgAccept $ liftPeer $ serverPeer server
  where
    liftPeer :: forall st'. PeerTraced ps 'AsServer st' r m a -> PeerTraced (Handshake ps) 'AsServer ('StLift st') r m a
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
