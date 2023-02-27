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
import Network.Protocol.Connection (ClientServerPair(..), Connection(..), ConnectionSource(..), Connector(..))
import Network.Protocol.Handshake.Client (handshakeClientConnector)
import Network.Protocol.Handshake.Types
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
