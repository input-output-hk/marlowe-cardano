{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- | A generic server for the handshake protocol. Includes a function for
-- interpreting a server as a typed-protocols peer that can be executed with a
-- driver and a codec.

module Network.Protocol.Handshake.Server
  where

import Data.Bifunctor (Bifunctor(bimap))
import Data.Functor ((<&>))
import Network.Protocol.Handshake.Types
import Network.TypedProtocol

-- | A generic server for the handshake protocol.
newtype HandshakeServer h server m a = HandshakeServer
  { recvMsgHandshake :: h -> m (Either a (server m a))
  }

instance (Functor m, Functor (server m)) => Functor (HandshakeServer h server m) where
  fmap f HandshakeServer{..} = HandshakeServer
    { recvMsgHandshake = fmap (bimap f $ fmap f) . recvMsgHandshake
    }

simpleHandshakeServer :: (Applicative m, Eq h) => h -> a -> server m a -> HandshakeServer h server m a
simpleHandshakeServer expected a server = HandshakeServer
  { recvMsgHandshake = \h -> pure if h == expected then Right server else Left a
  }

hoistHandshakeServer
  :: Functor m
  => (forall x. (forall y. m y -> n y) -> server m x -> server n x)
  -> (forall x. m x -> n x)
  -> HandshakeServer h server m a
  -> HandshakeServer h server n a
hoistHandshakeServer hoistServer f HandshakeServer{..} = HandshakeServer
  { recvMsgHandshake = f . (fmap . fmap) (hoistServer f) . recvMsgHandshake
  }

handshakeServerPeer
  :: forall client m h ps st a
   . Functor m
  => (forall x. client m x -> Peer ps 'AsServer st m x)
  -> HandshakeServer h client m a
  -> Peer (Handshake h ps) 'AsServer ('StInit st) m a
handshakeServerPeer serverPeer HandshakeServer{..} =
  Await (ClientAgency TokInit) \case
    MsgHandshake h -> Effect $ recvMsgHandshake h <&> \case
      Left a ->
        Yield (ServerAgency TokHandshake) MsgReject $ Done TokDone a
      Right server ->
        Yield (ServerAgency TokHandshake) MsgAccept $ liftPeer $ serverPeer server
  where
    liftPeer :: forall st'. Peer ps 'AsServer st' m a -> Peer (Handshake h ps) 'AsServer ('StLift st') m a
    liftPeer = \case
      Effect m -> Effect $ liftPeer <$> m
      Done tok a -> Done (TokLiftNobody tok) a
      Yield (ServerAgency tok) msg next -> Yield (ServerAgency $ TokLiftServer tok) (MsgLift msg) $ liftPeer next
      Await (ClientAgency tok) next -> Await (ClientAgency $ TokLiftClient tok) \(MsgLift msg) -> liftPeer $ next msg
