{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- | A generic client for the handshake protocol. Includes a function for
-- interpreting a client as a typed-protocols peer that can be executed with a
-- driver and a codec.

module Network.Protocol.Handshake.Client
  where

import Network.Protocol.Handshake.Types
import Network.TypedProtocol

-- | A generic client for the handshake protocol.
data HandshakeClient h client m a = HandshakeClient
  { handshake :: m h
  , recvMsgReject :: m a
  , recvMsgAccept :: m (client m a)
  }
  deriving Functor

simpleHandshakeClient :: Applicative m => h -> a -> client m a -> HandshakeClient h client m a
simpleHandshakeClient h a client = HandshakeClient
  { handshake = pure h
  , recvMsgReject = pure a
  , recvMsgAccept = pure client
  }

hoistHandshakeClient
  :: Functor m
  => (forall x. (forall y. m y -> n y) -> client m x -> client n x)
  -> (forall x. m x -> n x)
  -> HandshakeClient h client m a
  -> HandshakeClient h client n a
hoistHandshakeClient hoistClient f HandshakeClient{..} = HandshakeClient
  { handshake = f handshake
  , recvMsgReject = f recvMsgReject
  , recvMsgAccept = f $ hoistClient f <$> recvMsgAccept
  }

handshakeClientPeer
  :: forall client m h ps st a
   . Functor m
  => (forall x. client m x -> Peer ps 'AsClient st m x)
  -> HandshakeClient h client m a
  -> Peer (Handshake h ps) 'AsClient ('StInit st) m a
handshakeClientPeer clientPeer HandshakeClient{..} =
  Effect $ peerInit <$> handshake
  where
    peerInit :: h -> Peer (Handshake h ps) 'AsClient ('StInit st) m a
    peerInit h =
      Yield (ClientAgency TokInit) (MsgHandshake h) $
      Await (ServerAgency TokHandshake) \case
        MsgReject -> Effect $ Done TokDone <$> recvMsgReject
        MsgAccept -> Effect $ liftPeer . clientPeer <$> recvMsgAccept

    liftPeer :: forall st'. Peer ps 'AsClient st' m a -> Peer (Handshake h ps) 'AsClient ('StLift st') m a
    liftPeer = \case
      Effect m -> Effect $ liftPeer <$> m
      Done tok a -> Done (TokLiftNobody tok) a
      Yield (ClientAgency tok) msg next -> Yield (ClientAgency $ TokLiftClient tok) (MsgLift msg) $ liftPeer next
      Await (ServerAgency tok) next -> Await (ServerAgency $ TokLiftServer tok) \(MsgLift msg) -> liftPeer $ next msg
