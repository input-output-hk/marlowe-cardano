{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

-- | A generic server for the handshake protocol. Includes a function for
-- interpreting a server as a typed-protocols peer that can be executed with a
-- driver and a codec.

module Network.Protocol.Handshake.Server
  where

import Control.Monad.Cleanup (MonadCleanup)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Bifunctor (Bifunctor(bimap))
import Data.Binary (Binary)
import Data.ByteString.Lazy (ByteString)
import Data.Functor ((<&>))
import Data.Proxy (Proxy(..))
import Network.Protocol.ChainSeek.Codec (DeserializeError)
import Network.Protocol.Driver
  (AcceptSocketDriverSelector, RunServer(..), ToPeer, acceptRunServerPeerOverSocketWithLogging)
import Network.Protocol.Handshake.Codec (codecHandshake)
import Network.Protocol.Handshake.Types
import Network.Socket (Socket)
import Network.TypedProtocol
import Network.TypedProtocol.Codec (Codec)
import Observe.Event (EventBackend)
import Observe.Event.Backend (noopEventBackend)

-- | A generic server for the handshake protocol.
newtype HandshakeServer sig server m a = HandshakeServer
  { recvMsgHandshake :: sig -> m (Either a (server m a))
  }

instance (Functor m, Functor (server m)) => Functor (HandshakeServer sig server m) where
  fmap f HandshakeServer{..} = HandshakeServer
    { recvMsgHandshake = fmap (bimap f $ fmap f) . recvMsgHandshake
    }

simpleHandshakeServer :: (Eq sig, MonadFail m, Show sig) => sig -> server m a -> HandshakeServer sig server m a
simpleHandshakeServer expected server = HandshakeServer
  { recvMsgHandshake = \sig -> if sig == expected
      then pure $ Right server
      else fail $ "Rejecting handshake, " <> show sig <> " /= " <> show expected
  }

embedServerInHandshake :: (Eq sig, Show sig, MonadFail m) => sig -> RunServer m (HandshakeServer sig server) -> RunServer m server
embedServerInHandshake sig (RunServer runServer) = RunServer $ runServer . simpleHandshakeServer sig

acceptRunServerPeerOverSocketWithLoggingWithHandshake
  :: forall server protocol (st :: protocol) m r
   . ( MonadBaseControl IO m
     , MonadCleanup m
     , MonadFail m
     , Eq (Signature protocol)
     , Show (Signature protocol)
     , Binary (Signature protocol)
     , HasSignature protocol
     )
  => EventBackend m r (AcceptSocketDriverSelector (Handshake protocol))
  -> (forall x. DeserializeError -> m x)
  -> Socket
  -> Codec protocol DeserializeError m ByteString
  -> ToPeer server protocol 'AsServer st m
  -> m (RunServer m server)
acceptRunServerPeerOverSocketWithLoggingWithHandshake eventBackend throwImpl socket codec toPeer = do
  embedServerInHandshake (signature $ Proxy @protocol) <$> acceptRunServerPeerOverSocketWithLogging
    eventBackend
    throwImpl
    socket
    (codecHandshake codec)
    (handshakeServerPeer toPeer)

acceptRunServerPeerOverSocketWithHandshake
  :: forall server protocol (st :: protocol) m
   . ( MonadBaseControl IO m
     , MonadCleanup m
     , MonadFail m
     , Eq (Signature protocol)
     , Show (Signature protocol)
     , Binary (Signature protocol)
     , HasSignature protocol
     )
  => (forall x. DeserializeError -> m x)
  -> Socket
  -> Codec protocol DeserializeError m ByteString
  -> ToPeer server protocol 'AsServer st m
  -> m (RunServer m server)
acceptRunServerPeerOverSocketWithHandshake =
  acceptRunServerPeerOverSocketWithLoggingWithHandshake $ noopEventBackend ()

hoistHandshakeServer
  :: Functor m
  => (forall x. (forall y. m y -> n y) -> server m x -> server n x)
  -> (forall x. m x -> n x)
  -> HandshakeServer sig server m a
  -> HandshakeServer sig server n a
hoistHandshakeServer hoistServer f HandshakeServer{..} = HandshakeServer
  { recvMsgHandshake = f . (fmap . fmap) (hoistServer f) . recvMsgHandshake
  }

handshakeServerPeer
  :: forall client m ps st a
   . Functor m
  => (forall x. client m x -> Peer ps 'AsServer st m x)
  -> HandshakeServer (Signature ps) client m a
  -> Peer (Handshake ps) 'AsServer ('StInit st) m a
handshakeServerPeer serverPeer HandshakeServer{..} =
  Await (ClientAgency TokInit) \case
    MsgHandshake sig -> Effect $ recvMsgHandshake sig <&> \case
      Left a ->
        Yield (ServerAgency TokHandshake) MsgReject $ Done TokDone a
      Right server ->
        Yield (ServerAgency TokHandshake) MsgAccept $ liftPeer $ serverPeer server
  where
    liftPeer :: forall st'. Peer ps 'AsServer st' m a -> Peer (Handshake ps) 'AsServer ('StLift st') m a
    liftPeer = \case
      Effect m -> Effect $ liftPeer <$> m
      Done tok a -> Done (TokLiftNobody tok) a
      Yield (ServerAgency tok) msg next -> Yield (ServerAgency $ TokLiftServer tok) (MsgLift msg) $ liftPeer next
      Await (ClientAgency tok) next -> Await (ClientAgency $ TokLiftClient tok) \(MsgLift msg) -> liftPeer $ next msg
