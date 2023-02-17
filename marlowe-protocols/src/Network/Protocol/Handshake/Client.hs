{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

-- | A generic client for the handshake protocol. Includes a function for
-- interpreting a client as a typed-protocols peer that can be executed with a
-- driver and a codec.

module Network.Protocol.Handshake.Client
  where

import Control.Monad.Cleanup (MonadCleanup)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Network.Protocol.Codec (BinaryMessage)
import Network.Protocol.Driver (ConnectSocketDriverSelector, RunClient, ToPeer, runClientPeerOverSocketWithLogging)
import Network.Protocol.Handshake.Types
import Network.Socket (HostName, PortNumber)
import Network.TypedProtocol
import Observe.Event (EventBackend)
import Observe.Event.Backend (noopEventBackend)

-- | A generic client for the handshake protocol.
data HandshakeClient client m a = HandshakeClient
  { handshake :: m Text
  , recvMsgReject :: m a
  , recvMsgAccept :: m (client m a)
  }
  deriving Functor

simpleHandshakeClient :: MonadFail m => Text -> client m a -> HandshakeClient client m a
simpleHandshakeClient sig client = HandshakeClient
  { handshake = pure sig
  , recvMsgReject = fail "Handshake rejected by server"
  , recvMsgAccept = pure client
  }

embedClientInHandshake :: MonadFail m => Text -> RunClient m (HandshakeClient server) -> RunClient m server
embedClientInHandshake sig runClient = runClient . simpleHandshakeClient sig

runClientPeerOverSocketWithLoggingWithHandshake
  :: forall client ps (st :: ps) m r
   . ( MonadBaseControl IO m
     , MonadCleanup m
     , MonadFail m
     , HasSignature ps
     , BinaryMessage ps
     )
  => EventBackend m r (ConnectSocketDriverSelector (Handshake ps))
  -> HostName
  -> PortNumber
  -> ToPeer client ps 'AsClient st m
  -> RunClient m client
runClientPeerOverSocketWithLoggingWithHandshake eventBackend host port toPeer =
  embedClientInHandshake (signature $ Proxy @ps) $ runClientPeerOverSocketWithLogging
    eventBackend
    host
    port
    (handshakeClientPeer toPeer)

runClientPeerOverSocketWithHandshake
  :: forall client ps (st :: ps) m
   . ( MonadBaseControl IO m
     , MonadCleanup m
     , MonadFail m
     , HasSignature ps
     , BinaryMessage ps
     )
  => HostName
  -> PortNumber
  -> ToPeer client ps 'AsClient st m
  -> RunClient m client
runClientPeerOverSocketWithHandshake =
  runClientPeerOverSocketWithLoggingWithHandshake $ noopEventBackend ()

hoistHandshakeClient
  :: Functor m
  => (forall x. (forall y. m y -> n y) -> client m x -> client n x)
  -> (forall x. m x -> n x)
  -> HandshakeClient client m a
  -> HandshakeClient client n a
hoistHandshakeClient hoistClient f HandshakeClient{..} = HandshakeClient
  { handshake = f handshake
  , recvMsgReject = f recvMsgReject
  , recvMsgAccept = f $ hoistClient f <$> recvMsgAccept
  }

handshakeClientPeer
  :: forall client m ps st a
   . Functor m
  => (forall x. client m x -> Peer ps 'AsClient st m x)
  -> HandshakeClient client m a
  -> Peer (Handshake ps) 'AsClient ('StInit st) m a
handshakeClientPeer clientPeer HandshakeClient{..} =
  Effect $ peerInit <$> handshake
  where
    peerInit :: Text -> Peer (Handshake ps) 'AsClient ('StInit st) m a
    peerInit sig =
      Yield (ClientAgency TokInit) (MsgHandshake sig) $
      Await (ServerAgency TokHandshake) \case
        MsgReject -> Effect $ Done TokDone <$> recvMsgReject
        MsgAccept -> Effect $ liftPeer . clientPeer <$> recvMsgAccept

    liftPeer :: forall st'. Peer ps 'AsClient st' m a -> Peer (Handshake ps) 'AsClient ('StLift st') m a
    liftPeer = \case
      Effect m -> Effect $ liftPeer <$> m
      Done tok a -> Done (TokLiftNobody tok) a
      Yield (ClientAgency tok) msg next -> Yield (ClientAgency $ TokLiftClient tok) (MsgLift msg) $ liftPeer next
      Await (ServerAgency tok) next -> Await (ServerAgency $ TokLiftServer tok) \(MsgLift msg) -> liftPeer $ next msg
