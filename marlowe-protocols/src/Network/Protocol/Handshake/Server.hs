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
import Control.Monad.Trans.Control (MonadBaseControl, control)
import Data.Bifunctor (Bifunctor(bimap))
import Data.ByteString.Lazy (ByteString)
import Data.Functor ((<&>))
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Network.Protocol.ChainSeek.Codec (DeserializeError)
import Network.Protocol.Driver (AcceptSocketDriverSelector, RunServer(..), ToPeer, openServerPortWithLogging)
import Network.Protocol.Handshake.Codec (codecHandshake)
import Network.Protocol.Handshake.Types
import Network.Socket (HostName, PortNumber, withSocketsDo)
import Network.TypedProtocol
import Network.TypedProtocol.Codec (Codec)
import Observe.Event (EventBackend)

-- | A generic server for the handshake protocol.
newtype HandshakeServer server m a = HandshakeServer
  { recvMsgHandshake :: Text -> m (Either a (server m a))
  }

instance (Functor m, Functor (server m)) => Functor (HandshakeServer server m) where
  fmap f HandshakeServer{..} = HandshakeServer
    { recvMsgHandshake = fmap (bimap f $ fmap f) . recvMsgHandshake
    }

simpleHandshakeServer :: MonadFail m => Text -> server m a -> HandshakeServer server m a
simpleHandshakeServer expected server = HandshakeServer
  { recvMsgHandshake = \sig -> if sig == expected
      then pure $ Right server
      else fail $ "Rejecting handshake, " <> show sig <> " /= " <> show expected
  }

embedServerInHandshake :: MonadFail m => Text -> RunServer m (HandshakeServer server) -> RunServer m server
embedServerInHandshake sig (RunServer runServer) = RunServer $ runServer . simpleHandshakeServer sig

openServerPortWithLoggingWithHandshake
  :: forall server protocol (st :: protocol) m n r a
   . ( MonadBaseControl IO m
     , MonadBaseControl IO n
     , MonadCleanup m
     , MonadFail m
     , HasSignature protocol
     )
  => HostName
  -> PortNumber
  -> Codec protocol DeserializeError m ByteString
  -> ToPeer server protocol 'AsServer st m
  -> ((EventBackend m r (AcceptSocketDriverSelector (Handshake protocol)) -> m (RunServer m server)) -> n a)
  -> n a
openServerPortWithLoggingWithHandshake host port codec toPeer withAccept = control \runInBase -> withSocketsDo $ runInBase do
  openServerPortWithLogging
    host
    port
    (codecHandshake codec)
    (handshakeServerPeer toPeer)
    (withAccept . (fmap . fmap) (embedServerInHandshake (signature $ Proxy @protocol)))

hoistHandshakeServer
  :: Functor m
  => (forall x. (forall y. m y -> n y) -> server m x -> server n x)
  -> (forall x. m x -> n x)
  -> HandshakeServer server m a
  -> HandshakeServer server n a
hoistHandshakeServer hoistServer f HandshakeServer{..} = HandshakeServer
  { recvMsgHandshake = f . (fmap . fmap) (hoistServer f) . recvMsgHandshake
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
