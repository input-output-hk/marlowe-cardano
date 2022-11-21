{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Network.Protocol.Driver
  where

import Control.Exception.Lifted (bracket, bracketOnError)
import Control.Monad.Base (MonadBase(liftBase))
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.ByteString.Lazy (ByteString)
import Network.Channel (Channel(..), hoistChannel, socketAsChannel)
import Network.Socket (AddrInfo, SockAddr, Socket, addrAddress, close, connect, openSocket)
import Network.Socket.Address (accept)
import Network.TypedProtocol (Message, Peer, PeerHasAgency, PeerRole, SomeMessage, runPeerWithDriver)
import Network.TypedProtocol.Codec (Codec(..), DecodeStep(..))
import Network.TypedProtocol.Driver (Driver(..))

mkDriver
  :: forall ps failure bytes m
   . Monad m
  => (forall a. failure -> m a)
  -> Codec ps failure m bytes
  -> Channel m bytes
  -> Driver ps (Maybe bytes) m
mkDriver throwImpl Codec{..} Channel{..} = Driver{..}
  where
    sendMessage
      :: forall (pr :: PeerRole) (st :: ps) (st' :: ps)
       . PeerHasAgency pr st
      -> Message ps st st'
      -> m ()
    sendMessage tok = send . encode tok

    recvMessage
      :: forall (pr :: PeerRole) (st :: ps)
       . PeerHasAgency pr st
      -> Maybe bytes
      -> m (SomeMessage st, Maybe bytes)
    recvMessage tok trailing = decodeChannel trailing =<< decode tok

    decodeChannel
      :: Maybe bytes
      -> DecodeStep bytes failure m a
      -> m (a, Maybe bytes)
    decodeChannel _ (DecodeDone a trailing)     = pure (a, trailing)
    decodeChannel _ (DecodeFail failure)        = throwImpl failure
    decodeChannel Nothing (DecodePartial next)  = recv >>= next >>= decodeChannel Nothing
    decodeChannel trailing (DecodePartial next) = next trailing >>= decodeChannel Nothing

    startDState :: Maybe bytes
    startDState = Nothing

type RunClient m client = forall a. client m a -> m a
newtype RunServer m server = RunServer (forall a. server m a -> m a)

type ToPeer machine protocol peer st m = forall a. machine m a -> Peer protocol peer st m a

runClientPeerOverSocket
  :: MonadBaseControl IO m
  => (forall x. ex -> m x)
  -> AddrInfo
  -> Codec protocol ex m ByteString
  -> ToPeer client protocol peer st m
  -> RunClient m client
runClientPeerOverSocket throwImpl addr codec toPeer client =
  bracket open (liftBase . close) \socket -> do
    let channel = hoistChannel liftBase $  socketAsChannel socket
    let driver = mkDriver throwImpl codec channel
    let peer = toPeer client
    fst <$> runPeerWithDriver driver peer (startDState driver)
  where
    open = bracketOnError (liftBase $ openSocket addr) (liftBase . close) \sock -> do
      liftBase $ connect sock $ addrAddress addr
      pure sock

acceptRunServerPeerOverSocket
  :: MonadBase IO m
  => (forall x. ex -> m x)
  -> Socket
  -> Codec protocol ex m ByteString
  -> ToPeer server protocol peer st m
  -> m (RunServer m server)
acceptRunServerPeerOverSocket throwImpl socket codec toPeer = do
  (conn, _ :: SockAddr) <- liftBase $ accept socket
  let driver = mkDriver throwImpl codec $ hoistChannel liftBase $ socketAsChannel conn
  pure $ RunServer \server -> do
    let peer = toPeer server
    fst <$> runPeerWithDriver driver peer (startDState driver)
