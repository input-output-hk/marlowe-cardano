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
import Network.Socket (AddrInfo, Socket, addrAddress, close, connect, openSocket)
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

type RunAsPeer m machine = forall a. machine m a -> m a

type ToPeer machine protocol peer st m = forall a. machine m a -> Peer protocol peer st m a

runPeerOverSocket
  :: MonadBase IO m
  => (forall x. ex -> m x)
  -> Socket
  -> Codec protocol ex m ByteString
  -> ToPeer machine protocol peer st m
  -> RunAsPeer m machine
runPeerOverSocket throwImpl socket codec toPeer machine = do
  let channel = hoistChannel liftBase $  socketAsChannel socket
  let driver = mkDriver throwImpl codec channel
  let peer = toPeer machine
  fst <$> runPeerWithDriver driver peer (startDState driver)

runClientPeerOverSocket
  :: MonadBaseControl IO m
  => (forall x. ex -> m x)
  -> AddrInfo
  -> Codec protocol ex m ByteString
  -> ToPeer client protocol peer st m
  -> RunAsPeer m client
runClientPeerOverSocket throwImpl addr codec toPeer client =
  bracket open (liftBase . close) \socket ->
    runPeerOverSocket throwImpl socket codec toPeer client
  where
    open = bracketOnError (liftBase $ openSocket addr) (liftBase . close) \sock -> do
      liftBase $ connect sock $ addrAddress addr
      pure sock
