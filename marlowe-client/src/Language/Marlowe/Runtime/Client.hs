{-# LANGUAGE FlexibleContexts #-}

module Language.Marlowe.Runtime.Client
  ( module Control.Monad.Trans.Marlowe
  , module Control.Monad.Trans.Marlowe.Class
  , connectToMarloweRuntime
  , connectToMarloweRuntimeTraced
  ) where

import Control.Monad.Event.Class
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Marlowe
import Control.Monad.Trans.Marlowe.Class
import Language.Marlowe.Protocol.Client (marloweRuntimeClientPeer)
import Language.Marlowe.Protocol.Types (MarloweRuntime)
import Network.Protocol.Driver (tcpClient)
import Network.Protocol.Handshake.Client (handshakeClientConnector)
import Network.Protocol.Handshake.Types (Handshake)
import Network.Protocol.Trace (HasSpanContext, PeerSelector, traceClientConnector, traceClientConnectorNoop)
import Network.Socket (HostName, PortNumber)

connectToMarloweRuntimeTraced
  :: (MonadInjectEvent r (PeerSelector (Handshake MarloweRuntime)) s m, MonadIO m, HasSpanContext r, MonadFail m)
  => HostName
  -> PortNumber
  -> MarloweT m a
  -> m a
connectToMarloweRuntimeTraced host port action = runMarloweT action
  $ traceClientConnector inject
  $ handshakeClientConnector
  $ tcpClient host port marloweRuntimeClientPeer

connectToMarloweRuntime :: (MonadIO m, MonadFail m) => HostName -> PortNumber -> MarloweT m a -> m a
connectToMarloweRuntime host port action = runMarloweT action
  $ traceClientConnectorNoop
  $ handshakeClientConnector
  $ tcpClient host port marloweRuntimeClientPeer
