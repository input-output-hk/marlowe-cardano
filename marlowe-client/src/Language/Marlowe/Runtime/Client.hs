{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.Client (
  module Control.Monad.Trans.Marlowe,
  module Control.Monad.Trans.Marlowe.Class,
  module Language.Marlowe.Runtime.Client.Transfer,
  connectToMarloweRuntime,
  connectToMarloweRuntimeTraced,
) where

import Control.Monad.Event.Class
import Control.Monad.Trans.Marlowe
import Control.Monad.Trans.Marlowe.Class
import Language.Marlowe.Protocol.Client (marloweRuntimeClientPeer)
import Language.Marlowe.Protocol.Types (MarloweRuntime)
import Language.Marlowe.Runtime.Client.Transfer (exportContract, exportIncremental, importBundle, importIncremental)
import Network.Protocol.Driver (tcpClient)
import Network.Protocol.Driver.Trace (HasSpanContext, TcpClientSelector, tcpClientTraced)
import Network.Protocol.Handshake.Types (Handshake)
import Network.Socket (HostName, PortNumber)
import Observe.Event (InjectSelector)
import UnliftIO (MonadUnliftIO)

connectToMarloweRuntimeTraced
  :: (MonadEvent r s m, MonadUnliftIO m, MonadFail m, HasSpanContext r)
  => InjectSelector (TcpClientSelector (Handshake MarloweRuntime)) s
  -> HostName
  -> PortNumber
  -> MarloweT m a
  -> m a
connectToMarloweRuntimeTraced inj host port action =
  runMarloweT action $
    tcpClientTraced inj host port marloweRuntimeClientPeer

connectToMarloweRuntime :: (MonadUnliftIO m, MonadFail m) => HostName -> PortNumber -> MarloweT m a -> m a
connectToMarloweRuntime host port action =
  runMarloweT action $
    tcpClient host port marloweRuntimeClientPeer
