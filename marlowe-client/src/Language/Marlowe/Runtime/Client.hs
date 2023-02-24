module Language.Marlowe.Runtime.Client
  where

import Control.Monad.Trans.Marlowe (MarloweT, runMarloweT)
import Language.Marlowe.Protocol.Client (marloweClientPeer)
import Network.Protocol.Connection (SomeConnector(..))
import Network.Protocol.Driver (tcpClient)
import Network.Protocol.Handshake.Client (handshakeClientConnector)
import Network.Socket (HostName, PortNumber)

connectToMarloweRuntime :: HostName -> PortNumber -> MarloweT m a -> m a
connectToMarloweRuntime host port action = runMarloweT action
  $ SomeConnector
  $ handshakeClientConnector
  $ tcpClient host port marloweClientPeer
