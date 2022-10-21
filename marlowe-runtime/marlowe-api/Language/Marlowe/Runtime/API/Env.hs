{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.API.Env
  where

import Control.Concurrent.STM (STM)
import Control.Exception (Exception, bracket, bracketOnError, throwIO)
import Data.ByteString.Lazy (ByteString)
import Language.Marlowe.Protocol.Sync.Client (MarloweSyncClient)
import Language.Marlowe.Runtime.Discovery.Api (DiscoveryQuery)
import Language.Marlowe.Runtime.History.Api (HistoryCommand, HistoryQuery)
import Language.Marlowe.Runtime.Transaction.Api (MarloweTxCommand)
import Network.Channel (socketAsChannel)
import Network.Protocol.Driver (mkDriver)
import Network.Protocol.Job.Client (JobClient)
import Network.Protocol.Query.Client (QueryClient)
import Network.Socket (AddrInfo, addrAddress, close, connect, openSocket)
import Network.TypedProtocol (Driver(startDState), Peer, PeerRole(AsClient), runPeerWithDriver)
import Network.TypedProtocol.Codec (Codec)

-- | A function signature for running a client for some protocol in some monad m.
type RunClient m client = forall a. client m a -> m a

-- | The environment for the Marlowe Runtime CLI.
data Env m = Env
  { envRunHistoryJobClient :: !(RunClient m (JobClient HistoryCommand))
  , envRunHistoryQueryClient :: !(RunClient m (QueryClient HistoryQuery))
  , envRunHistorySyncClient :: !(RunClient m MarloweSyncClient)
  , envRunDiscoveryQueryClient :: !(RunClient m (QueryClient DiscoveryQuery))
  , envRunTxJobClient :: !(RunClient m (JobClient MarloweTxCommand))
  , sigInt :: STM ()
  }

-- | Run a client as a typed protocols peer over a socket.
runClientPeerOverSocket
  :: Exception ex
  => AddrInfo -- ^ Socket address to connect to
  -> Codec protocol ex IO ByteString -- ^ A codec for the protocol
  -> (forall a. client IO a -> Peer protocol 'AsClient st IO a) -- ^ Interpret the client as a protocol peer
  -> RunClient IO client
runClientPeerOverSocket addr codec clientToPeer client = bracket open close \socket -> do
  let channel = socketAsChannel socket
  let driver = mkDriver throwIO codec channel
  let peer = clientToPeer client
  fst <$> runPeerWithDriver driver peer (startDState driver)
  where
    open = bracketOnError (openSocket addr) close \sock -> do
      connect sock $ addrAddress addr
      pure sock
