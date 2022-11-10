{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Main
  where

import Control.Concurrent.Async (mapConcurrently_)
import Control.Exception (Exception, bracket, bracketOnError, throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import qualified Data.ByteString.Lazy as LB
import Flags
import Language.Marlowe.Protocol.HeaderSync.Client (marloweHeaderSyncClientPeer)
import Language.Marlowe.Protocol.HeaderSync.Codec (codecMarloweHeaderSync)
import Monad (AppEnv(..), AppM(..))
import Network.Channel (socketAsChannel)
import Network.Protocol.Driver (mkDriver)
import Network.Socket
  ( AddrInfo(..)
  , HostName
  , PortNumber
  , SocketType(..)
  , addrAddress
  , close
  , connect
  , defaultHints
  , getAddrInfo
  , openSocket
  )
import Network.TypedProtocol (Peer, PeerRole(..), runPeerWithDriver, startDState)
import Network.TypedProtocol.Codec (Codec)
import Network.Wai.Handler.Warp (run)
import Process.ContractHeaderIndexer (ContractHeaderIndexer(..), RunClient, mkContractHeaderIndexer)
import Servant
import qualified Server

main :: IO ()
main = do
  discoverySyncAddr <- resolve "127.0.0.1" 3722
  ContractHeaderIndexer{..} <- mkContractHeaderIndexer
    $ runClientPeerOverSocket discoverySyncAddr codecMarloweHeaderSync marloweHeaderSyncClientPeer
  let
    env = AppEnv
      { _loadContractHeaders = liftIO . loadContractHeaders
      }
  mapConcurrently_ id
    [ run 8080 $ app env Enabled
    , runContractHeaderIndexer
    ]
  where
    resolve :: HostName -> PortNumber -> IO AddrInfo
    resolve host port =
      head <$> getAddrInfo (Just defaultHints { addrSocketType = Stream }) (Just host) (Just $ show port)

app :: AppEnv -> Flag openAPIFlag -> Application
app env Enabled = serve api $ hoistServer api (flip runReaderT env . runAppM) $ Server.server Enabled
  where
    api = Server.api Enabled
app env Disabled = serve api $ hoistServer api (flip runReaderT env . runAppM) $ Server.server Disabled
  where
    api = Server.api Disabled

-- | Run a client as a typed protocols peer over a socket.
runClientPeerOverSocket
  :: Exception ex
  => AddrInfo -- ^ Socket address to connect to
  -> Codec protocol ex IO LB.ByteString -- ^ A codec for the protocol
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
