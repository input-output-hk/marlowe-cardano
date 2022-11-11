{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Main
  where

import Control.Exception (Exception, bracket, bracketOnError, throwIO)
import qualified Data.ByteString.Lazy as LB
import Language.Marlowe.Protocol.HeaderSync.Client (marloweHeaderSyncClientPeer)
import Language.Marlowe.Protocol.HeaderSync.Codec (codecMarloweHeaderSync)
import Language.Marlowe.Runtime.Web.Server
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

main :: IO ()
main = do
  discoverySyncAddr <- resolve "127.0.0.1" 3722
  Server{..} <- mkServer ServerDependencies
    { openAPIEnabled = True
    , runApplication = run 8080
    , runMarloweHeaderSyncClient = runClientPeerOverSocket
        discoverySyncAddr
        codecMarloweHeaderSync
        marloweHeaderSyncClientPeer
    }
  runServer
  where
    resolve :: HostName -> PortNumber -> IO AddrInfo
    resolve host port =
      head <$> getAddrInfo (Just defaultHints { addrSocketType = Stream }) (Just host) (Just $ show port)

-- | Run a client as a typed protocols peer over a socket.
runClientPeerOverSocket
  :: Exception ex
  => AddrInfo -- ^ Socket address to connect to
  -> Codec protocol ex IO LB.ByteString -- ^ A codec for the protocol
  -> (forall a. client IO a -> Peer protocol 'AsClient st IO a) -- ^ Interpret the client as a protocol peer
  -> (forall a. client IO a -> IO a)
runClientPeerOverSocket addr codec clientToPeer client = bracket open close \socket -> do
  let channel = socketAsChannel socket
  let driver = mkDriver throwIO codec channel
  let peer = clientToPeer client
  fst <$> runPeerWithDriver driver peer (startDState driver)
  where
    open = bracketOnError (openSocket addr) close \sock -> do
      connect sock $ addrAddress addr
      pure sock
