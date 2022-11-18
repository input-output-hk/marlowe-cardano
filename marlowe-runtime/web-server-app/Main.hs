{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main
  where

import Control.Concurrent.STM (atomically)
import Control.Exception (throwIO)
import Language.Marlowe.Protocol.HeaderSync.Client (marloweHeaderSyncClientPeer)
import Language.Marlowe.Protocol.HeaderSync.Codec (codecMarloweHeaderSync)
import Language.Marlowe.Runtime.Web.Server
import Network.Protocol.Driver (runClientPeerOverSocket)
import Network.Socket (AddrInfo(..), HostName, PortNumber, SocketType(..), defaultHints, getAddrInfo)
import Network.Wai.Handler.Warp (run)
import Observe.Event.Render.JSON (DefaultRenderSelectorJSON(defaultRenderSelectorJSON))
import Observe.Event.Render.JSON.Handle (JSONRef, simpleJsonStderrBackend)
import Options
import System.IO (BufferMode(..), hSetBuffering, stderr, stdout)

main :: IO ()
main = hSetBuffering stdout LineBuffering
  >> hSetBuffering stderr LineBuffering
  >> getOptions
  >>= optionsToServerDependencies
  >>= atomically . mkServer
  >>= runServer

optionsToServerDependencies :: Options -> IO (ServerDependencies JSONRef)
optionsToServerDependencies Options{..} = do
  discoverySyncAddr <- resolve discoveryHost discoverySyncPort
  eventBackend <- simpleJsonStderrBackend defaultRenderSelectorJSON
  pure ServerDependencies
    { openAPIEnabled
    , runApplication = run $ fromIntegral port
    , runMarloweHeaderSyncClient = runClientPeerOverSocket
        throwIO
        discoverySyncAddr
        codecMarloweHeaderSync
        marloweHeaderSyncClientPeer
    , eventBackend
    }

resolve :: HostName -> PortNumber -> IO AddrInfo
resolve host port =
  head <$> getAddrInfo (Just defaultHints { addrSocketType = Stream }) (Just host) (Just $ show port)
