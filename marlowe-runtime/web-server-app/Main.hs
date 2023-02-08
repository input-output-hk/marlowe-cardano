{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main
  where

import Control.Concurrent.Component (runComponent_)
import Control.Exception (throwIO)
import Language.Marlowe.Protocol.HeaderSync.Client (marloweHeaderSyncClientPeer)
import Language.Marlowe.Protocol.HeaderSync.Codec (codecMarloweHeaderSync)
import Language.Marlowe.Protocol.Sync.Client (marloweSyncClientPeer)
import Language.Marlowe.Protocol.Sync.Codec (codecMarloweSync)
import Language.Marlowe.Runtime.Web.Server
import Network.Protocol.Handshake.Client (runClientPeerOverSocketWithHandshake)
import Network.Protocol.Job.Client (jobClientPeer)
import Network.Protocol.Job.Codec (codecJob)
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
  >>= runComponent_ server

optionsToServerDependencies :: Options -> IO (ServerDependencies JSONRef)
optionsToServerDependencies Options{..} = do
  discoverySyncAddr <- resolve discoveryHost discoverySyncPort
  historySyncAddr <- resolve historyHost historySyncPort
  txCommandAddr <- resolve txHost txCommandPort
  eventBackend <- simpleJsonStderrBackend defaultRenderSelectorJSON
  pure ServerDependencies
    { openAPIEnabled
    , accessControlAllowOriginAll
    , runApplication = run $ fromIntegral port
    , runMarloweHeaderSyncClient = runClientPeerOverSocketWithHandshake
        throwIO
        discoverySyncAddr
        codecMarloweHeaderSync
        marloweHeaderSyncClientPeer
    , runMarloweSyncClient = runClientPeerOverSocketWithHandshake
        throwIO
        historySyncAddr
        codecMarloweSync
        marloweSyncClientPeer
    , runTxJobClient = runClientPeerOverSocketWithHandshake
        throwIO
        txCommandAddr
        codecJob
        jobClientPeer
    , eventBackend
    }

resolve :: HostName -> PortNumber -> IO AddrInfo
resolve host port =
  head <$> getAddrInfo (Just defaultHints { addrSocketType = Stream }) (Just host) (Just $ show port)
