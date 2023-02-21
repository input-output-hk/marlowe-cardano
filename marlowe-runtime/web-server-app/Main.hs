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
import Language.Marlowe.Protocol.Query.Client (marloweQueryClientPeer)
import Language.Marlowe.Runtime.Web.Server
import Network.Protocol.Driver (runConnector, tcpClient)
import Network.Protocol.Handshake.Client (handshakeClientConnector)
import Network.Protocol.Job.Client (jobClientPeer)
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
  eventBackend <- simpleJsonStderrBackend defaultRenderSelectorJSON
  pure ServerDependencies
    { openAPIEnabled
    , accessControlAllowOriginAll
    , runApplication = run $ fromIntegral port
    , runMarloweQueryClient = runConnector
        $ handshakeClientConnector
        $ tcpClient syncHost syncQueryPort marloweQueryClientPeer
    , runTxJobClient = runConnector
        $ handshakeClientConnector
        $ tcpClient txHost txCommandPort jobClientPeer
    , eventBackend
    }
