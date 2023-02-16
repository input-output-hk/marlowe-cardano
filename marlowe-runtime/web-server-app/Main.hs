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
import Language.Marlowe.Protocol.Query.Codec (codecMarloweQuery)
import Language.Marlowe.Runtime.Web.Server
import Network.Protocol.Handshake.Client (runClientPeerOverSocketWithHandshake)
import Network.Protocol.Job.Client (jobClientPeer)
import Network.Protocol.Job.Codec (codecJob)
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
    , runMarloweQueryClient = runClientPeerOverSocketWithHandshake
        syncHost
        syncQueryPort
        codecMarloweQuery
        marloweQueryClientPeer
    , runTxJobClient = runClientPeerOverSocketWithHandshake
        txHost
        txCommandPort
        codecJob
        jobClientPeer
    , eventBackend
    }
