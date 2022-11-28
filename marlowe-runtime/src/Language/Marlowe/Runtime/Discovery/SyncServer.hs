{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Marlowe.Runtime.Discovery.SyncServer
  where

import Control.Concurrent.Component
import Control.Exception (displayException)
import Data.Foldable (traverse_)
import Data.Void (Void)
import Language.Marlowe.Protocol.HeaderSync.Server
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader, ChainPoint, WithGenesis(..))
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Discovery.Store (NextHeaders(..))
import Network.Protocol.Driver (RunServer(..))
import Observe.Event (Event, EventBackend, addField, reference, subEventBackend, withEvent)
import Observe.Event.BackendModification (modifyEventBackend, setInitialCause)
import Observe.Event.DSL (FieldSpec(..), SelectorField(..), SelectorSpec(..))
import Observe.Event.Render.JSON.DSL.Compile (compile)
import Observe.Event.Syntax ((≔))

type BlockHeaders = [Chain.BlockHeader]

compile $ SelectorSpec "worker"
  [ "done" ≔ ''Void
  , ["request", "next"] ≔ ''NextHeaders
  , "intersect" ≔ FieldSpec "intersect"
      [ "headers" ≔ ''BlockHeaders
      , "found" ≔ ''BlockHeader
      ]

  , "poll" ≔ ''NextHeaders
  , "cancel" ≔ ''Void
  ]

compile $ SelectorSpec ["discovery", "sync", "server"]
  [ ["new", "client"] ≔ ''Void
  , "worker" ≔ Inject ''WorkerSelector
  , ["worker", "crashed"] ≔ ''String
  , ["worker", "terminated"] ≔ ''Void
  ]

type RunSyncServer m = RunServer m MarloweHeaderSyncServer

data DiscoverySyncServerDependencies r = DiscoverySyncServerDependencies
  { acceptRunSyncServer :: IO (RunSyncServer IO)
  , getNextHeaders :: ChainPoint -> IO NextHeaders
  , getIntersect :: [BlockHeader] -> IO (Maybe BlockHeader)
  , eventBackend :: EventBackend IO r DiscoverySyncServerSelector
  }

discoverySyncServer :: Component IO (DiscoverySyncServerDependencies r) ()
discoverySyncServer = serverComponentWith
  worker
  (\eventBackend ex -> do
    withEvent eventBackend WorkerCrashed \ev -> addField ev $ displayException ex
  )
  (\eventBackend -> do
    withEvent eventBackend WorkerTerminated $ const $ pure ()
  )
  \DiscoverySyncServerDependencies{..} -> do
      runSyncServer <- acceptRunSyncServer
      withEvent eventBackend NewClient \ev -> do
        pure
          ( modifyEventBackend (setInitialCause $ reference ev) eventBackend
          , WorkerDependencies
              { runSyncServer
              , getNextHeaders
              , getIntersect
              , eventBackend = subEventBackend Worker ev
              }
          )

data WorkerDependencies r = WorkerDependencies
  { runSyncServer :: RunSyncServer IO
  , getNextHeaders :: ChainPoint -> IO NextHeaders
  , getIntersect :: [BlockHeader] -> IO (Maybe BlockHeader)
  , eventBackend :: EventBackend IO r WorkerSelector
  }

worker :: forall r. Component IO (WorkerDependencies r) ()
worker = component_ \WorkerDependencies{..} -> do
  let
    RunServer run = runSyncServer

    withEvent' :: WorkerSelector f -> (Event IO r WorkerSelector f -> IO a) -> IO a
    withEvent' = withEvent eventBackend

    server :: MarloweHeaderSyncServer IO ()
    server = MarloweHeaderSyncServer $ pure $ idleServer Genesis

    idleServer :: ChainPoint -> ServerStIdle IO ()
    idleServer point = ServerStIdle
      { recvMsgDone = withEvent' Done $ const $ pure ()
      , recvMsgRequestNext = withEvent' RequestNext $ nextServer point
      , recvMsgIntersect = withEvent' Intersect . intersectServer point
      }

    nextServer :: ChainPoint -> Event IO r WorkerSelector NextHeaders -> IO (ServerStNext IO ())
    nextServer point ev = do
      result <- getNextHeaders point
      addField ev result
      pure case result of
        Wait -> SendMsgWait $ waitServer point
        RollBackward point' -> SendMsgRollBackward point' $ idleServer point'
        RollForward block headers -> SendMsgNewHeaders block headers $ idleServer $ At block

    intersectServer
      :: ChainPoint
      -> [BlockHeader]
      -> Event IO r WorkerSelector IntersectField
      -> IO (ServerStIntersect IO ())
    intersectServer point blockHeaders ev = do
      addField ev $ Headers blockHeaders
      result <- getIntersect blockHeaders
      traverse_ (addField ev . Found) result
      pure case result of
        Nothing          -> SendMsgIntersectNotFound $ idleServer point
        Just blockHeader -> SendMsgIntersectFound blockHeader $ idleServer $ At blockHeader

    waitServer :: ChainPoint -> ServerStWait IO ()
    waitServer point = ServerStWait
      { recvMsgPoll = withEvent' Poll $ nextServer point
      , recvMsgCancel = withEvent' Cancel $ const $ pure $ idleServer point
      }
  run server
