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
import Language.Marlowe.Protocol.HeaderSync.Server
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader, ChainPoint, WithGenesis(..))
import Language.Marlowe.Runtime.Discovery.Api
import Network.Protocol.Driver (RunServer(..))
import Observe.Event (EventBackend)
import Observe.Event.DSL (SelectorSpec(..))
import Observe.Event.Render.JSON.DSL.Compile (compile)
import Observe.Event.Syntax ((≔))
import System.IO (hPutStrLn, stderr)

compile $ SelectorSpec ["discovery", "sync", "server"]
  [ "todo" ≔ ''()
  ]

type RunSyncServer m = RunServer m MarloweHeaderSyncServer

data DiscoverySyncServerDependencies r = DiscoverySyncServerDependencies
  { acceptRunSyncServer :: IO (RunSyncServer IO)
  , getNextHeaders :: ChainPoint -> IO (Maybe (Either ChainPoint (BlockHeader, [ContractHeader])))
  , getIntersect :: [BlockHeader] -> IO (Maybe BlockHeader)
  , eventBackend :: EventBackend IO r DiscoverySyncServerSelector
  }

discoverySyncServer :: Component IO (DiscoverySyncServerDependencies r) ()
discoverySyncServer = serverComponent
  worker
  (hPutStrLn stderr . ("Sync worker crashed with exception: " <>) . show)
  (hPutStrLn stderr "Sync client terminated normally")
  \DiscoverySyncServerDependencies{..} -> do
      runSyncServer <- acceptRunSyncServer
      pure WorkerDependencies {..}

data WorkerDependencies = WorkerDependencies
  { runSyncServer     :: RunSyncServer IO
  , getNextHeaders :: ChainPoint -> IO (Maybe (Either ChainPoint (BlockHeader, [ContractHeader])))
  , getIntersect :: [BlockHeader] -> IO (Maybe BlockHeader)
  }

newtype Worker = Worker
  { runWorker :: IO ()
  }

worker :: Component IO WorkerDependencies ()
worker = component_ \WorkerDependencies{..} -> do
  let
    RunServer run = runSyncServer

    server :: MarloweHeaderSyncServer IO ()
    server = MarloweHeaderSyncServer $ pure $ idleServer Genesis

    intersectServer :: ChainPoint -> [BlockHeader] -> IO (ServerStIntersect IO ())
    intersectServer point blockHeaders = do
      result <- getIntersect blockHeaders
      pure case result of
        Nothing          -> SendMsgIntersectNotFound $ idleServer point
        Just blockHeader -> SendMsgIntersectFound blockHeader $ idleServer $ At blockHeader

    idleServer :: ChainPoint -> ServerStIdle IO ()
    idleServer point = ServerStIdle
      { recvMsgDone = pure ()
      , recvMsgRequestNext = nextServer point
      , recvMsgIntersect = intersectServer point
      }

    nextServer :: ChainPoint -> IO (ServerStNext IO ())
    nextServer point = do
      result <- getNextHeaders point
      pure case result of
        Nothing -> SendMsgWait $ waitServer point
        Just (Left point') -> SendMsgRollBackward point' $ idleServer point'
        Just (Right (block, headers)) -> SendMsgNewHeaders block headers $ idleServer $ At block

    waitServer :: ChainPoint -> ServerStWait IO ()
    waitServer point = ServerStWait
      { recvMsgPoll = nextServer point
      , recvMsgCancel = pure $ idleServer point
      }
  run server
