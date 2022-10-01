{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Language.Marlowe.Runtime.Discovery.SyncServer
  where

import Control.Concurrent.Async (Concurrently(Concurrently, runConcurrently))
import Control.Concurrent.STM (STM, atomically)
import Control.Exception (SomeException, catch)
import Language.Marlowe.Protocol.HeaderSync.Server
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader, ChainPoint, WithGenesis(..))
import Language.Marlowe.Runtime.Discovery.Api
import System.IO (hPutStrLn, stderr)

newtype RunSyncServer m = RunSyncServer (forall a. MarloweHeaderSyncServer m a -> IO a)

data DiscoverySyncServerDependencies = DiscoverySyncServerDependencies
  { acceptRunSyncServer :: IO (RunSyncServer IO)
  , getNextHeaders :: ChainPoint -> IO (Maybe (Either ChainPoint (BlockHeader, [ContractHeader])))
  , getIntersect :: [BlockHeader] -> IO (Maybe BlockHeader)
  }

newtype DiscoverySyncServer = DiscoverySyncServer
  { runDiscoverySyncServer :: IO ()
  }

mkDiscoverySyncServer :: DiscoverySyncServerDependencies -> STM DiscoverySyncServer
mkDiscoverySyncServer DiscoverySyncServerDependencies{..} = do
  let
    runDiscoverySyncServer = do
      runSyncServer <- acceptRunSyncServer
      Worker{..} <- atomically $ mkWorker WorkerDependencies {..}
      runConcurrently $
        Concurrently (runWorker `catch` catchWorker) *> Concurrently runDiscoverySyncServer
  pure $ DiscoverySyncServer { runDiscoverySyncServer }

catchWorker :: SomeException -> IO ()
catchWorker = hPutStrLn stderr . ("Sync worker crashed with exception: " <>) . show

data WorkerDependencies = WorkerDependencies
  { runSyncServer     :: RunSyncServer IO
  , getNextHeaders :: ChainPoint -> IO (Maybe (Either ChainPoint (BlockHeader, [ContractHeader])))
  , getIntersect :: [BlockHeader] -> IO (Maybe BlockHeader)
  }

newtype Worker = Worker
  { runWorker :: IO ()
  }

mkWorker :: WorkerDependencies -> STM Worker
mkWorker WorkerDependencies{..} =
  let
    RunSyncServer run = runSyncServer
  in
    pure Worker { runWorker = run server }

  where
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
