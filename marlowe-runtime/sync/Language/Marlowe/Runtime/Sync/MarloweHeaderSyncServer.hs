{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.Sync.MarloweHeaderSyncServer
  where

import Control.Concurrent.Component
import Language.Marlowe.Protocol.HeaderSync.Server
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader, ChainPoint, WithGenesis(..))
import Language.Marlowe.Runtime.Sync.Database (DatabaseQueries(..), Next(..))
import Network.Protocol.Driver (RunServer(..))

data MarloweHeaderSyncServerDependencies = MarloweHeaderSyncServerDependencies
  { databaseQueries :: DatabaseQueries IO
  , acceptRunMarloweHeaderSyncServer :: IO (RunServer IO MarloweHeaderSyncServer)
  }

marloweHeaderSyncServer :: Component IO MarloweHeaderSyncServerDependencies ()
marloweHeaderSyncServer = serverComponent (component_ worker) \MarloweHeaderSyncServerDependencies{..} -> do
  runMarloweHeaderSyncServer <- acceptRunMarloweHeaderSyncServer
  pure WorkerDependencies{..}

data WorkerDependencies = WorkerDependencies
  { databaseQueries :: DatabaseQueries IO
  , runMarloweHeaderSyncServer :: RunServer IO MarloweHeaderSyncServer
  }

worker :: WorkerDependencies -> IO ()
worker WorkerDependencies{..} = do
  let RunServer runServer = runMarloweHeaderSyncServer
  runServer $ MarloweHeaderSyncServer $ pure $ serverIdle Genesis
  where
    DatabaseQueries{..} = databaseQueries

    serverIdle :: ChainPoint -> ServerStIdle IO ()
    serverIdle clientPos = ServerStIdle
      { recvMsgRequestNext
      , recvMsgIntersect
      , recvMsgDone = pure ()
      }
      where
        recvMsgRequestNext = do
          nextHeaders <- getNextHeaders clientPos
          pure case nextHeaders of
            Rollback targetPoint -> SendMsgRollBackward targetPoint $ serverIdle targetPoint
            Wait -> SendMsgWait ServerStWait
              { recvMsgPoll = recvMsgRequestNext
              , recvMsgCancel = pure $ serverIdle clientPos
              }
            Next nextBlock headers -> SendMsgNewHeaders nextBlock headers $ serverIdle $ At nextBlock

        recvMsgIntersect :: [BlockHeader] -> IO (ServerStIntersect IO ())
        recvMsgIntersect points = do
          mIntersection <- getIntersection points
          pure case mIntersection of
            Nothing -> SendMsgIntersectNotFound $ serverIdle clientPos
            Just intersection -> SendMsgIntersectFound intersection $ serverIdle $ At intersection
