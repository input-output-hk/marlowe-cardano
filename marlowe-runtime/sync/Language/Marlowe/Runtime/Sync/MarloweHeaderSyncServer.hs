{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.Sync.MarloweHeaderSyncServer
  where

import Control.Concurrent.Component
import Language.Marlowe.Protocol.HeaderSync.Server
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader, ChainPoint, WithGenesis(..))
import Language.Marlowe.Runtime.Sync.Database (DatabaseQueries(..), Next(..))
import Network.Protocol.Connection (SomeConnectionSource, SomeServerConnector, acceptSomeConnector)
import Network.Protocol.Driver (runSomeConnector)

data MarloweHeaderSyncServerDependencies = MarloweHeaderSyncServerDependencies
  { databaseQueries :: DatabaseQueries IO
  , headerSyncSource :: SomeConnectionSource MarloweHeaderSyncServer IO
  }

marloweHeaderSyncServer :: Component IO MarloweHeaderSyncServerDependencies ()
marloweHeaderSyncServer = serverComponent (component_ worker) \MarloweHeaderSyncServerDependencies{..} -> do
  connector <- acceptSomeConnector headerSyncSource
  pure WorkerDependencies{..}

data WorkerDependencies = WorkerDependencies
  { databaseQueries :: DatabaseQueries IO
  , connector :: SomeServerConnector MarloweHeaderSyncServer IO
  }

worker :: WorkerDependencies -> IO ()
worker WorkerDependencies{..} = do
  runSomeConnector connector $ MarloweHeaderSyncServer $ pure $ serverIdle Genesis
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
