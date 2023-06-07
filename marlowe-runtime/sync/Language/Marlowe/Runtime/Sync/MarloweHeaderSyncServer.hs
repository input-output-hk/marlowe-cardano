{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.Sync.MarloweHeaderSyncServer where

import Colog (Message, WithLog)
import Control.Concurrent.Component
import Language.Marlowe.Protocol.HeaderSync.Server
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader, ChainPoint, WithGenesis(..))
import Language.Marlowe.Runtime.Sync.Database (DatabaseQueries(..), Next(..))
import Network.Protocol.Connection (ConnectionSource, Connector, acceptConnector, runConnector)
import UnliftIO (MonadUnliftIO)

data MarloweHeaderSyncServerDependencies m = MarloweHeaderSyncServerDependencies
  { databaseQueries :: DatabaseQueries m
  , headerSyncSource :: ConnectionSource MarloweHeaderSyncServer m
  }

marloweHeaderSyncServer
  :: (MonadUnliftIO m, WithLog env Message m)
  => Component m (MarloweHeaderSyncServerDependencies m) ()
marloweHeaderSyncServer = serverComponent "marlowe-header-sync-server" (component_ "marlowe-header-sync-worker" worker) \MarloweHeaderSyncServerDependencies{..} -> do
  connector <- acceptConnector headerSyncSource
  pure WorkerDependencies{..}

data WorkerDependencies m = WorkerDependencies
  { databaseQueries :: DatabaseQueries m
  , connector :: Connector MarloweHeaderSyncServer m
  }

worker :: forall m. MonadUnliftIO m => WorkerDependencies m -> m ()
worker WorkerDependencies{..} = do
  runConnector connector $ MarloweHeaderSyncServer $ pure $ serverIdle Genesis
  where
    DatabaseQueries{..} = databaseQueries

    serverIdle :: ChainPoint -> ServerStIdle m ()
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

        recvMsgIntersect :: [BlockHeader] -> m (ServerStIntersect m ())
        recvMsgIntersect points = do
          mIntersection <- getIntersection points
          pure case mIntersection of
            Nothing -> SendMsgIntersectNotFound $ serverIdle clientPos
            Just intersection -> SendMsgIntersectFound intersection $ serverIdle $ At intersection
