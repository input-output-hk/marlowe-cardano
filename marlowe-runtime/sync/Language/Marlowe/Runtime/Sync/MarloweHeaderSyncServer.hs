{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.Sync.MarloweHeaderSyncServer
  where

import Control.Concurrent.Component.UnliftIO
import Control.Monad.Trans.Control (MonadBaseControl)
import Language.Marlowe.Protocol.HeaderSync.Server
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader, ChainPoint, WithGenesis(..))
import Language.Marlowe.Runtime.Sync.Database (DatabaseQueries(..), Next(..))
import Network.Protocol.Connection (SomeConnectionSource, SomeServerConnector, acceptSomeConnector)
import Network.Protocol.Driver (runSomeConnector)
import UnliftIO (MonadUnliftIO)

data MarloweHeaderSyncServerDependencies m = MarloweHeaderSyncServerDependencies
  { databaseQueries :: DatabaseQueries m
  , headerSyncSource :: SomeConnectionSource MarloweHeaderSyncServer m
  }

marloweHeaderSyncServer :: (MonadBaseControl IO m, MonadUnliftIO m) => Component m (MarloweHeaderSyncServerDependencies m) ()
marloweHeaderSyncServer = serverComponent (component_ worker) \MarloweHeaderSyncServerDependencies{..} -> do
  connector <- acceptSomeConnector headerSyncSource
  pure WorkerDependencies{..}

data WorkerDependencies m = WorkerDependencies
  { databaseQueries :: DatabaseQueries m
  , connector :: SomeServerConnector MarloweHeaderSyncServer m
  }

worker :: forall m. MonadBaseControl IO m => WorkerDependencies m -> m ()
worker WorkerDependencies{..} = do
  runSomeConnector connector $ MarloweHeaderSyncServer $ pure $ serverIdle Genesis
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
