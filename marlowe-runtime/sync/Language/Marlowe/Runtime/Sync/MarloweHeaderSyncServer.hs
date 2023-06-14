{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.Sync.MarloweHeaderSyncServer where

import Colog (Message, WithLog)
import Control.Concurrent.Component
import Control.Monad.Event.Class (MonadEvent)
import Language.Marlowe.Protocol.HeaderSync.Server
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader, ChainPoint, WithGenesis(..))
import Language.Marlowe.Runtime.Sync.Database (DatabaseQueries(..), Next(..))
import Network.Protocol.Connection (SomeConnectionSourceTraced, SomeServerConnectorTraced, acceptSomeConnectorTraced)
import Network.Protocol.Driver.Trace (HasSpanContext, runSomeConnectorTraced)
import UnliftIO (MonadUnliftIO)

data MarloweHeaderSyncServerDependencies r s m = MarloweHeaderSyncServerDependencies
  { databaseQueries :: DatabaseQueries m
  , headerSyncSource :: SomeConnectionSourceTraced MarloweHeaderSyncServer r s m
  }

marloweHeaderSyncServer
  :: (MonadUnliftIO m, MonadEvent r s m, HasSpanContext r, WithLog env Message m)
  => Component m (MarloweHeaderSyncServerDependencies r s m) ()
marloweHeaderSyncServer = serverComponent "marlowe-header-sync-server" (component_ "marlowe-header-sync-worker" worker) \MarloweHeaderSyncServerDependencies{..} -> do
  connector <- acceptSomeConnectorTraced headerSyncSource
  pure WorkerDependencies{..}

data WorkerDependencies r s m = WorkerDependencies
  { databaseQueries :: DatabaseQueries m
  , connector :: SomeServerConnectorTraced MarloweHeaderSyncServer r s m
  }

worker
  :: forall r s m
   . (MonadUnliftIO m, MonadEvent r s m, HasSpanContext r)
  => WorkerDependencies r s m
  -> m ()
worker WorkerDependencies{..} = do
  runSomeConnectorTraced connector $ MarloweHeaderSyncServer $ pure $ serverIdle Genesis
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
