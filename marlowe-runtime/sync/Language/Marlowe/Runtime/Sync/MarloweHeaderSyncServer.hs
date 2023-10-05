{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.Sync.MarloweHeaderSyncServer where

import Language.Marlowe.Protocol.HeaderSync.Server
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader, ChainPoint, WithGenesis (..))
import Language.Marlowe.Runtime.Sync.Database (DatabaseQueries (..), Next (..))
import Network.Protocol.Connection (ServerSource (..))
import UnliftIO (MonadUnliftIO)

newtype MarloweHeaderSyncServerDependencies m = MarloweHeaderSyncServerDependencies
  { databaseQueries :: DatabaseQueries m
  }

marloweHeaderSyncServer
  :: forall m
   . (MonadUnliftIO m)
  => MarloweHeaderSyncServerDependencies m
  -> ServerSource MarloweHeaderSyncServer m ()
marloweHeaderSyncServer MarloweHeaderSyncServerDependencies{..} = ServerSource $ pure server
  where
    server = MarloweHeaderSyncServer $ pure $ serverIdle Genesis
    DatabaseQueries{..} = databaseQueries

    serverIdle :: ChainPoint -> ServerStIdle m ()
    serverIdle clientPos =
      ServerStIdle
        { recvMsgRequestNext
        , recvMsgIntersect
        , recvMsgDone = pure ()
        }
      where
        recvMsgRequestNext = do
          nextHeaders <- getNextHeaders clientPos
          pure case nextHeaders of
            Rollback targetPoint _ -> SendMsgRollBackward targetPoint $ serverIdle targetPoint
            Wait ->
              SendMsgWait
                ServerStWait
                  { recvMsgPoll = recvMsgRequestNext
                  , recvMsgCancel = pure $ serverIdle clientPos
                  }
            Next nextBlock _ headers -> SendMsgNewHeaders nextBlock headers $ serverIdle $ At nextBlock

        recvMsgIntersect :: [BlockHeader] -> m (ServerStIntersect m ())
        recvMsgIntersect points = do
          mIntersection <- getIntersection points
          pure case mIntersection of
            Nothing -> SendMsgIntersectNotFound $ serverIdle clientPos
            Just intersection -> SendMsgIntersectFound intersection $ serverIdle $ At intersection
