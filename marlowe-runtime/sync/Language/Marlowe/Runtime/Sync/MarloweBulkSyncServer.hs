{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.Sync.MarloweBulkSyncServer where

import Language.Marlowe.Protocol.BulkSync.Server
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader, ChainPoint, WithGenesis (..))
import Language.Marlowe.Runtime.Sync.Database (DatabaseQueries (..), Next (..))
import Network.Protocol.Connection (ServerSource (..))
import UnliftIO (MonadUnliftIO)

newtype MarloweBulkSyncServerDependencies m = MarloweBulkSyncServerDependencies
  { databaseQueries :: DatabaseQueries m
  }

marloweBulkSyncServer
  :: forall m
   . (MonadUnliftIO m)
  => MarloweBulkSyncServerDependencies m
  -> ServerSource MarloweBulkSyncServer m ()
marloweBulkSyncServer MarloweBulkSyncServerDependencies{..} = ServerSource $ pure server
  where
    server = MarloweBulkSyncServer $ pure $ serverIdle Genesis
    DatabaseQueries{..} = databaseQueries

    serverIdle :: ChainPoint -> ServerStIdle m ()
    serverIdle clientPos =
      ServerStIdle
        { recvMsgRequestNext
        , recvMsgIntersect
        , recvMsgDone = pure ()
        }
      where
        recvMsgRequestNext batchSize = do
          results <- getNextBlocks batchSize clientPos
          pure case results of
            Rollback targetPoint tip -> SendMsgRollBackward targetPoint tip $ serverIdle targetPoint
            Wait ->
              SendMsgWait
                ServerStPoll
                  { recvMsgPoll = recvMsgRequestNext batchSize
                  , recvMsgCancel = pure $ serverIdle clientPos
                  }
            Next newPos tip blocks -> SendMsgRollForward blocks tip $ serverIdle $ At newPos

        recvMsgIntersect :: [BlockHeader] -> m (ServerStIntersect m ())
        recvMsgIntersect points = do
          tip <- getTip
          case tip of
            Genesis -> pure $ SendMsgIntersectNotFound Genesis $ serverIdle clientPos
            At tipBlock -> do
              mIntersection <- getIntersection points
              pure case mIntersection of
                Nothing -> SendMsgIntersectNotFound (At tipBlock) $ serverIdle clientPos
                Just intersection -> SendMsgIntersectFound intersection tipBlock $ serverIdle $ At intersection
