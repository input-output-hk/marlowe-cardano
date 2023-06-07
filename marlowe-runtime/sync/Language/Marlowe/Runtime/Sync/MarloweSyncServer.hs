{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.Sync.MarloweSyncServer where

import Colog (Message, WithLog)
import Control.Concurrent.Component
import Data.Type.Equality (testEquality, type (:~:)(Refl))
import Language.Marlowe.Protocol.Sync.Server
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader, ChainPoint, WithGenesis(..))
import Language.Marlowe.Runtime.Core.Api (ContractId, MarloweVersion, SomeMarloweVersion(..))
import Language.Marlowe.Runtime.History.Api (SomeCreateStep(..))
import Language.Marlowe.Runtime.Sync.Database (DatabaseQueries(..), Next(..))
import Network.Protocol.Connection (ConnectionSource, Connector, acceptConnector, runConnector)
import UnliftIO (MonadUnliftIO)

data MarloweSyncServerDependencies m = MarloweSyncServerDependencies
  { databaseQueries :: DatabaseQueries m
  , syncSource :: ConnectionSource MarloweSyncServer m
  }

marloweSyncServer
  :: (MonadUnliftIO m, WithLog env Message m)
  => Component m (MarloweSyncServerDependencies m) ()
marloweSyncServer = serverComponent "marlowe-sync-server" (component_ "marlowe-sync-worker" worker) \MarloweSyncServerDependencies{..} -> do
  connector <- acceptConnector syncSource
  pure WorkerDependencies{..}

data WorkerDependencies m = WorkerDependencies
  { databaseQueries :: DatabaseQueries m
  , connector :: Connector MarloweSyncServer m
  }

worker :: forall m. (MonadUnliftIO m) => WorkerDependencies m -> m ()
worker WorkerDependencies{..} = do
  runConnector connector $ MarloweSyncServer $ pure serverInit
  where
    DatabaseQueries{..} = databaseQueries

    serverInit :: ServerStInit m ()
    serverInit = ServerStInit
      { recvMsgFollowContract
      , recvMsgIntersect
      }

    recvMsgFollowContract :: ContractId -> m (ServerStFollow m ())
    recvMsgFollowContract contractId = do
      mCreateStep <- getCreateStep contractId
      pure case mCreateStep of
        Nothing -> SendMsgContractNotFound ()
        Just (block, SomeCreateStep version createStep) ->
          SendMsgContractFound block version createStep $ serverIdle block contractId version Genesis

    recvMsgIntersect :: ContractId -> MarloweVersion v -> [BlockHeader] -> m (ServerStIntersect v m ())
    recvMsgIntersect contractId version points = do
      mCreateBlock <- fmap fst <$> getCreateStep contractId
      mIntersection <- getIntersectionForContract contractId points
      pure case (,) <$> mCreateBlock <*> mIntersection of
        Nothing -> SendMsgIntersectNotFound ()
        Just (createBlock, (block, SomeMarloweVersion version')) -> case testEquality version version' of
          Nothing -> SendMsgIntersectNotFound ()
          Just Refl -> SendMsgIntersectFound block $ serverIdle createBlock contractId version $ At block

    serverIdle :: forall v. BlockHeader -> ContractId -> MarloweVersion v -> ChainPoint -> ServerStIdle v m ()
    serverIdle createBlock contractId version clientPos = ServerStIdle
      { recvMsgRequestNext
      , recvMsgDone = pure ()
      }
      where
        nextIdle = serverIdle createBlock contractId version
        recvMsgRequestNext = do
          nextSteps <- getNextSteps version contractId clientPos
          pure case nextSteps of
            Rollback Genesis -> SendMsgRollBackCreation ()
            Rollback (At targetBlock)
              | targetBlock < createBlock -> SendMsgRollBackCreation ()
              | otherwise -> SendMsgRollBackward targetBlock $ nextIdle $ At targetBlock
            Wait -> SendMsgWait serverWait
            Next nextBlock steps -> SendMsgRollForward nextBlock steps $ nextIdle $ At nextBlock

        serverWait :: ServerStWait v m ()
        serverWait = ServerStWait
          { recvMsgPoll = do
              contractTip <- getTipForContract contractId
              case contractTip of
                Genesis -> pure $ SendMsgRollBackCreation ()
                _
                  | clientPos /= contractTip -> recvMsgRequestNext
                  | otherwise -> pure $ SendMsgWait serverWait
          , recvMsgCancel = pure $ nextIdle clientPos
          }
