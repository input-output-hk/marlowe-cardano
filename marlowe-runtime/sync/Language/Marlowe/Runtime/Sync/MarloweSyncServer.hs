{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.Sync.MarloweSyncServer
  where

import Control.Concurrent.Component
import Data.Type.Equality (testEquality, type (:~:)(Refl))
import Language.Marlowe.Protocol.Sync.Server
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader, ChainPoint, WithGenesis(..))
import Language.Marlowe.Runtime.Core.Api (ContractId, MarloweVersion, SomeMarloweVersion(..))
import Language.Marlowe.Runtime.History.Api (SomeCreateStep(..))
import Language.Marlowe.Runtime.Sync.Database (DatabaseQueries(..), Next(..))
import Network.Protocol.Connection (SomeConnectionSource, SomeServerConnector, acceptSomeConnector)
import Network.Protocol.Driver (runSomeConnector)

data MarloweSyncServerDependencies = MarloweSyncServerDependencies
  { databaseQueries :: DatabaseQueries IO
  , syncSource :: SomeConnectionSource MarloweSyncServer IO
  }

marloweSyncServer :: Component IO MarloweSyncServerDependencies ()
marloweSyncServer = serverComponent (component_ worker) \MarloweSyncServerDependencies{..} -> do
  connector <- acceptSomeConnector syncSource
  pure WorkerDependencies{..}

data WorkerDependencies = WorkerDependencies
  { databaseQueries :: DatabaseQueries IO
  , connector :: SomeServerConnector MarloweSyncServer IO
  }

worker :: WorkerDependencies -> IO ()
worker WorkerDependencies{..} = do
  runSomeConnector connector $ MarloweSyncServer $ pure serverInit
  where
    DatabaseQueries{..} = databaseQueries

    serverInit :: ServerStInit IO ()
    serverInit = ServerStInit
      { recvMsgFollowContract
      , recvMsgIntersect
      }

    recvMsgFollowContract :: ContractId -> IO (ServerStFollow IO ())
    recvMsgFollowContract contractId = do
      mCreateStep <- getCreateStep contractId
      pure case mCreateStep of
        Nothing -> SendMsgContractNotFound ()
        Just (block, SomeCreateStep version createStep) ->
          SendMsgContractFound block version createStep $ serverIdle block contractId version Genesis

    recvMsgIntersect :: ContractId -> MarloweVersion v -> [BlockHeader] -> IO (ServerStIntersect v IO ())
    recvMsgIntersect contractId version points = do
      mCreateBlock <- fmap fst <$> getCreateStep contractId
      mIntersection <- getIntersectionForContract contractId points
      pure case (,) <$> mCreateBlock <*> mIntersection of
        Nothing -> SendMsgIntersectNotFound ()
        Just (createBlock, (block, SomeMarloweVersion version')) -> case testEquality version version' of
          Nothing -> SendMsgIntersectNotFound ()
          Just Refl -> SendMsgIntersectFound block $ serverIdle createBlock contractId version $ At block

    serverIdle :: forall v. BlockHeader -> ContractId -> MarloweVersion v -> ChainPoint -> ServerStIdle v IO ()
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

        serverWait :: ServerStWait v IO ()
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
