{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Language.Marlowe.Runtime.ChainSync.Server where

import Colog (Message, WithLog)
import Control.Concurrent.Component
import Data.Functor (void, (<&>))
import Language.Marlowe.Runtime.ChainSync.Api (ChainPoint, Move, RuntimeChainSeekServer, WithGenesis(..))
import Language.Marlowe.Runtime.ChainSync.Database (GetTip(..), MoveClient(..), MoveResult(..))
import Network.Protocol.ChainSeek.Server
import Network.Protocol.Connection (ConnectionSource, Connector, acceptConnector, runConnector)
import UnliftIO (MonadUnliftIO)

data ChainSyncServerDependencies m = ChainSyncServerDependencies
  { syncSource :: ConnectionSource RuntimeChainSeekServer m
  , moveClient :: MoveClient m
  , getTip :: GetTip m
  }

chainSyncServer
  :: (MonadUnliftIO m, WithLog env Message m)
  => Component m (ChainSyncServerDependencies m) ()
chainSyncServer = serverComponent "chain-seek-server" worker \ChainSyncServerDependencies{..} -> do
  connector <- acceptConnector syncSource
  pure WorkerDependencies{..}

data WorkerDependencies m = WorkerDependencies
  { connector :: Connector RuntimeChainSeekServer m
  , moveClient :: MoveClient m
  , getTip :: GetTip m
  }

worker
  :: forall env m. (MonadUnliftIO m, WithLog env Message m)
  => Component m (WorkerDependencies m) ()
worker = component_ "chain-seek-worker" \WorkerDependencies{..} -> do
  let
    runWorker = void $ runConnector connector $ ChainSeekServer $ pure $ stIdle Genesis

    stIdle :: ChainPoint -> ServerStIdle Move ChainPoint ChainPoint m ()
    stIdle pos = ServerStIdle
      { recvMsgQueryNext = stNext pos
      , recvMsgDone = pure ()
      }

    stNext :: ChainPoint -> Move err result -> m (ServerStNext Move err result ChainPoint ChainPoint m ())
    stNext pos move = runMoveClient moveClient pos move <&> \case
      RollForward result pos' tip -> SendMsgRollForward result (At pos') tip $ stIdle $ At pos'
      RollBack pos' tip           -> SendMsgRollBackward pos' tip $ stIdle pos'
      Reject err tip              -> SendMsgQueryRejected err tip $ stIdle pos
      Wait tip                    -> SendMsgWait $ stPoll move pos tip

    stPoll :: Move err result -> ChainPoint -> ChainPoint -> ServerStPoll Move err result ChainPoint ChainPoint m ()
    stPoll move pos tip = ServerStPoll
      { recvMsgPoll = do
          newTip <- runGetTip getTip
          if tip /= newTip
            then stNext pos move
            else pure $ SendMsgWait $ stPoll move pos tip
      , recvMsgCancel = pure $ stIdle pos
      }

  runWorker
