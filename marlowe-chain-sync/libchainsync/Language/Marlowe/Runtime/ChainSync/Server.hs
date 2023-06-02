{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Language.Marlowe.Runtime.ChainSync.Server where

import Control.Concurrent.Component
import Control.Monad.Event.Class (MonadEvent)
import Data.Functor (void, (<&>))
import Language.Marlowe.Runtime.ChainSync.Api (ChainPoint, Move, RuntimeChainSeekServer, WithGenesis(..))
import Language.Marlowe.Runtime.ChainSync.Database (GetTip(..), MoveClient(..), MoveResult(..))
import Network.Protocol.ChainSeek.Server
import Network.Protocol.Connection (SomeConnectionSourceTraced, SomeServerConnectorTraced, acceptSomeConnectorTraced)
import Network.Protocol.Driver.Trace (HasSpanContext, runSomeConnectorTraced)
import UnliftIO (MonadUnliftIO)

data ChainSyncServerDependencies r s m = ChainSyncServerDependencies
  { syncSource :: SomeConnectionSourceTraced RuntimeChainSeekServer r s m
  , moveClient :: MoveClient m
  , getTip :: GetTip m
  }

chainSyncServer
  :: (MonadUnliftIO m, MonadEvent r s m, HasSpanContext r)
  => Component m (ChainSyncServerDependencies r s m) ()
chainSyncServer = serverComponent worker \ChainSyncServerDependencies{..} -> do
  connector <- acceptSomeConnectorTraced syncSource
  pure WorkerDependencies{..}

data WorkerDependencies r s m = WorkerDependencies
  { connector :: SomeServerConnectorTraced RuntimeChainSeekServer r s m
  , moveClient :: MoveClient m
  , getTip :: GetTip m
  }

worker
  :: forall r s m. (MonadUnliftIO m, MonadEvent r s m, HasSpanContext r)
  => Component m (WorkerDependencies r s m) ()
worker = component_ \WorkerDependencies{..} -> do
  let
    runWorker = void $ runSomeConnectorTraced connector $ ChainSeekServer $ pure $ stIdle Genesis

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
