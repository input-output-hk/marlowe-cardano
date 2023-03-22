{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Language.Marlowe.Runtime.ChainSync.Server
  where

import Control.Concurrent.Component
import Data.Functor (void, (<&>))
import Language.Marlowe.Runtime.ChainSync.Api (ChainPoint, Move, RuntimeChainSeekServer, WithGenesis(..))
import Language.Marlowe.Runtime.ChainSync.Database (GetTip(..), MoveClient(..), MoveResult(..))
import Network.Protocol.ChainSeek.Server
import Network.Protocol.Connection (SomeConnectionSource, SomeServerConnector, acceptSomeConnector)
import Network.Protocol.Driver (runSomeConnector)

data ChainSyncServerDependencies = ChainSyncServerDependencies
  { syncSource :: SomeConnectionSource RuntimeChainSeekServer IO
  , moveClient :: MoveClient IO
  , getTip :: GetTip IO
  }

chainSyncServer :: Component IO ChainSyncServerDependencies ()
chainSyncServer = serverComponent worker \ChainSyncServerDependencies{..} -> do
  connector <- acceptSomeConnector syncSource
  pure WorkerDependencies{..}

data WorkerDependencies = WorkerDependencies
  { connector :: SomeServerConnector RuntimeChainSeekServer IO
  , moveClient :: MoveClient IO
  , getTip :: GetTip IO
  }

worker :: Component IO WorkerDependencies ()
worker = component_ \WorkerDependencies{..} -> do
  let
    runWorker = void $ runSomeConnector connector $ ChainSeekServer $ stIdle Genesis

    stIdle :: ChainPoint -> IO (ServerStIdle Move ChainPoint ChainPoint IO ())
    stIdle pos = pure ServerStIdle
      { recvMsgQueryNext = stNext pos
      , recvMsgDone = pure ()
      }

    stNext :: ChainPoint -> Move err result -> IO (ServerStNext Move err result ChainPoint ChainPoint IO ())
    stNext pos move = runMoveClient moveClient pos move <&> \case
      RollForward result pos' tip -> SendMsgRollForward result (At pos') tip $ stIdle $ At pos'
      RollBack pos' tip           -> SendMsgRollBackward pos' tip $ stIdle pos'
      Reject err tip              -> SendMsgQueryRejected err tip $ stIdle pos
      Wait tip                    -> SendMsgWait $ pure $ stPoll move pos tip

    stPoll :: Move err result -> ChainPoint -> ChainPoint -> ServerStPoll Move err result ChainPoint ChainPoint IO ()
    stPoll move pos tip = ServerStPoll
      { recvMsgPoll = do
          newTip <- runGetTip getTip
          if tip /= newTip
            then stNext pos move
            else pure $ SendMsgWait $ pure $ stPoll move pos tip
      , recvMsgCancel = stIdle pos
      }

  runWorker
