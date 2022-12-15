{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.ChainSync.Server
  where

import qualified Cardano.Api as Cardano
import Control.Concurrent.Component
import Control.Concurrent.STM (STM, atomically)
import Data.Functor (void, (<&>))
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoBlockHeader)
import Language.Marlowe.Runtime.ChainSync.Api
  (ChainPoint, Move, RuntimeChainSeekServer, ServerStPoll(..), WithGenesis(..), moveSchema)
import Language.Marlowe.Runtime.ChainSync.Database (MoveClient(..), MoveResult(..))
import Network.Protocol.ChainSeek.Server
  (ChainSeekServer(..), ServerStHandshake(..), ServerStIdle(..), ServerStInit(..), ServerStNext(..))
import Network.Protocol.Driver (RunServer(..))

type RunChainSeekServer m = RunServer m RuntimeChainSeekServer

data ChainSyncServerDependencies = ChainSyncServerDependencies
  { acceptRunChainSeekServer :: IO (RunChainSeekServer IO)
  , moveClient               :: !(MoveClient IO)
  , localTip                 :: !(STM Cardano.ChainTip)
  }

chainSyncServer :: Component IO ChainSyncServerDependencies ()
chainSyncServer = serverComponent worker \ChainSyncServerDependencies{..} -> do
  runChainSeekServer <- acceptRunChainSeekServer
  pure WorkerDependencies{..}

data WorkerDependencies = WorkerDependencies
  { runChainSeekServer :: !(RunChainSeekServer IO)
  , moveClient         :: !(MoveClient IO)
  , localTip           :: !(STM Cardano.ChainTip)
  }

worker :: Component IO WorkerDependencies ()
worker = component_ \WorkerDependencies{..} -> do
  let
    RunServer runServer = runChainSeekServer
    runWorker = void $ runServer server

    server = ChainSeekServer $ pure stInit

    stInit = ServerStInit \version -> pure if version == moveSchema
      then SendMsgHandshakeConfirmed $ stIdle Genesis
      else SendMsgHandshakeRejected [ moveSchema ] ()

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
          newTip <- atomically localTip <&> \case
            Cardano.ChainTipAtGenesis -> Genesis
            Cardano.ChainTip slotNo hash blockNo -> At $ fromCardanoBlockHeader $ Cardano.BlockHeader slotNo hash blockNo
          if tip /= newTip
            then stNext pos move
            else pure $ SendMsgWait $ pure $ stPoll move pos tip
      , recvMsgCancel = stIdle pos
      }

  runWorker
