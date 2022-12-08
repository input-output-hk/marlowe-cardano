{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.ChainSync.Server
  where

import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Shelley as Cardano
import Control.Concurrent.Component
import Control.Concurrent.STM (STM, atomically)
import Control.Monad (guard)
import Data.ByteString.Short (toShort)
import Data.Functor (void)
import Language.Marlowe.Runtime.ChainSync.Api
  ( BlockHeader(BlockHeader)
  , BlockHeaderHash(unBlockHeaderHash)
  , ChainPoint
  , Move
  , RuntimeChainSeekServer
  , WithGenesis(..)
  , moveSchema
  )
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
chainSyncServer = serverComponent worker mempty mempty \ChainSyncServerDependencies{..} -> do
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
      { recvMsgQueryNext = \query -> do
          let
            goWait lastTip = do
              atomically $ awaitTipChange case lastTip of
                Genesis -> Cardano.ChainTipAtGenesis
                At (BlockHeader slotNo hash blockNo) -> Cardano.ChainTip
                  (Cardano.SlotNo $ fromIntegral slotNo)
                  (Cardano.HeaderHash $ toShort $ unBlockHeaderHash hash)
                  (Cardano.BlockNo $ fromIntegral blockNo)
              pure $ SendMsgPing $ pollQuery goWait

            awaitTipChange lastTip = do
              newTip <- localTip
              guard $ lastTip /= newTip

            pollQuery onWait = do
              qResult <- runMoveClient moveClient pos query
              case qResult of
                RollForward result pos' tip -> pure $ SendMsgRollForward result (At pos') tip $ stIdle $ At pos'
                RollBack pos' tip           -> pure $ SendMsgRollBackward pos' tip $ stIdle pos'
                Reject err tip              -> pure $ SendMsgQueryRejected err tip $ stIdle pos
                Wait tip                    -> onWait tip

          pollQuery $ pure . SendMsgWait . goWait
      , recvMsgDone = pure ()
      }

  runWorker
