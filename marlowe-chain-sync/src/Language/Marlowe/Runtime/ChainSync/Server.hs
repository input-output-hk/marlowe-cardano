{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.ChainSync.Server
  where

import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Shelley as Cardano
import Colog (logDebug, logError)
import Control.Concurrent.Async (wait, waitEitherCatch)
import Control.Concurrent.STM (STM, atomically)
import Control.Exception (throwIO)
import Control.Monad (guard)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Short (toShort)
import Data.Functor (void)
import qualified Data.Text as T
import Data.Void (Void, absurd)
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
import Language.Marlowe.Runtime.Logging.Colog.LogIO (LogIO, withAsyncLogIO)
import Network.Protocol.ChainSeek.Server
  (ChainSeekServer(..), ServerStHandshake(..), ServerStIdle(..), ServerStInit(..), ServerStNext(..))

newtype RunChainSeekServer m = RunChainSeekServer (forall a. RuntimeChainSeekServer m a -> LogIO a)

data ChainSyncServerDependencies = ChainSyncServerDependencies
  { acceptRunChainSeekServer :: LogIO (RunChainSeekServer LogIO)
  , moveClient               :: !(MoveClient LogIO)
  , localTip                 :: !(STM Cardano.ChainTip)
  }

newtype ChainSyncServer = ChainSyncServer
  { runChainSyncServer :: LogIO Void
  }

mkChainSyncServer :: ChainSyncServerDependencies -> STM ChainSyncServer
mkChainSyncServer ChainSyncServerDependencies{..} = do
  let
    runChainSyncServer = do
      runChainSeekServer <- acceptRunChainSeekServer
      logDebug "New client connected"
      worker <- liftIO $ atomically $ mkWorker WorkerDependencies {..}
      withAsyncLogIO (runWorker worker) \aworker ->
        withAsyncLogIO runChainSyncServer \aserver -> do
          result <- liftIO $ waitEitherCatch aworker aserver
          case result of
            Right (Left ex) -> liftIO $ throwIO ex
            Right (Right x) -> absurd x
            Left (Left ex)  -> do
              logError . T.pack . mappend "Lost client with exception " . show $ ex
            Left _  -> do
              logDebug "Client terminated normally"
          liftIO $ wait aserver
  pure $ ChainSyncServer { runChainSyncServer }

data WorkerDependencies = WorkerDependencies
  { runChainSeekServer :: !(RunChainSeekServer LogIO)
  , moveClient         :: !(MoveClient LogIO)
  , localTip           :: !(STM Cardano.ChainTip)
  }

newtype Worker = Worker
  { runWorker :: LogIO ()
  }

mkWorker :: WorkerDependencies -> STM Worker
mkWorker WorkerDependencies{..} = do
  let
    RunChainSeekServer runServer = runChainSeekServer
    runWorker = void $ runServer server

    server = ChainSeekServer $ pure stInit

    stInit = ServerStInit \version -> pure if version == moveSchema
      then SendMsgHandshakeConfirmed $ stIdle Genesis
      else SendMsgHandshakeRejected moveSchema ()

    stIdle :: ChainPoint -> LogIO (ServerStIdle Move ChainPoint ChainPoint LogIO ())
    stIdle pos = pure ServerStIdle
      { recvMsgQueryNext = \query -> do
          let
            goWait lastTip = do
              liftIO $ atomically $ awaitTipChange case lastTip of
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

  pure $ Worker { runWorker }
