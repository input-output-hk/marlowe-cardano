{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE RankNTypes            #-}

module Language.Marlowe.Runtime.ChainSync.Server where

import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Shelley as Cardano
import Control.Concurrent.Async (wait, waitEitherCatch, withAsync)
import Control.Concurrent.STM (STM, atomically)
import Control.Exception (throwIO)
import Control.Monad (guard)
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Short (toShort)
import Data.Functor (void)
import qualified Data.Text as T
import Data.Text.IO (hPutStrLn)
import Data.Void (Void, absurd)
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader (BlockHeader), BlockHeaderHash (unBlockHeaderHash),
                                               ChainPoint, Move, WithGenesis (..), runtimeChainSeekCodec,
                                               schemaVersion1_0)
import Language.Marlowe.Runtime.ChainSync.Database (MoveClient (..), MoveResult (..))
import Network.Channel (Channel (..))
import Network.Protocol.ChainSeek.Server (ChainSeekServer (..), ServerStHandshake (..), ServerStIdle (..),
                                          ServerStInit (..), ServerStNext (..), chainSeekServerPeer)
import Network.Protocol.Driver (mkDriver)
import Network.TypedProtocol (Driver (..), runPeerWithDriver)
import System.IO (stderr)

data ChainSyncServerDependencies = ChainSyncServerDependencies
  { acceptChannel :: IO (Channel IO LBS.ByteString, IO ())
  , moveClient    :: !(MoveClient IO)
  , localTip      :: !(STM Cardano.ChainTip)
  }

newtype ChainSyncServer = ChainSyncServer
  { runChainSyncServer :: IO Void
  }

mkChainSyncServer :: ChainSyncServerDependencies -> STM ChainSyncServer
mkChainSyncServer ChainSyncServerDependencies{..} = do
  let
    runChainSyncServer = do
      (channel, close) <- acceptChannel
      hPutStrLn stderr "New client connected"
      worker <- atomically $ mkWorker WorkerDependencies {..}
      withAsync (runWorker worker) \aworker ->
        withAsync runChainSyncServer \aserver -> do
          result <- waitEitherCatch aworker aserver
          close
          case result of
            Right (Left ex) -> throwIO ex
            Right (Right x) -> absurd x
            Left (Left ex)  -> do
              hPutStrLn stderr $ "Lost client with exception " <> T.pack (show ex)
            Left _  -> do
              hPutStrLn stderr "Client terminated normally"
          wait aserver
  pure $ ChainSyncServer { runChainSyncServer }

data WorkerDependencies = WorkerDependencies
  { channel    :: !(Channel IO LBS.ByteString)
  , moveClient :: !(MoveClient IO)
  , localTip   :: !(STM Cardano.ChainTip)
  }

newtype Worker = Worker
  { runWorker :: IO ()
  }

mkWorker :: WorkerDependencies -> STM Worker
mkWorker WorkerDependencies{..} = do
  let
    runWorker = void $ runPeerWithDriver driver peer (startDState driver)

    driver = mkDriver throwIO runtimeChainSeekCodec channel

    peer = chainSeekServerPeer Genesis server

    server = ChainSeekServer $ pure stInit

    stInit = ServerStInit \version -> pure if version == schemaVersion1_0
      then SendMsgHandshakeConfirmed $ stIdle Genesis
      else SendMsgHandshakeRejected [ schemaVersion1_0 ] ()

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

  pure $ Worker { runWorker }
