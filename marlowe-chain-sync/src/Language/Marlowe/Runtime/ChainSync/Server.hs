{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE EmptyCase  #-}
{-# LANGUAGE MultiWayIf #-}

module Language.Marlowe.Runtime.ChainSync.Server where

import Cardano.Api (ChainPoint (..), ChainTip)
import Control.Concurrent.Async (wait, waitEitherCatch, withAsync)
import Control.Concurrent.STM (STM, atomically)
import Control.Exception (throwIO)
import qualified Data.ByteString.Lazy as LBS
import Data.Functor (void)
import Data.Void (Void, absurd)
import Language.Marlowe.Runtime.ChainSync.Protocol (Query, runtimeFilteredChainSyncCodec, schemaVersion1_0)
import Network.Channel (Channel (..))
import Network.Protocol.Driver (mkDriver)
import Network.Protocol.FilteredChainSync.Server (FilteredChainSyncServer (..), ServerStHandshake (..),
                                                  ServerStIdle (..), ServerStInit (..), filteredChainSyncServerPeer)
import Network.TypedProtocol (Driver (..), runPeerWithDriver)

newtype ChainSyncServerDependencies = ChainSyncServerDependencies
  { getChannel :: IO (Channel IO LBS.ByteString)
  }

newtype ChainSyncServer = ChainSyncServer
  { runChainSyncServer :: IO Void
  }

mkChainSyncServer :: ChainSyncServerDependencies -> STM ChainSyncServer
mkChainSyncServer ChainSyncServerDependencies{..} = do
  let
    runChainSyncServer = do
      channel <- getChannel
      worker <- atomically $ mkWorker $ WorkerDependencies { channel }
      withAsync (runWorker worker) \aworker ->
        withAsync runChainSyncServer \aserver -> do
          result <- waitEitherCatch aworker aserver
          case result of
            Right (Left ex) -> throwIO ex
            Right (Right x) -> absurd x
            Left _          -> wait aserver
  pure $ ChainSyncServer { runChainSyncServer }

newtype WorkerDependencies = WorkerDependencies
  { channel :: Channel IO LBS.ByteString
  }

newtype Worker = Worker
  { runWorker :: IO ()
  }

mkWorker :: WorkerDependencies -> STM Worker
mkWorker WorkerDependencies{..} = do
  let
    runWorker = void $ runPeerWithDriver driver peer (startDState driver)

    driver = mkDriver throwIO runtimeFilteredChainSyncCodec channel

    peer = filteredChainSyncServerPeer ChainPointAtGenesis server

    server = FilteredChainSyncServer $ pure stInit

    stInit = ServerStInit \version -> pure if version == schemaVersion1_0
      then SendMsgHandshakeConfirmed stIdle
      else SendMsgHandshakeRejected [ schemaVersion1_0 ] ()

    stIdle :: IO (ServerStIdle Query ChainPoint ChainTip IO ())
    stIdle = pure ServerStIdle
      { recvMsgQueryNext = \case
      , recvMsgDone = pure ()
      }

  pure $ Worker { runWorker }
