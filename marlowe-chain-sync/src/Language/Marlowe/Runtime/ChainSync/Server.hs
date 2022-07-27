{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase             #-}

module Language.Marlowe.Runtime.ChainSync.Server where

import Cardano.Api (ChainPoint (..), ChainTip)
import Control.Concurrent.Async (wait, waitEitherCatch, withAsync)
import Control.Concurrent.STM (STM, atomically)
import Control.Exception (throwIO)
import Control.Monad (guard)
import qualified Data.ByteString.Lazy as LBS
import Data.Functor (void)
import Data.Void (Void, absurd)
import Language.Marlowe.Runtime.ChainSync.Database (GetQueryResult (..))
import Language.Marlowe.Runtime.ChainSync.Protocol (Query, QueryResult (..), runtimeFilteredChainSyncCodec,
                                                    schemaVersion1_0)
import Network.Channel (Channel (..))
import Network.Protocol.Driver (mkDriver)
import Network.Protocol.FilteredChainSync.Server (FilteredChainSyncServer (..), ServerStHandshake (..),
                                                  ServerStIdle (..), ServerStInit (..), ServerStNext (..),
                                                  filteredChainSyncServerPeer)
import Network.TypedProtocol (Driver (..), runPeerWithDriver)

data ChainSyncServerDependencies = ChainSyncServerDependencies
  { getChannel     :: !(IO (Channel IO LBS.ByteString))
  , getQueryResult :: !(GetQueryResult IO)
  , localTip       :: !(STM ChainTip)
  }

newtype ChainSyncServer = ChainSyncServer
  { runChainSyncServer :: IO Void
  }

mkChainSyncServer :: ChainSyncServerDependencies -> STM ChainSyncServer
mkChainSyncServer ChainSyncServerDependencies{..} = do
  let
    runChainSyncServer = do
      channel <- getChannel
      worker <- atomically $ mkWorker WorkerDependencies {..}
      withAsync (runWorker worker) \aworker ->
        withAsync runChainSyncServer \aserver -> do
          result <- waitEitherCatch aworker aserver
          case result of
            Right (Left ex) -> throwIO ex
            Right (Right x) -> absurd x
            Left _          -> wait aserver
  pure $ ChainSyncServer { runChainSyncServer }

data WorkerDependencies = WorkerDependencies
  { channel        :: !(Channel IO LBS.ByteString)
  , getQueryResult :: !(GetQueryResult IO)
  , localTip       :: !(STM ChainTip)
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
      then SendMsgHandshakeConfirmed $ stIdle ChainPointAtGenesis
      else SendMsgHandshakeRejected [ schemaVersion1_0 ] ()

    stIdle :: ChainPoint -> IO (ServerStIdle Query ChainPoint ChainTip IO ())
    stIdle pos = pure ServerStIdle
      { recvMsgQueryNext = \query -> do
          let
            goWait lastTip = do
              atomically $ awaitTipChange lastTip
              pollQuery pure goWait

            awaitTipChange lastTip = do
              newTip <- localTip
              guard $ lastTip /= newTip

            pollQuery onReply onWait = do
              qResult <- runGetQueryResult getQueryResult pos query
              case qResult of
                RollForward result pos' tip -> onReply $ SendMsgRollForward result pos' tip $ stIdle pos'
                RollBack pos' tip           -> onReply $ SendMsgRollBackward pos' tip $ stIdle pos'
                Reject err tip              -> onReply $ SendMsgQueryRejected err tip $ stIdle pos
                Wait tip                    -> onWait tip

          pollQuery (pure . Left) (pure . Right . goWait)
      , recvMsgDone = pure ()
      }

  pure $ Worker { runWorker }
