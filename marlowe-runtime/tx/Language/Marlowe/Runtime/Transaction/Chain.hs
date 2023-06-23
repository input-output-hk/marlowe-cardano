{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.Transaction.Chain where

import Colog (Message, WithLog)
import Control.Concurrent.Component
import Control.Concurrent.STM (STM, newTVar, readTVar, writeTVar)
import Data.Functor (($>))
import Data.Void (absurd)
import Language.Marlowe.Runtime.ChainSync.Api (Move (..), RuntimeChainSeekClient)
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Network.Protocol.ChainSeek.Client
import Network.Protocol.Connection (Connector, runConnector)
import UnliftIO (MonadUnliftIO, atomically, finally)
import UnliftIO.Concurrent (threadDelay)

newtype TransactionChainClientDependencies m = TransactionChainClientDependencies
  { chainSyncConnector :: Connector RuntimeChainSeekClient m
  }

transactionChainClient
  :: (MonadUnliftIO m, WithLog env Message m)
  => Component m (TransactionChainClientDependencies m) (STM Bool, STM Chain.ChainPoint)
transactionChainClient = component "tx-chain-seek-client" \TransactionChainClientDependencies{..} -> do
  tipVar <- newTVar Chain.Genesis
  connectedVar <- newTVar False
  pure
    ( flip finally (atomically $ writeTVar connectedVar False) $
        runConnector chainSyncConnector $
          client connectedVar tipVar
    , (readTVar connectedVar, readTVar tipVar)
    )
  where
    client connectedVar tipVar = ChainSeekClient do
      atomically $ writeTVar connectedVar True
      pure clientIdle
      where
        clientIdle = SendMsgQueryNext AdvanceToTip clientNext
        clientNext =
          ClientStNext
            { recvMsgRollForward = \_ _ tip -> atomically do
                writeTVar tipVar tip
                pure clientIdle
            , recvMsgRollBackward = \_ tip -> atomically do
                writeTVar tipVar tip
                pure clientIdle
            , recvMsgQueryRejected = absurd
            , recvMsgWait = threadDelay 1_000_000 $> SendMsgPoll clientNext
            }
