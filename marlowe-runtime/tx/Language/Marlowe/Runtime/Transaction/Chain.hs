{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.Transaction.Chain
  where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Component
import Control.Concurrent.STM (STM, atomically, newTVar, readTVar, writeTVar)
import Data.Functor (($>))
import Data.Void (absurd)
import Language.Marlowe.Runtime.ChainSync.Api (Move(..), RuntimeChainSeekClient)
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Network.Protocol.ChainSeek.Client
import Network.Protocol.Connection (SomeClientConnector)
import Network.Protocol.Driver (runSomeConnector)

newtype TransactionChainClientDependencies = TransactionChainClientDependencies
  { chainSyncConnector :: SomeClientConnector RuntimeChainSeekClient IO
  }

transactionChainClient :: Component IO TransactionChainClientDependencies (STM Chain.ChainPoint)
transactionChainClient = component \TransactionChainClientDependencies{..} -> do
  tipVar <- newTVar Chain.Genesis
  pure (runSomeConnector chainSyncConnector $ client tipVar, readTVar tipVar)
  where
  client tipVar = ChainSeekClient $ pure clientIdle
    where
    clientIdle = SendMsgQueryNext AdvanceToTip clientNext
    clientNext = ClientStNext
      { recvMsgRollForward = \_ _ tip -> atomically do
          writeTVar tipVar tip
          pure clientIdle
      , recvMsgRollBackward = \_ tip -> atomically do
          writeTVar tipVar tip
          pure clientIdle
      , recvMsgQueryRejected = absurd
      , recvMsgWait = threadDelay 1_000_000 $> SendMsgPoll clientNext
      }
