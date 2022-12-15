{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.Transaction.Chain
  where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Component
import Control.Concurrent.STM (STM, atomically, newTVar, readTVar, writeTVar)
import Data.Functor (($>))
import Data.Void (absurd)
import Language.Marlowe.Runtime.ChainSync.Api (Move(..), RuntimeChainSeekClient, moveSchema)
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Network.Protocol.ChainSeek.Client
import Network.Protocol.Driver (RunClient)

newtype TransactionChainClientDependencies = TransactionChainClientDependencies
  { connectToChainSeek :: RunClient IO RuntimeChainSeekClient
  }

transactionChainClient :: Component IO TransactionChainClientDependencies (STM Chain.ChainPoint)
transactionChainClient = component \TransactionChainClientDependencies{..} -> do
  tipVar <- newTVar Chain.Genesis
  pure (connectToChainSeek $ client tipVar, readTVar tipVar)
  where
  client tipVar = ChainSeekClient $ pure $ SendMsgRequestHandshake moveSchema ClientStHandshake
    { recvMsgHandshakeRejected = \versions -> error $ "Schema not supported, requires " <> show versions
    , recvMsgHandshakeConfirmed = pure clientIdle
    }
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
