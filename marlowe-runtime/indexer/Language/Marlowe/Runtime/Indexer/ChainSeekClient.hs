{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Language.Marlowe.Runtime.Indexer.ChainSeekClient
  where

import Control.Concurrent.Component
import Control.Concurrent.STM (STM, newTQueue, readTQueue)
import Language.Marlowe.Runtime.ChainSync.Api (ChainPoint, RuntimeChainSeekClient)
import Language.Marlowe.Runtime.Indexer.Database (DatabaseQueries)
import Language.Marlowe.Runtime.Indexer.Types (MarloweBlock)
import Network.Protocol.Driver (RunClient)

data ChainSeekClientDependencies = ChainSeekClientDependencies
  { databaseQueries :: DatabaseQueries IO
  , runChainSeekClient :: RunClient IO RuntimeChainSeekClient
  }

data ChainEvent
  = RollForward MarloweBlock
  | RollBackward ChainPoint

chainSeekClient :: Component IO ChainSeekClientDependencies (STM ChainEvent)
chainSeekClient = component \_ -> do
  eventQueue <- newTQueue
  pure (pure (), readTQueue eventQueue)
