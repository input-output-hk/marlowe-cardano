{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE RankNTypes         #-}

module Language.Marlowe.Runtime.ChainSyncTest where

import Control.Concurrent.Async (race_)
import Control.Concurrent.STM (TVar, atomically, newTVar)
import Control.Monad (join)
import Language.Marlowe.Runtime.ChainSync
import Language.Marlowe.Runtime.ChainSync.Api (RuntimeChainSeekClient)
import Language.Marlowe.Runtime.ChainSync.Genesis (GenesisBlock)
import Language.Marlowe.Runtime.ChainSync.NodeClient (Changes, emptyChanges)
import Test.Hspec (Expectation)

withTestChainSync :: GenesisBlock -> ((forall a. RuntimeChainSeekClient IO a -> IO a) -> TVar Changes -> Expectation) -> Expectation
withTestChainSync genesisBlock test = join $ atomically do
  changesVar <- newTVar emptyChanges
  TestChainSync{..} <- mkTestChainSync TestChainSyncDependencies{..}
  pure $ race_ runChainSync $ test connectToChainSeek changesVar
