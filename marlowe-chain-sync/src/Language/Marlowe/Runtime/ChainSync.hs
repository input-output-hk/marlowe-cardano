{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE RankNTypes            #-}

module Language.Marlowe.Runtime.ChainSync
  ( ChainSync (..)
  , ChainSyncDependencies (..)
  , mkChainSync
  , TestChainSync (..)
  , TestChainSyncDependencies (..)
  , mkTestChainSync
  ) where

import Cardano.Api (CardanoMode)
import qualified Cardano.Api as Cardano
import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.STM (STM, TVar, atomically, newEmptyTMVar, newTQueue, putTMVar, readTMVar, readTQueue,
                               readTVar, writeTQueue, writeTVar)
import Control.Monad (guard, unless)
import Data.Time (NominalDiffTime, secondsToNominalDiffTime)
import Language.Marlowe.Runtime.ChainSync.Api (RuntimeChainSeekClient, WithGenesis (Genesis), chainSeekClientPeer,
                                               chainSeekServerPeer)
import Language.Marlowe.Runtime.ChainSync.Database (CommitGenesisBlock (..), DatabaseQueries (..), GetGenesisBlock (..),
                                                    hoistDatabaseQueries)
import qualified Language.Marlowe.Runtime.ChainSync.Database.Memory as Memory
import Language.Marlowe.Runtime.ChainSync.Genesis (GenesisBlock)
import Language.Marlowe.Runtime.ChainSync.NodeClient (Changes, isEmptyChanges, toEmptyChanges)
import Language.Marlowe.Runtime.ChainSync.QueryServer (ChainSyncQueryServer (..), ChainSyncQueryServerDependencies (..),
                                                       RunQueryServer, mkChainSyncQueryServer)
import Language.Marlowe.Runtime.ChainSync.Server (ChainSyncServer (..), ChainSyncServerDependencies (..),
                                                  RunChainSeekServer (..), mkChainSyncServer)
import Language.Marlowe.Runtime.ChainSync.Store (ChainStore (..), ChainStoreDependencies (..), mkChainStore)
import Network.Protocol.Driver (runPeers)
import Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure)

data ChainSyncDependencies = ChainSyncDependencies
  { getChanges           :: !(STM Changes)
  , databaseQueries      :: !(DatabaseQueries IO)
  , persistRateLimit     :: !NominalDiffTime
  , genesisBlock         :: !GenesisBlock
  , acceptRunChainSeekServer :: IO (RunChainSeekServer IO)
  , acceptRunQueryServer :: IO (RunQueryServer IO)
  , queryLocalNodeState
      :: forall result
       . Maybe Cardano.ChainPoint
      -> Cardano.QueryInMode CardanoMode result
      -> IO (Either AcquireFailure result)
  }

newtype ChainSync = ChainSync { runChainSync :: IO () }

mkChainSync :: ChainSyncDependencies -> STM ChainSync
mkChainSync ChainSyncDependencies{..} = do
  let DatabaseQueries{..} = databaseQueries
  let rateLimit = persistRateLimit
  ChainStore{..} <- mkChainStore ChainStoreDependencies{..}
  ChainSyncServer{..} <- mkChainSyncServer ChainSyncServerDependencies{..}
  ChainSyncQueryServer{..} <- mkChainSyncQueryServer ChainSyncQueryServerDependencies{..}
  pure $ ChainSync do
    mDbGenesisBlock <- runGetGenesisBlock getGenesisBlock
    case mDbGenesisBlock of
      Just dbGenesisBlock -> unless (dbGenesisBlock == genesisBlock) do
        fail "Existing genesis block does not match computed genesis block"
      Nothing -> runCommitGenesisBlock commitGenesisBlock genesisBlock

    runChainStore
      `concurrently_` runChainSyncServer
      `concurrently_` runChainSyncQueryServer

data TestChainSyncDependencies = TestChainSyncDependencies
  { changesVar   :: !(TVar Changes)
  , genesisBlock :: !GenesisBlock
  }

data TestChainSync = TestChainSync
  { runChainSync       :: IO ()
  , connectToChainSeek :: forall a. RuntimeChainSeekClient IO a -> IO a
  }

mkTestChainSync :: TestChainSyncDependencies -> STM TestChainSync
mkTestChainSync TestChainSyncDependencies{..} = do
  chainSeekQueue <- newTQueue
  queryQueue <- newTQueue
  databaseQueries <- hoistDatabaseQueries atomically <$> Memory.databaseQueries genesisBlock
  let
    getChanges = do
      changes <- readTVar changesVar
      guard $ not $ isEmptyChanges changes
      writeTVar changesVar $ toEmptyChanges changes
      pure changes
  let persistRateLimit = secondsToNominalDiffTime 0
  let acceptRunChainSeekServer = atomically $ readTQueue chainSeekQueue
  let acceptRunQueryServer = atomically $ readTQueue queryQueue
  let queryLocalNodeState _ _ = fail "Query local node state not supported in test chain sync"
  ChainSync{..} <- mkChainSync ChainSyncDependencies{..}
  let
    connectToChainSeek :: forall a. RuntimeChainSeekClient IO a -> IO a
    connectToChainSeek client = do
      resultVar <- atomically do
        resultVar <- newEmptyTMVar
        writeTQueue chainSeekQueue $ RunChainSeekServer \server -> do
          (a, b) <- runPeers (chainSeekClientPeer Genesis client) (chainSeekServerPeer Genesis server)
          atomically $ putTMVar resultVar a
          pure b
        pure resultVar
      atomically $ readTMVar resultVar
  pure TestChainSync{..}
