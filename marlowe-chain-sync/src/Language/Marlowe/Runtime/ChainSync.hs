{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.ChainSync
  ( ChainSync (..)
  , ChainSyncDependencies (..)
  , mkChainSync
  ) where

import Cardano.Api (CardanoMode, LocalNodeClientProtocolsInMode)
import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.STM (STM)
import Control.Monad (unless)
import qualified Data.ByteString.Lazy as LBS
import Data.Time (NominalDiffTime)
import Language.Marlowe.Runtime.ChainSync.Database (CommitGenesisBlock (..), DatabaseQueries (..), GetGenesisBlock (..))
import Language.Marlowe.Runtime.ChainSync.Genesis (GenesisBlock)
import Language.Marlowe.Runtime.ChainSync.NodeClient (NodeClient (..), NodeClientDependencies (..), mkNodeClient)
import Language.Marlowe.Runtime.ChainSync.Server (ChainSyncServer (..), ChainSyncServerDependencies (..),
                                                  mkChainSyncServer)
import Language.Marlowe.Runtime.ChainSync.Store (ChainStore (..), ChainStoreDependencies (..), mkChainStore)
import Network.Channel (Channel)

data ChainSyncDependencies = ChainSyncDependencies
  { connectToLocalNode :: !(LocalNodeClientProtocolsInMode CardanoMode -> IO ())
  , databaseQueries    :: !(DatabaseQueries IO)
  , persistRateLimit   :: !NominalDiffTime
  , genesisBlock       :: !GenesisBlock
  , acceptChannel      :: IO (Channel IO LBS.ByteString, IO ())
  }

newtype ChainSync = ChainSync { runChainSync :: IO () }

mkChainSync :: ChainSyncDependencies -> STM ChainSync
mkChainSync ChainSyncDependencies{..} = do
  let DatabaseQueries{..} = databaseQueries
  NodeClient{..} <- mkNodeClient NodeClientDependencies{..}
  let rateLimit = persistRateLimit
  ChainStore{..} <- mkChainStore ChainStoreDependencies{..}
  ChainSyncServer{..} <- mkChainSyncServer ChainSyncServerDependencies{..}
  pure $ ChainSync do
    mDbGenesisBlock <- runGetGenesisBlock getGenesisBlock
    case mDbGenesisBlock of
      Just dbGenesisBlock -> unless (dbGenesisBlock == genesisBlock) do
        fail "Existing genesis block does not match computed genesis block"
      Nothing -> runCommitGenesisBlock commitGenesisBlock genesisBlock

    runNodeClient
      `concurrently_` runChainStore
      `concurrently_` runChainSyncServer
