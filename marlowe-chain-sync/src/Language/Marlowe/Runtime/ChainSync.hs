module Language.Marlowe.Runtime.ChainSync
  ( ChainSync (..)
  , ChainSyncDependencies (..)
  , mkChainSync
  ) where

import Cardano.Api (CardanoMode, LocalNodeConnectInfo)
import Control.Concurrent.Async (Concurrently (..))
import Control.Concurrent.STM (STM)
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.Time (NominalDiffTime)
import Language.Marlowe.Runtime.ChainSync.Database (DatabaseQueries (..))
import Language.Marlowe.Runtime.ChainSync.NodeClient (NodeClient (..), NodeClientDependencies (..), mkNodeClient)
import Language.Marlowe.Runtime.ChainSync.Store (ChainStore (..), ChainStoreDependencies (..), mkChainStore)

data ChainSyncDependencies = ChainSyncDependencies
  { localNodeConnectInfo :: !(LocalNodeConnectInfo CardanoMode)
  , databaseQueries      :: !(DatabaseQueries IO)
  , persistRateLimit     :: !NominalDiffTime
  }

newtype ChainSync = ChainSync { runChainSync :: IO () }

mkChainSync :: ChainSyncDependencies -> STM ChainSync
mkChainSync ChainSyncDependencies{..} = do
  let DatabaseQueries{..} = databaseQueries
  NodeClient{..} <- mkNodeClient NodeClientDependencies
    { localNodeConnectInfo
    , getHeaderAtPoint
    , getIntersectionPoints
    }
  ChainStore{..} <- mkChainStore ChainStoreDependencies
    { commitRollback
    , commitBlocks
    , rateLimit = persistRateLimit
    , getChanges
    , clearChanges
    }
  pure $ ChainSync $ void $ runConcurrently $ traverse_ Concurrently
    [ runNodeClient
    , runChainStore
    ]
