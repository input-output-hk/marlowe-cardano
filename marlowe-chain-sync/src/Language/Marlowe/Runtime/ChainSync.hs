module Language.Marlowe.Runtime.ChainSync
  ( ChainSync (..)
  , ChainSyncDependencies (..)
  , mkChainSync
  ) where

import Cardano.Api (CardanoMode, LocalNodeConnectInfo)
import qualified Cardano.Chain.Genesis as Byron
import Control.Concurrent.Async (waitBoth, withAsyncBound)
import Control.Concurrent.STM (STM)
import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.Functor (void)
import Data.Time (NominalDiffTime)
import Language.Marlowe.Runtime.ChainSync.Database (CommitGenesisBlock (..), DatabaseQueries (..), GetGenesisBlock (..))
import Language.Marlowe.Runtime.ChainSync.Genesis (computeByronGenesisBlock)
import Language.Marlowe.Runtime.ChainSync.NodeClient (NodeClient (..), NodeClientDependencies (..), mkNodeClient)
import Language.Marlowe.Runtime.ChainSync.Store (ChainStore (..), ChainStoreDependencies (..), mkChainStore)

data ChainSyncDependencies = ChainSyncDependencies
  { localNodeConnectInfo :: !(LocalNodeConnectInfo CardanoMode)
  , databaseQueries      :: !(DatabaseQueries IO)
  , persistRateLimit     :: !NominalDiffTime
  , genesisConfigHash    :: !ByteString
  , genesisConfig        :: !Byron.Config
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
    }
  pure $ ChainSync do
    let genesisBlock = computeByronGenesisBlock genesisConfigHash genesisConfig
    mDbGenesisBlock <- runGetGenesisBlock getGenesisBlock
    case mDbGenesisBlock of
      Just dbGenesisBlock -> unless (dbGenesisBlock == genesisBlock) do
        fail "Existing genesis block does not match computed genesis block"
      Nothing -> runCommitGenesisBlock commitGenesisBlock genesisBlock

    void $ withAsyncBound runNodeClient \a1 ->
      withAsyncBound runChainStore \a2 ->
        waitBoth a1 a2
