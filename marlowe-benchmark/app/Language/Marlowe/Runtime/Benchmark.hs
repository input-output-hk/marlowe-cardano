{-# LANGUAGE RecordWildCards #-}

-- | Benchmark for protocols.
module Language.Marlowe.Runtime.Benchmark (
  -- * Benchmarking
  BenchmarkConfig (..),
  measure,
) where

import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Marlowe (MarloweT)
import Data.Aeson (FromJSON, ToJSON)
import Data.Default (Default (..))
import Data.Word (Word8)
import GHC.Generics (Generic)

import qualified Data.Aeson as A (encode)
import qualified Data.ByteString.Lazy.Char8 as LBS8 (putStrLn)
import qualified Language.Marlowe.Runtime.Benchmark.BulkSync as Bulk (measure)
import qualified Language.Marlowe.Runtime.Benchmark.HeaderSync as HeaderSync (measure)
import qualified Language.Marlowe.Runtime.Benchmark.Query as Query (measure)
import qualified Language.Marlowe.Runtime.Benchmark.Sync as Sync (measure)

-- | Benchmark configuration.
data BenchmarkConfig = BenchmarkConfig
  { headerSyncParallelism :: Int
  -- ^ Number of parallel clients for `HeaderSync` protocol.
  , headerMaxContracts :: Int
  -- ^ Maximum number of contracts to be read by each `HeaderSync` client.
  , bulkParallelism :: Int
  -- ^ Number of parallel clients for the `BulkSync` protocol.
  , bulkPageSize :: Word8
  -- ^ Number of blocks to fetch at a time for the `BulkSync` clients.
  , bulkMaxBlocks :: Int
  -- ^ Maximum number of blocks to fetch for each `BulkSync` client.
  , syncParallelism :: Int
  -- ^ Number of parallel clients for `Sync` protocol.
  , syncBatchSize :: Int
  -- ^ Number of contracts to be read by each `Sync` client.
  , queryParallelism :: Int
  -- ^ Number of parallel clients for `Query` protocol.
  , queryBatchSize :: Int
  -- ^ Number of queries to be executed by each `Query` client.
  , queryPageSize :: Int
  -- ^ Page size for each `Query` client.
  }
  deriving (Eq, FromJSON, Generic, Ord, Show, ToJSON)

instance Default BenchmarkConfig where
  def =
    BenchmarkConfig
      { headerSyncParallelism = 4
      , headerMaxContracts = maxBound
      , bulkParallelism = 4
      , bulkPageSize = 128
      , bulkMaxBlocks = maxBound
      , syncParallelism = 4
      , syncBatchSize = 512
      , queryParallelism = 4
      , queryBatchSize = 16
      , queryPageSize = 256
      }

-- | Run the benchmarks.
measure
  :: BenchmarkConfig
  -> MarloweT IO ()
measure BenchmarkConfig{..} =
  do
    when (headerSyncParallelism == 0) $
      error "At least one `HeaderSync` client must be run."
    (headerSyncResults, contractIds) <- HeaderSync.measure headerSyncParallelism headerMaxContracts
    liftIO . forM_ headerSyncResults $ LBS8.putStrLn . A.encode
    when (bulkParallelism > 0) $
      do
        bulkResults <- Bulk.measure bulkParallelism bulkPageSize bulkMaxBlocks
        liftIO . forM_ bulkResults $ LBS8.putStrLn . A.encode
    when (syncParallelism > 0) $
      do
        syncResults <- Sync.measure syncParallelism syncBatchSize contractIds
        liftIO . forM_ syncResults $ LBS8.putStrLn . A.encode
    when (queryParallelism > 0) $
      do
        queryResults <-
          Query.measure queryParallelism queryBatchSize queryPageSize "No policy ID" $
            replicate (queryParallelism * queryBatchSize) mempty
        liftIO . forM_ queryResults $ LBS8.putStrLn . A.encode
