{-# LANGUAGE RecordWildCards #-}

module Language.Marlowe.Runtime.Benchmark (
  BenchmarkConfig (..),
  measure,
) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Marlowe (MarloweT)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

import qualified Data.Aeson as A (encode)
import qualified Data.ByteString.Lazy.Char8 as LBS8 (putStrLn)
import qualified Language.Marlowe.Runtime.Benchmark.HeaderSync as HeaderSync (measure)
import qualified Language.Marlowe.Runtime.Benchmark.Query as Query (measure)
import qualified Language.Marlowe.Runtime.Benchmark.Sync as Sync (measure)

data BenchmarkConfig = BenchmarkConfig
  { headerSyncParallelism :: Int
  -- ^ Number of parallel clients for `HeaderSync` protocol.
  , maxContracts :: Int
  -- ^ Maximum number of contracts to be read by each `HeaderSync` client.
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

measure
  :: BenchmarkConfig
  -> MarloweT IO ()
measure BenchmarkConfig{..} =
  do
    (headerSyncResults, contractIds) <- HeaderSync.measure headerSyncParallelism maxContracts
    liftIO . forM_ headerSyncResults $ LBS8.putStrLn . A.encode
    syncResults <- Sync.measure syncParallelism syncBatchSize contractIds
    liftIO . forM_ syncResults $ LBS8.putStrLn . A.encode
    queryResults <-
      Query.measure queryParallelism queryBatchSize queryPageSize "No policy ID" $
        replicate (queryParallelism * queryBatchSize) mempty
    liftIO . forM_ queryResults $ LBS8.putStrLn . A.encode
