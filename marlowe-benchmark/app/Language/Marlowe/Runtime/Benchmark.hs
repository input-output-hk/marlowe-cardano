{-# LANGUAGE RecordWildCards #-}

-- | Benchmark for protocols.
module Language.Marlowe.Runtime.Benchmark (
  -- * Benchmarking
  BenchmarkConfig (..),
  measure,
) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Marlowe (MarloweT)
import Data.Aeson (FromJSON, ToJSON)
import Data.Default (Default (..))
import Data.Maybe (isJust)
import Data.Time.Clock (getCurrentTime)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Language.Marlowe.Runtime.ChainSync.Api (Address)
import System.Directory (doesFileExist, removeFile)
import System.IO (hPutStrLn, stderr)

import qualified Cardano.Api as C
import qualified Data.Aeson as A (encode)
import qualified Data.ByteString.Lazy.Char8 as LBS8 (appendFile, putStrLn, unlines)
import qualified Language.Marlowe.Runtime.Benchmark.BulkSync as Bulk (measure)
import qualified Language.Marlowe.Runtime.Benchmark.HeaderSync as HeaderSync (measure)
import qualified Language.Marlowe.Runtime.Benchmark.Lifecycle as Lifecycle (measure)
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
  , lifecycleParallelism :: Int
  -- ^ Number of parallel clients for basic transaction lifecycle.
  , lifecycleContracts :: Int
  -- ^ Number of contracts to be run by each basic transaction lifecycle client.
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
      , lifecycleParallelism = 1
      , lifecycleContracts = 1
      }

-- | Run the benchmarks.
measure
  :: (C.IsShelleyBasedEra era)
  => BenchmarkConfig
  -> Maybe (C.SocketPath, C.CardanoEra era, C.NetworkId, Address, C.SigningKey C.PaymentExtendedKey)
  -> Maybe FilePath
  -> MarloweT IO ()
measure BenchmarkConfig{..} faucet out =
  do
    liftIO $
      maybe
        (pure ())
        (\out' -> do exists <- doesFileExist out'; when exists $ removeFile out')
        out
    let report x =
          liftIO $ case out of
            Nothing -> mapM_ (LBS8.putStrLn . A.encode) x
            Just out' -> LBS8.appendFile out' $ LBS8.unlines $ A.encode <$> x
    when (headerSyncParallelism > 0) $
      do
        liftIO $ hPutStrLn stderr . ("HeaderSync: " <>) . show =<< getCurrentTime
        (headerSyncResults, contractIds) <- HeaderSync.measure headerSyncParallelism headerMaxContracts
        report headerSyncResults
        when (bulkParallelism > 0) $
          do
            liftIO $ hPutStrLn stderr . ("BulkSync: " <>) . show =<< getCurrentTime
            bulkResults <- Bulk.measure bulkParallelism bulkPageSize bulkMaxBlocks
            report bulkResults
        when (syncParallelism > 0) $
          do
            liftIO $ hPutStrLn stderr . ("Sync: " <>) . show =<< getCurrentTime
            syncResults <- Sync.measure syncParallelism syncBatchSize contractIds
            report syncResults
        when (queryParallelism > 0) $
          do
            liftIO $ hPutStrLn stderr . ("Query: " <>) . show =<< getCurrentTime
            queryResults <-
              Query.measure queryParallelism queryBatchSize queryPageSize "No policy ID" $
                replicate (queryParallelism * queryBatchSize) mempty
            report queryResults
    when (isJust faucet && lifecycleParallelism > 0) $
      do
        Just (node, era, network, faucetAddress, faucetKey) <- pure faucet
        liftIO $ hPutStrLn stderr . ("Lifecycle: " <>) . show =<< getCurrentTime
        lifecycleResults <- Lifecycle.measure node era network lifecycleParallelism faucetAddress faucetKey lifecycleContracts
        report lifecycleResults
    liftIO $ hPutStrLn stderr . ("Done: " <>) . show =<< getCurrentTime
