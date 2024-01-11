{-# LANGUAGE RecordWildCards #-}

-- | Benchmark for the `Query` protocol.
module Language.Marlowe.Runtime.Benchmark.Query (
  -- * Benchmarking
  Benchmark (..),
  measure,
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Marlowe (MarloweT)
import Data.Aeson (ToJSON)
import Data.Default (Default (..))
import Data.Foldable (foldlM)
import Data.List.Split (chunksOf)
import Data.Time.Clock (NominalDiffTime, UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Generics (Generic)
import Language.Marlowe.Protocol.Query.Client (MarloweQueryClient, getContractHeaders)
import Language.Marlowe.Protocol.Query.Types (ContractFilter, Order (Ascending), Page (..), Range (..))
import Language.Marlowe.Runtime.Client (runMarloweQueryClient)
import UnliftIO (forConcurrently)

data Benchmark = Benchmark
  { metric :: String
  , start :: UTCTime
  , finish :: UTCTime
  , query :: String
  , queriesPerSecond :: Double
  , pagesPerSecond :: Double
  , contractsPerSecond :: Double
  , seconds :: Double
  }
  deriving (Eq, Generic, Ord, Show, ToJSON)

data Statistics = Statistics
  { queries :: Int
  , pages :: Int
  , contracts :: Int
  , duration :: NominalDiffTime
  }
  deriving (Eq, Generic, Ord, Show, ToJSON)

instance Default Statistics where
  def = Statistics def def def 0

-- | Measure the performance of the protocol.
measure
  :: Int
  -- ^ Number of parallel clients for `Query` protocol.
  -> Int
  -- ^ Number of queries to be executed by each `Query` client.
  -> Int
  -- ^ Page size for each `Query` client.
  -> String
  -- ^ Label for the contract filter.
  -> [ContractFilter]
  -- ^ Contract filters to query.
  -> MarloweT IO [Benchmark]
  -- ^ Action to run the benchmark.
measure parallelism batchSize pageSize query filters =
  let batches = take parallelism $ chunksOf batchSize filters
   in forConcurrently batches $
        run "Query" pageSize query

-- | Run a benchmark client.
run
  :: String
  -- ^ Label for the benchmark.
  -> Int
  -- ^ Page size for each `Query` client.
  -> String
  -- ^ Label for the contract filter.
  -> [ContractFilter]
  -- ^ Contract filters to query.
  -> MarloweT IO Benchmark
  -- ^ Action to run the benchmark.
run metric pageSize query filters =
  do
    start <- liftIO getCurrentTime
    start' <- liftIO getPOSIXTime
    Statistics{..} <- foldlM ((runMarloweQueryClient .) . benchmark start' pageSize) def filters
    let seconds = realToFrac duration
        queriesPerSecond = realToFrac queries / seconds
        pagesPerSecond = realToFrac pages / seconds
        contractsPerSecond = realToFrac contracts / seconds
    finish <- liftIO getCurrentTime
    pure Benchmark{..}

-- | Run the benchmark.
benchmark
  :: (MonadIO m)
  => NominalDiffTime -- When the benchmark started.
  -> Int
  -- ^ Page size for each `Query` client.
  -> Statistics
  -- ^ The statistics so far.
  -> ContractFilter
  -- ^ The contract filter.
  -> MarloweQueryClient m Statistics
  -- ^ Action to run the benchmark.
benchmark start pageSize initial@Statistics{queries} cFilter =
  let accumulate = (. getContractHeaders cFilter) . (=<<) . handleNextPage
      handleNextPage stats Nothing = pure stats
      handleNextPage stats@Statistics{pages, contracts} (Just Page{..}) =
        do
          now <- liftIO getPOSIXTime
          let stats' =
                stats
                  { pages = pages + 1
                  , contracts = contracts + length items
                  , duration = now - start
                  }
          case nextRange of
            Nothing -> pure stats'
            Just range -> stats' `accumulate` range
   in initial{queries = queries + 1} `accumulate` Range Nothing 0 pageSize Ascending
