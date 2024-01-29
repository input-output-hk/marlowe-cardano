{-# LANGUAGE RecordWildCards #-}

-- | Benchmark for the `Query` protocol.
module Language.Marlowe.Runtime.Benchmark.Query (
  -- * Benchmarking
  Benchmark (..),
  measure,

  -- * Queries
  Query (..),
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Marlowe (MarloweT)
import Data.Aeson (FromJSON, ToJSON)
import Data.Default (Default (..))
import Data.Foldable (foldlM)
import Data.List.Split (chunksOf)
import Data.Time.Clock (NominalDiffTime, UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Generics (Generic)
import Language.Marlowe.Protocol.Query.Client (
  MarloweQueryClient,
  getContractHeaders,
  getContractState,
  getPayout,
  getPayouts,
  getTransaction,
  getTransactions,
  getWithdrawal,
  getWithdrawals,
 )
import Language.Marlowe.Protocol.Query.Types (
  ContractFilter,
  Order (Ascending),
  Page (..),
  PayoutFilter,
  Range (..),
  WithdrawalFilter,
 )
import Language.Marlowe.Runtime.ChainSync.Api (TxId, TxOutRef)
import Language.Marlowe.Runtime.Client (runMarloweQueryClient)
import Language.Marlowe.Runtime.Core.Api (ContractId)
import UnliftIO (forConcurrently)

data Query
  = QueryHeaders ContractFilter
  | QueryState ContractId
  | QueryTransaction TxId
  | QueryTransactions ContractId
  | QueryWithdrawal TxId
  | QueryWithdrawals WithdrawalFilter
  | QueryPayouts PayoutFilter
  | QueryPayout TxOutRef
  deriving (Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)

data Benchmark = Benchmark
  { metric :: String
  , start :: UTCTime
  , finish :: UTCTime
  , query :: String
  , queriesPerSecond :: Double
  , pagesPerSecond :: Double
  , resultsPerSecond :: Double
  , seconds :: Double
  }
  deriving (Eq, Generic, Ord, Show, ToJSON)

data Statistics = Statistics
  { queries :: Int
  , pages :: Int
  , results :: Int
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
  -> [Query]
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
  -> [Query]
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
        resultsPerSecond = realToFrac results / seconds
    finish <- liftIO getCurrentTime
    pure Benchmark{..}

-- | Run the benchmark.
benchmark
  :: (MonadIO m)
  => NominalDiffTime
  -- ^ When the benchmark started.
  -> Int
  -- ^ Page size for each `Query` client.
  -> Statistics
  -- ^ The statistics so far.
  -> Query
  -- ^ The contract filter.
  -> MarloweQueryClient m Statistics
  -- ^ Action to run the benchmark.
benchmark start pageSize initial (QueryHeaders cFilter) =
  benchmarkPaged start pageSize initial $ getContractHeaders cFilter
benchmark start _ initial (QueryState cId) =
  benchmarkUnpaged start initial =<< getContractState cId
benchmark start _ initial (QueryTransaction txId) =
  benchmarkUnpaged start initial =<< getTransaction txId
benchmark start _ initial (QueryTransactions cId) =
  benchmarkUnpaged start initial =<< getTransactions cId
benchmark start _ initial (QueryWithdrawal txId) =
  benchmarkUnpaged start initial =<< getWithdrawal txId
benchmark start pageSize initial (QueryWithdrawals wFilter) =
  benchmarkPaged start pageSize initial $ getWithdrawals wFilter
benchmark start pageSize initial (QueryPayouts pFilter) =
  benchmarkPaged start pageSize initial $ getPayouts pFilter
benchmark start _ initial (QueryPayout txOutRef) =
  benchmarkUnpaged start initial =<< getPayout txOutRef

benchmarkUnpaged
  :: (MonadIO m)
  => NominalDiffTime
  -> Statistics
  -> Maybe a
  -> MarloweQueryClient m Statistics
benchmarkUnpaged start initial@Statistics{queries, pages} result =
  do
    now <- liftIO getPOSIXTime
    pure $
      initial
        { queries = queries + 1
        , pages = pages + maybe 0 (const 1) result
        , duration = now - start
        }

benchmarkPaged
  :: (MonadIO m)
  => NominalDiffTime
  -> Int
  -> Statistics
  -> (Range a -> MarloweQueryClient m (Maybe (Page a b)))
  -> MarloweQueryClient m Statistics
benchmarkPaged start pageSize initial@Statistics{queries} query =
  let accumulate = (. query) . (=<<) . handleNextPage
      handleNextPage stats Nothing = pure stats
      handleNextPage stats@Statistics{pages, results} (Just Page{..}) =
        do
          now <- liftIO getPOSIXTime
          let stats' =
                stats
                  { pages = pages + 1
                  , results = results + length items
                  , duration = now - start
                  }
          case nextRange of
            Nothing -> pure stats'
            Just range -> stats' `accumulate` range
   in initial{queries = queries + 1} `accumulate` Range Nothing 0 pageSize Ascending
