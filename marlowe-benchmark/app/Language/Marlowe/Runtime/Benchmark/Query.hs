{-# LANGUAGE RecordWildCards #-}

module Language.Marlowe.Runtime.Benchmark.Query (
  Benchmark (..),
  measure,
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Marlowe (MarloweT)
import Data.Aeson (ToJSON)
import Data.Default (Default (..))
import Data.Foldable (foldlM)
import Data.List.Split (chunksOf)
import Data.Time.Clock (NominalDiffTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Generics (Generic)
import Language.Marlowe.Protocol.Query.Client (MarloweQueryClient)
import qualified Language.Marlowe.Protocol.Query.Client as Query
import Language.Marlowe.Protocol.Query.Types (ContractFilter)
import qualified Language.Marlowe.Protocol.Query.Types as Query
import Language.Marlowe.Runtime.Client (runMarloweQueryClient)
import UnliftIO (forConcurrently)

data Benchmark = Benchmark
  { metric :: String
  , query :: String
  , queriesPerSecond :: Double
  , pagesPerSecond :: Double
  , contractsPerSecond :: Double
  , seconds :: Double
  }
  deriving (Eq, Generic, Ord, Show, ToJSON)

data Statistics = Statistics
  { queries :: Integer
  , pages :: Integer
  , contracts :: Integer
  , duration :: NominalDiffTime
  }
  deriving (Eq, Generic, Ord, Show, ToJSON)

instance Default Statistics where
  def = Statistics def def def 0

measure
  :: Int
  -> Int
  -> Int
  -> String
  -> [ContractFilter]
  -> MarloweT IO [Benchmark]
measure parallelism batchSize pageSize query filters =
  let batches = take parallelism $ chunksOf batchSize filters
   in forConcurrently batches $
        run "Query" pageSize query

run
  :: String
  -> Int
  -> String
  -> [ContractFilter]
  -> MarloweT IO Benchmark
run metric pageSize query filters =
  do
    start <- liftIO getPOSIXTime
    Statistics{..} <- foldlM ((runMarloweQueryClient .) . benchmark start pageSize) def filters
    let seconds = realToFrac duration
        queriesPerSecond = fromInteger queries / seconds
        pagesPerSecond = fromInteger pages / seconds
        contractsPerSecond = fromInteger contracts / seconds
    pure Benchmark{..}

benchmark
  :: (MonadIO m)
  => NominalDiffTime
  -> Int
  -> Statistics
  -> ContractFilter
  -> MarloweQueryClient m Statistics
benchmark start pageSize initial@Statistics{queries} cFilter =
  let accumulate = (. Query.getContractHeaders cFilter) . (=<<) . handleNextPage
      handleNextPage stats Nothing = pure stats
      handleNextPage stats@Statistics{pages, contracts} (Just Query.Page{..}) =
        do
          now <- liftIO getPOSIXTime
          let stats' =
                stats
                  { pages = pages + 1
                  , contracts = contracts + toInteger (length items)
                  , duration = now - start
                  }
          case nextRange of
            Nothing -> pure stats'
            Just range -> stats' `accumulate` range
   in initial{queries = queries + 1} `accumulate` Query.Range Nothing 0 pageSize Query.Ascending
