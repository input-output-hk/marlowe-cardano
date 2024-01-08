{-# LANGUAGE RecordWildCards #-}

-- | Benchmark for the `BulkSync` protocol.
module Language.Marlowe.Runtime.Benchmark.BulkSync (
  -- * Benchmarking
  Benchmark (..),
  measure,
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Marlowe (MarloweT)
import Data.Aeson (ToJSON)
import Data.Default (Default (..))
import Data.Time.Clock (NominalDiffTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Language.Marlowe.Protocol.BulkSync.Client (
  ClientStIdle (..),
  ClientStNext (..),
  ClientStPoll (..),
  MarloweBulkSyncClient (MarloweBulkSyncClient),
 )
import Language.Marlowe.Runtime.Client (runMarloweBulkSyncClient)
import Language.Marlowe.Runtime.History.Api (MarloweBlock (..), MarloweCreateTransaction (newContracts))
import UnliftIO (replicateConcurrently)

data Benchmark = Benchmark
  { metric :: String
  , blocksPerSecond :: Double
  , createsPerSecond :: Double
  , applyInputsPerSecond :: Double
  , withdrawsPerSecond :: Double
  , seconds :: Double
  }
  deriving (Eq, Generic, Ord, Show, ToJSON)

data Statistics = Statistics
  { blocks :: Int
  , creates :: Int
  , applyInputs :: Int
  , withdraws :: Int
  , duration :: NominalDiffTime
  }
  deriving (Eq, Generic, Ord, Show, ToJSON)

instance Default Statistics where
  def = Statistics def def def def 0

-- | Measure the performance of the protocol.
measure
  :: Int
  -- ^ Number of parallel clients for the `BulkSync` protocol.
  -> Word8
  -- ^ Number of blocks to fetch at a time for the `BulkSync` clients.
  -> Int
  -- ^ Maximum number of blocks to fetch for each `BulkSync` client.
  -> MarloweT IO [Benchmark]
  -- ^ Action for running the benchmark.
measure parallelism pageSize maxBlocks =
  replicateConcurrently parallelism $
    run "BulkSync" pageSize maxBlocks

-- | Run the benchmarking client.
run
  :: String
  -- ^ Label for the benchmark.
  -> Word8
  -- ^ Number of blocks to fetch at a time for the `BulkSync` clients.
  -> Int
  -- ^ Maximum number of blocks to fetch for each `BulkSync` client.
  -> MarloweT IO Benchmark
  -- ^ Action for running the benchmark.
run metric pageSize maxBlocks =
  do
    Statistics{..} <- runMarloweBulkSyncClient . benchmark pageSize maxBlocks =<< liftIO getPOSIXTime
    let seconds = realToFrac duration
        blocksPerSecond = realToFrac blocks / seconds
        createsPerSecond = realToFrac creates / seconds
        applyInputsPerSecond = realToFrac applyInputs / seconds
        withdrawsPerSecond = realToFrac withdraws / seconds
    pure Benchmark{..}

-- | Run a benchmark.
benchmark
  :: (MonadIO m)
  => Word8
  -- ^ Number of blocks to fetch at a time for the `BulkSync` clients.
  -> Int
  -- ^ Maximum number of blocks to fetch for each `BulkSync` client.
  -> NominalDiffTime
  -- ^ When the benchmark started.
  -> MarloweBulkSyncClient m Statistics
  -- ^ Action for running the benchmark.
benchmark pageSize maxBlocks start =
  let idle = SendMsgRequestNext pageSize . next
      next stats@Statistics{..} =
        ClientStNext
          { recvMsgRollForward = \blocks' _tip ->
              if blocks >= maxBlocks
                then pure $ SendMsgDone stats
                else do
                  now <- liftIO getPOSIXTime
                  pure $
                    idle
                      stats
                        { blocks = blocks + length blocks'
                        , creates = creates + sum (length . mconcat . fmap newContracts . createTransactions <$> blocks')
                        , applyInputs = applyInputs + sum (length . applyInputsTransactions <$> blocks')
                        , withdraws = withdraws + sum (length . withdrawTransactions <$> blocks')
                        , duration = now - start
                        }
          , recvMsgRollBackward = \_point _tip -> pure $ idle stats
          , recvMsgWait = pure . SendMsgCancel $ SendMsgDone stats
          }
   in MarloweBulkSyncClient . pure $ idle def
