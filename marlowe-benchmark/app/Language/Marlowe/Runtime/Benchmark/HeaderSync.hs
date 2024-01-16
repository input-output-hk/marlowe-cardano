{-# LANGUAGE RecordWildCards #-}

-- | Benchmark for the `HeaderSync` protocol.
module Language.Marlowe.Runtime.Benchmark.HeaderSync (
  -- * Benchmarking
  Benchmark (..),
  measure,
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Marlowe (MarloweT)
import Control.Monad.Trans.Marlowe.Class (runMarloweHeaderSyncClient)
import Data.Aeson (ToJSON)
import Data.Bifunctor (second)
import Data.Default (Default (..))
import Data.Time.Clock (NominalDiffTime, UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Generics (Generic)
import Language.Marlowe.Protocol.HeaderSync.Client (
  ClientStIdle (..),
  ClientStNext (..),
  ClientStWait (..),
  MarloweHeaderSyncClient (MarloweHeaderSyncClient),
 )
import Language.Marlowe.Runtime.Core.Api (ContractId)
import Language.Marlowe.Runtime.Discovery.Api (contractId)
import UnliftIO (replicateConcurrently)

import qualified Data.Set as S (Set, fromList, size)

data Benchmark = Benchmark
  { metric :: String
  , start :: UTCTime
  , finish :: UTCTime
  , blocksPerSecond :: Double
  , contractsPerSecond :: Double
  , seconds :: Double
  }
  deriving (Eq, Generic, Ord, Show, ToJSON)

data Statistics = Statistics
  { blocks :: Int
  , contracts :: S.Set ContractId
  , duration :: NominalDiffTime
  }
  deriving (Eq, Generic, Ord, Show, ToJSON)

instance Default Statistics where
  def = Statistics def mempty 0

-- | Measure the performance of the protocol.
measure
  :: Int
  -- ^ Number of parallel clients for `HeaderSync` protocol.
  -> Int
  -- ^ Maximum number of contracts to be read by each `HeaderSync` client.
  -> MarloweT IO ([Benchmark], S.Set ContractId)
measure parallelism maxContracts =
  second head . unzip
    <$> replicateConcurrently parallelism (run "HeaderSync" maxContracts)

-- | Run a benchmark client.
run
  :: String
  -- ^ Label for the benchmark.
  -> Int
  -- ^ Maximum number of contracts to be read by each `HeaderSync` client.
  -> MarloweT IO (Benchmark, S.Set ContractId)
  -- ^ Action for running the benchmark.
run metric maxContracts =
  do
    start <- liftIO getCurrentTime
    Statistics{..} <- runMarloweHeaderSyncClient . benchmark maxContracts =<< liftIO getPOSIXTime
    let seconds = realToFrac duration
        blocksPerSecond = realToFrac blocks / seconds
        contractsPerSecond = fromIntegral (S.size contracts) / seconds
    finish <- liftIO getCurrentTime
    pure (Benchmark{..}, contracts)

-- | Run the benchmark.
benchmark
  :: (MonadIO m)
  => Int
  -- ^ Maximum number of contracts to be read by each `HeaderSync` client.
  -> NominalDiffTime
  -- ^ When the benchmark started.
  -> MarloweHeaderSyncClient m Statistics
  -- ^ Action for running the benchmark.
benchmark maxContracts start =
  let clientIdle = SendMsgRequestNext . clientNext
      clientWait = pure . SendMsgCancel . SendMsgDone
      clientNext stats@Statistics{..} =
        ClientStNext
          { recvMsgNewHeaders = \_blockHeader results ->
              if S.size contracts >= maxContracts
                then pure $ SendMsgDone stats
                else do
                  now <- liftIO getPOSIXTime
                  pure $
                    clientIdle $
                      stats
                        { blocks = blocks + 1
                        , contracts = contracts <> S.fromList (contractId <$> results)
                        , duration = now - start
                        }
          , recvMsgRollBackward = \_chainPoint ->
              pure $ clientIdle stats
          , recvMsgWait = clientWait stats
          }
   in MarloweHeaderSyncClient . pure $ clientIdle def
