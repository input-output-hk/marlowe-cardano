{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Benchmark for the `Sync` protocol.
module Language.Marlowe.Runtime.Benchmark.Sync (
  -- * Benchmarking
  Benchmark (..),
  measure,
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Marlowe (MarloweT)
import Data.Aeson (ToJSON)
import Data.Default (Default (..))
import Data.Foldable (foldlM, toList)
import Data.List.Split (chunksOf)
import Data.Time.Clock (NominalDiffTime, UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Type.Equality ((:~:) (Refl))
import GHC.Generics (Generic)
import Language.Marlowe.Protocol.Sync.Client (
  ClientStFollow (..),
  ClientStIdle (..),
  ClientStInit (..),
  ClientStNext (..),
  ClientStWait (..),
  MarloweSyncClient (MarloweSyncClient),
 )
import Language.Marlowe.Runtime.Client (runMarloweSyncClient)
import Language.Marlowe.Runtime.Core.Api (
  ContractId,
  IsMarloweVersion (..),
  MarloweVersion (..),
  MarloweVersionTag (V1),
  assertVersionsEqual,
 )
import UnliftIO (forConcurrently)

import qualified Data.Set as S (Set)

data Benchmark = Benchmark
  { metric :: String
  , start :: UTCTime
  , finish :: UTCTime
  , contractsPerSecond :: Double
  , stepsPerSecond :: Double
  , seconds :: Double
  }
  deriving (Eq, Generic, Ord, Show, ToJSON)

data Statistics v = Statistics
  { contracts :: Int
  , steps :: Int
  , duration :: NominalDiffTime
  , version :: MarloweVersion v
  }
  deriving (Eq, Generic, Ord, Show, ToJSON)

instance (IsMarloweVersion v) => Default (Statistics v) where
  def = Statistics def def 0 marloweVersion

-- | Measure the performance of the protocol.
measure
  :: Int
  -- ^ Number of parallel clients for `Sync` protocol.
  -> Int
  -- ^ Number of contracts to be read by each `Sync` client.
  -> S.Set ContractId
  -> MarloweT IO [Benchmark]
  -- ^ Action for running the benchmark.
measure parallelism batchSize contractIds =
  let batches = take parallelism . chunksOf batchSize $ toList contractIds
   in forConcurrently batches $
        run "Sync"

-- | Run a benchmark client.
run
  :: String
  -- ^ Label for the benchmark.
  -> [ContractId]
  -- ^ Contracts to be synced.
  -> MarloweT IO Benchmark
  -- ^ Action for running the benchmark.
run metric contractIds =
  do
    start' <- liftIO getPOSIXTime
    start <- liftIO getCurrentTime
    Statistics{..} <- foldlM ((runMarloweSyncClient .) . benchmark start') (def :: Statistics 'V1) contractIds
    let seconds = realToFrac duration
        contractsPerSecond = realToFrac contracts / seconds
        stepsPerSecond = realToFrac steps / seconds
    finish <- liftIO getCurrentTime
    pure Benchmark{..}

-- | Run the benchmark.
benchmark
  :: forall v m
   . (IsMarloweVersion v)
  => (MonadIO m)
  => NominalDiffTime
  -- ^ When the benchmark started.
  -> Statistics v
  -- ^ The statistics so far.
  -> ContractId
  -- ^ The contract to be synced.
  -> MarloweSyncClient m (Statistics v)
  -- ^ Action for running the benchmark.
benchmark start initial@Statistics{contracts} contractId =
  let clientInit =
        SendMsgFollowContract
          contractId
          ClientStFollow
            { recvMsgContractNotFound = pure initial
            , recvMsgContractFound = \_blockHeader version _createStep ->
                case version `assertVersionsEqual` (marloweVersion :: MarloweVersion v) of
                  Refl -> do
                    now <- liftIO getPOSIXTime
                    pure $
                      clientIdle version $
                        initial
                          { contracts = contracts + 1
                          , duration = now - start
                          }
            }
      clientIdle = (SendMsgRequestNext .) . clientNext
      clientNext :: MarloweVersion v -> Statistics v -> ClientStNext v m (Statistics v)
      clientNext version stats@Statistics{steps} =
        ClientStNext
          { recvMsgRollBackCreation = pure stats
          , recvMsgRollBackward = \_blockHeader ->
              pure $ clientIdle version stats
          , recvMsgRollForward = \_blockHeader steps' ->
              do
                now <- liftIO getPOSIXTime
                pure
                  . clientIdle version
                  $ stats
                    { steps = steps + length steps'
                    , duration = now - start
                    }
          , recvMsgWait =
              pure . SendMsgCancel $ SendMsgDone stats
          }
   in MarloweSyncClient $ pure clientInit
