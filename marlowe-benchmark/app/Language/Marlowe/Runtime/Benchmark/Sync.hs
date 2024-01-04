{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Marlowe.Runtime.Benchmark.Sync (
  Benchmark (..),
  measure,
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Marlowe (MarloweT)
import Data.Aeson (ToJSON)
import Data.Default (Default (..))
import Data.Foldable (foldlM, toList)
import Data.List.Split (chunksOf)
import Data.Time.Clock (NominalDiffTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Type.Equality ((:~:) (Refl))
import GHC.Generics (Generic)
import Language.Marlowe.Protocol.Sync.Client (
  ClientStFollow (ClientStFollow, recvMsgContractFound, recvMsgContractNotFound),
  ClientStIdle (SendMsgDone, SendMsgRequestNext),
  ClientStInit (SendMsgFollowContract),
  ClientStNext (..),
  ClientStWait (SendMsgCancel),
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
  , contractsPerSecond :: Double
  , stepsPerSecond :: Double
  , seconds :: Double
  }
  deriving (Eq, Generic, Ord, Show, ToJSON)

data Statistics v = Statistics
  { contracts :: Integer
  , steps :: Integer
  , duration :: NominalDiffTime
  , version :: MarloweVersion v
  }
  deriving (Eq, Generic, Ord, Show, ToJSON)

instance (IsMarloweVersion v) => Default (Statistics v) where
  def = Statistics def def 0 marloweVersion

measure
  :: Int
  -> Int
  -> S.Set ContractId
  -> MarloweT IO [Benchmark]
measure count batchSize contractIds =
  let batches = take count . chunksOf batchSize $ toList contractIds
   in forConcurrently batches $
        run "Sync"

run
  :: String
  -> [ContractId]
  -> MarloweT IO Benchmark
run metric contractIds =
  do
    start <- liftIO getPOSIXTime
    Statistics{..} <- foldlM ((runMarloweSyncClient .) . benchmark start) (def :: Statistics 'V1) contractIds
    let seconds = realToFrac duration
        contractsPerSecond = fromInteger contracts / seconds
        stepsPerSecond = fromInteger steps / seconds
    pure Benchmark{..}

benchmark
  :: forall v m
   . (IsMarloweVersion v)
  => (MonadIO m)
  => NominalDiffTime
  -> Statistics v
  -> ContractId
  -> MarloweSyncClient m (Statistics v)
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
                    { steps = toInteger (length steps') + steps
                    , duration = now - start
                    }
          , recvMsgWait =
              pure . SendMsgCancel $ SendMsgDone stats
          }
   in MarloweSyncClient $ pure clientInit
