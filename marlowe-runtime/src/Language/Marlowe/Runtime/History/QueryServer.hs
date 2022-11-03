{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Language.Marlowe.Runtime.History.QueryServer
  where

import Colog (logError)
import Control.Concurrent.STM (STM, atomically)
import Control.Exception (SomeException)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (bimap)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import qualified Data.Text as T
import Data.Void (Void, absurd)
import Language.Marlowe.Runtime.Core.Api
import Language.Marlowe.Runtime.History.Api
import Language.Marlowe.Runtime.Logging.Colog.LogIO
  (ConcurrentlyLogIO(ConcurrentlyLogIO), LogIO, catchLogIO, runConcurrentlyLogIO)
import Network.Protocol.Query.Server
import Network.Protocol.Query.Types
import Numeric.Natural (Natural)

newtype RunQueryServer m = RunQueryServer (forall a. RuntimeHistoryQueryServer m a -> m a)

data HistoryQueryServerDependencies = HistoryQueryServerDependencies
  { acceptRunQueryServer :: LogIO (RunQueryServer LogIO)
  , followerStatuses     :: STM (Map ContractId FollowerStatus)
  , followerPageSize     :: Natural
  }

newtype HistoryQueryServer = HistoryQueryServer
  { runHistoryQueryServer :: LogIO ()
  }

mkHistoryQueryServer :: HistoryQueryServerDependencies -> STM HistoryQueryServer
mkHistoryQueryServer HistoryQueryServerDependencies{..} = do
  let
    runHistoryQueryServer = do
      runQueryServer <- acceptRunQueryServer
      Worker{..} <- liftIO $ atomically $ mkWorker WorkerDependencies {..}
      runConcurrentlyLogIO $
        ConcurrentlyLogIO (runWorker `catchLogIO` catchWorker) *> ConcurrentlyLogIO runHistoryQueryServer
  pure $ HistoryQueryServer { runHistoryQueryServer }

catchWorker :: SomeException -> LogIO ()
catchWorker = logError . T.pack . mappend "Query worker crashed with exception: " . show

data WorkerDependencies = WorkerDependencies
  { runQueryServer   :: RunQueryServer LogIO
  , followerStatuses :: STM (Map ContractId FollowerStatus)
  , followerPageSize :: Natural
  }

newtype Worker = Worker
  { runWorker :: LogIO ()
  }

mkWorker :: WorkerDependencies -> STM Worker
mkWorker WorkerDependencies{..} =
  let
    RunQueryServer run = runQueryServer
  in
    pure Worker { runWorker = run server }

  where
    server :: RuntimeHistoryQueryServer LogIO ()
    server = queryServer' querySchema \case
      GetFollowedContracts  -> getFollowedContractsServer followerPageSize followerStatuses
      GetStatuses contractIds -> getStatusesServer contractIds followerStatuses

getFollowedContractsServer
  :: Natural
  -> STM (Map ContractId FollowerStatus)
  -> LogIO (ServerStNext HistoryQuery 'CanReject ContractId Void (Map ContractId FollowerStatus) LogIO ())
getFollowedContractsServer followerPageSize followerStatuses = do
  followers <- liftIO $ atomically followerStatuses
  pure $ next followers
  where
  next
    :: Map ContractId FollowerStatus
    -> ServerStNext HistoryQuery k ContractId Void (Map ContractId FollowerStatus) LogIO ()
  next followers =
    let
      (results, remaining) = bimap (Map.fromDistinctAscList . fmap snd) (fmap snd)
        $ break ((== followerPageSize) . fst)
        $ zip [0..]
        $ Map.toAscList followers
      nextPageDelimiter = fst <$> listToMaybe remaining
    in
      SendMsgNextPage results nextPageDelimiter ServerStPage
        { recvMsgRequestDone = pure ()
        , recvMsgRequestNext = \delimiter -> pure
            $ next
            $ Map.fromDistinctAscList
            $ dropWhile ((/= delimiter) . fst) remaining
        }

getStatusesServer
  :: Set ContractId
  -> STM (Map ContractId FollowerStatus)
  -> LogIO (ServerStNext HistoryQuery 'CanReject Void Void (Map ContractId FollowerStatus) LogIO ())
getStatusesServer contractIds followerStatuses = do
  followers <- liftIO $ atomically followerStatuses
  pure $ SendMsgNextPage (Map.restrictKeys followers contractIds) Nothing ServerStPage
    { recvMsgRequestDone = pure ()
    , recvMsgRequestNext = absurd
    }
