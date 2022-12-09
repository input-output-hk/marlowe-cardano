{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Language.Marlowe.Runtime.History.QueryServer
  where

import Control.Concurrent.Component
import Control.Concurrent.STM (STM, atomically)
import Data.Bifunctor (bimap)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import Data.Void (Void, absurd)
import Language.Marlowe.Runtime.Core.Api
import Language.Marlowe.Runtime.History.Api
import Network.Protocol.Driver (RunServer(..))
import Network.Protocol.Query.Server
import Network.Protocol.Query.Types
import Numeric.Natural (Natural)

type RunQueryServer m = RunServer m RuntimeHistoryQueryServer

data HistoryQueryServerDependencies = HistoryQueryServerDependencies
  { acceptRunQueryServer :: IO (RunQueryServer IO)
  , followerStatuses     :: STM (Map ContractId FollowerStatus)
  , followerPageSize     :: Natural
  }

historyQueryServer :: Component IO HistoryQueryServerDependencies ()
historyQueryServer = serverComponent worker mempty mempty \HistoryQueryServerDependencies{..} -> do
  runQueryServer <- acceptRunQueryServer
  pure WorkerDependencies {..}

data WorkerDependencies = WorkerDependencies
  { runQueryServer   :: RunQueryServer IO
  , followerStatuses :: STM (Map ContractId FollowerStatus)
  , followerPageSize :: Natural
  }

worker :: Component IO WorkerDependencies ()
worker = component_ \WorkerDependencies{..} -> do
  let
    RunServer run = runQueryServer

    server :: RuntimeHistoryQueryServer IO ()
    server = QueryServer $ pure $ ServerStInit \case
      GetFollowedContracts  -> getFollowedContractsServer followerPageSize followerStatuses
      GetStatuses contractIds -> getStatusesServer contractIds followerStatuses
  run server

getFollowedContractsServer
  :: Natural
  -> STM (Map ContractId FollowerStatus)
  -> IO (ServerStNext HistoryQuery 'CanReject ContractId Void (Map ContractId FollowerStatus) IO ())
getFollowedContractsServer followerPageSize followerStatuses = do
  followers <- atomically followerStatuses
  pure $ next followers
  where
  next
    :: Map ContractId FollowerStatus
    -> ServerStNext HistoryQuery k ContractId Void (Map ContractId FollowerStatus) IO ()
  next followers =
    let
      (results, remaining) = bimap (Map.fromDistinctAscList . fmap snd) (fmap snd)
        $ break ((== followerPageSize) . fst)
        $ zip [0..]
        $ Map.toAscList followers
      nextPageDelimiter = fst <$> listToMaybe remaining
    in
      SendMsgNextPage results nextPageDelimiter ServerStPage
        { recvMsgDone = pure ()
        , recvMsgRequestNext = \delimiter -> pure
            $ next
            $ Map.fromDistinctAscList
            $ dropWhile ((/= delimiter) . fst) remaining
        }

getStatusesServer
  :: Set ContractId
  -> STM (Map ContractId FollowerStatus)
  -> IO (ServerStNext HistoryQuery 'CanReject Void Void (Map ContractId FollowerStatus) IO ())
getStatusesServer contractIds followerStatuses = do
  followers <- atomically followerStatuses
  pure $ SendMsgNextPage (Map.restrictKeys followers contractIds) Nothing ServerStPage
    { recvMsgDone = pure ()
    , recvMsgRequestNext = absurd
    }
