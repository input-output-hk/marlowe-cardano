{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Language.Marlowe.Runtime.History.QueryServer
  where

import Control.Concurrent.Async (Concurrently(Concurrently, runConcurrently))
import Control.Concurrent.STM (STM, atomically)
import Control.Exception (SomeException, catch)
import Data.Bifunctor (bimap)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import Data.Void (Void)
import Language.Marlowe.Runtime.Core.Api
import Language.Marlowe.Runtime.History.Api
import Network.Protocol.Query.Server
import Network.Protocol.Query.Types
import Numeric.Natural (Natural)
import System.IO (hPutStrLn, stderr)

newtype RunQueryServer m = RunQueryServer (forall a. RuntimeHistoryQueryServer m a -> m a)

data HistoryQueryServerDependencies = HistoryQueryServerDependencies
  { acceptRunQueryServer :: IO (RunQueryServer IO)
  , followerStatuses     :: STM (Map ContractId FollowerStatus)
  , followerPageSize     :: Natural
  }

newtype HistoryQueryServer = HistoryQueryServer
  { runHistoryQueryServer :: IO ()
  }

mkHistoryQueryServer :: HistoryQueryServerDependencies -> STM HistoryQueryServer
mkHistoryQueryServer HistoryQueryServerDependencies{..} = do
  let
    runHistoryQueryServer = do
      runQueryServer <- acceptRunQueryServer
      Worker{..} <- atomically $ mkWorker WorkerDependencies {..}
      runConcurrently $
        Concurrently (runWorker `catch` catchWorker) *> Concurrently runHistoryQueryServer
  pure $ HistoryQueryServer { runHistoryQueryServer }

catchWorker :: SomeException -> IO ()
catchWorker = hPutStrLn stderr . ("Query worker crashed with exception: " <>) . show

data WorkerDependencies = WorkerDependencies
  { runQueryServer   :: RunQueryServer IO
  , followerStatuses :: STM (Map ContractId FollowerStatus)
  , followerPageSize :: Natural
  }

newtype Worker = Worker
  { runWorker :: IO ()
  }

mkWorker :: WorkerDependencies -> STM Worker
mkWorker WorkerDependencies{..} =
  let
    RunQueryServer run = runQueryServer
  in
    pure Worker { runWorker = run server }

  where
    server :: RuntimeHistoryQueryServer IO ()
    server = QueryServer $ pure $ ServerStInit \case
      GetFollowedContracts  -> getFollowedContractsServer followerPageSize followerStatuses

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
