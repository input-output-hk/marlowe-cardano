{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StrictData            #-}

module Language.Marlowe.Runtime.History.QueryServer where

import Control.Concurrent.Async (Concurrently (Concurrently, runConcurrently))
import Control.Concurrent.STM (STM, atomically)
import Control.Exception (SomeException, catch)
import Data.Bifunctor (bimap)
import Data.Functor (void)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Void (Void)
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader)
import Language.Marlowe.Runtime.Core.Api
import Language.Marlowe.Runtime.History.Api
import Network.Protocol.Query.Server
import Network.Protocol.Query.Types
import Numeric.Natural (Natural)
import System.IO (hPutStrLn, stderr)

newtype RunQueryServer m = RunQueryServer (forall a. RuntimeHistoryQueryServer m a -> IO a)

data HistoryProducer v = HistoryProducer (Map BlockHeader [ContractStep v]) (Maybe (IO (HistoryProducer v)))

data SomeHistoryProducer = forall v. SomeHistoryProducer (MarloweVersion v) (IO (HistoryProducer v))

data HistoryQueryServerDependencies = HistoryQueryServerDependencies
  { acceptRunQueryServer :: IO (RunQueryServer IO)
  , followerStatuses     :: STM (Map ContractId FollowerStatus)
  , followerPageSize     :: Natural
  , getHistory           :: ContractId -> IO (Either ContractHistoryError SomeHistoryProducer)
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
  , getHistory       :: ContractId -> IO (Either ContractHistoryError SomeHistoryProducer)
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
      GetHistory contractId -> getHistoryServer getHistory contractId

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

getHistoryServer
  :: (ContractId -> IO (Either ContractHistoryError SomeHistoryProducer))
  -> ContractId
  -> IO (ServerStNext HistoryQuery 'CanReject () ContractHistoryError SomeHistoryPage IO ())
getHistoryServer getHistory contractId = do
  result <- getHistory contractId
  case result of
    Left err                                     -> pure $ SendMsgReject err ()
    Right (SomeHistoryProducer version producer) -> next version <$> producer
  where
  next
    :: MarloweVersion v
    -> HistoryProducer v
    -> ServerStNext HistoryQuery k () ContractHistoryError SomeHistoryPage IO ()
  next version (HistoryProducer page mNextPage) =
    SendMsgNextPage (SomeHistoryPage version page) (void mNextPage) ServerStPage
      { recvMsgDone = pure ()
      , recvMsgRequestNext = \_ -> next version <$> fromMaybe (pure $ HistoryProducer mempty Nothing) mNextPage
      }
