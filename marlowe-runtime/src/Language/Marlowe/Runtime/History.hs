{-# LANGUAGE GADTs       #-}
{-# LANGUAGE RankNTypes  #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE StrictData  #-}

module Language.Marlowe.Runtime.History where

import Control.Concurrent.Async (Concurrently (..))
import Control.Concurrent.STM (STM, atomically, modifyTVar, newTVar, readTVar, retry)
import Control.Monad (forever, guard, when)
import Data.Foldable (asum)
import qualified Data.Map as Map
import Language.Marlowe.Runtime.ChainSync.Api (RuntimeChainSeekClient, ScriptHash, SlotConfig, WithGenesis (..),
                                               isAfter)
import Language.Marlowe.Runtime.Core.Api (MarloweVersion (..), SomeMarloweVersion, parseContractId)
import Language.Marlowe.Runtime.History.Api (FollowerStatus (..), SomeHistoryPage (..))
import Language.Marlowe.Runtime.History.Follower
import Language.Marlowe.Runtime.History.FollowerSupervisor
import Language.Marlowe.Runtime.History.JobServer
import Language.Marlowe.Runtime.History.QueryServer
import Numeric.Natural (Natural)

data HistoryDependencies = HistoryDependencies
  { acceptRunJobServer   :: IO (RunJobServer IO)
  , acceptRunQueryServer :: IO (RunQueryServer IO)
  , getMarloweVersion    :: ScriptHash -> Maybe (SomeMarloweVersion, ScriptHash)
  , connectToChainSeek   :: forall a. RuntimeChainSeekClient IO a -> IO a
  , followerPageSize     :: Natural
  , slotConfig           :: SlotConfig
  , securityParameter    :: Int
  }

newtype History = History
  { runHistory :: IO ()
  }

mkHistory :: HistoryDependencies -> STM History
mkHistory HistoryDependencies{..} = do
  -- TODO move to a database (SCP-4278)
  historyVar <- newTVar Map.empty
  FollowerSupervisor{..} <- mkFollowerSupervisor FollowerSupervisorDependencies{..}
  HistoryJobServer{..} <- mkHistoryJobServer HistoryJobServerDependencies{..}
  let
    getHistory contractId = do
      _ <- atomically $ followContract contractId
      atomically do
        statuses <- followerStatuses
        case Map.lookup contractId statuses of
          Just (Following _) -> retry
          Just Pending -> retry
          Just (Failed err) -> pure $ Left err
          _ -> do
            histories <- readTVar historyVar
            case Map.lookup contractId histories of
              Nothing -> retry
              Just (SomeHistoryPage version page) ->
                pure $ Right $ SomeHistoryProducer version $ pure $ HistoryProducer page Nothing
  HistoryQueryServer{..} <- mkHistoryQueryServer HistoryQueryServerDependencies{..}
  let
    repl = do
      line <- getLine
      loop <- case words line of

        ["add", cidRaw] -> True <$ case parseContractId cidRaw of
          Nothing  -> putStrLn "invalid cid"
          Just cid -> print =<< atomically (followContract cid)

        ["rm", cidRaw] -> True <$ case parseContractId cidRaw of
          Nothing  -> putStrLn "invalid cid"
          Just cid -> print =<< atomically (stopFollowingContract cid)

        ["status"] -> do
          print =<< atomically followerStatuses
          pure True

        ["quit"] -> pure False

        _ -> True <$ putStrLn "invalid command"
      when loop repl
  let
    collectHistories = forever $ atomically do
      newChanges <- changes
      guard $ not $ Map.null newChanges
      modifyTVar historyVar \histories -> Map.foldlWithKey applyChange histories newChanges
      where
        applyChange histories contractId = \case
          RemoveContract -> Map.delete contractId histories
          UpdateContract (SomeContractChanges version ContractChanges{..}) ->
            let
              history = case Map.lookup contractId histories of
                Nothing -> SomeHistoryPage version steps
                Just (SomeHistoryPage version' page) -> case (version, version') of
                  (MarloweV1, MarloweV1) ->
                    let
                      page' = case rollbackTo of
                        Nothing      -> page
                        Just Genesis -> Map.empty
                        Just (At slotNo) -> Map.fromDistinctAscList
                          $ dropWhile (isAfter slotNo . fst)
                          $ Map.toAscList page
                    in
                      SomeHistoryPage MarloweV1 $ Map.unionWith (<>) page' steps
            in
              Map.insert contractId history histories

  pure History
    { runHistory = runConcurrently $ asum $ Concurrently <$>
        [ runFollowerSupervisor
        , runHistoryJobServer
        , runHistoryQueryServer
        , putStrLn "enter a command" *> repl
        , collectHistories
        ]
    }
