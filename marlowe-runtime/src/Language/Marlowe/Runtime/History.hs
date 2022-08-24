{-# LANGUAGE RankNTypes  #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE StrictData  #-}

module Language.Marlowe.Runtime.History where

import Control.Concurrent.Async (race_)
import Control.Concurrent.STM (STM, atomically)
import Control.Monad (when)
import Language.Marlowe.Runtime.ChainSync.Api (RuntimeChainSeekClient, ScriptHash, SlotConfig)
import Language.Marlowe.Runtime.Core.Api (SomeMarloweVersion, parseContractId)
import Language.Marlowe.Runtime.History.FollowerSupervisor

data HistoryDependencies = HistoryDependencies
  { getMarloweVersion  :: ScriptHash -> Maybe (SomeMarloweVersion, ScriptHash)
  , connectToChainSeek :: forall a. RuntimeChainSeekClient IO a -> IO a
  , slotConfig         :: SlotConfig
  , securityParameter  :: Int
  }

newtype History = History
  { runHistory :: IO ()
  }


mkHistory :: HistoryDependencies -> STM History
mkHistory HistoryDependencies{..} = do
  FollowerSupervisor{..} <- mkFollowerSupervisor FollowerSupervisorDependencies{..}
  let
    go = do
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

        ["changes"] -> do
          print =<< atomically changes
          pure True

        ["quit"] -> pure False

        _ -> True <$ putStrLn "invalid command"
      when loop go
  pure History
    { runHistory = race_ runFollowerSupervisor $ putStrLn "enter a command" *> go
    }
