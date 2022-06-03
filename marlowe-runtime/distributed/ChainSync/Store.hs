{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeApplications          #-}
module ChainSync.Store where

import ChainSync.Client (ChainSyncMsg (..))
import ChainSync.Database (Block (..), ChainSyncQuery, rollBackward, rollForward)
import qualified ChainSync.Database as DB
import Control.Distributed.Process (Closure, Process, ProcessId, ProcessMonitorNotification (..), ReceivePort, SendPort,
                                    getSelfPid, match, matchChan, monitor, newChan, receiveWait, sendChan)
import Control.Distributed.Process.Closure (mkClosure, remotable)
import Control.Distributed.Process.Serializable (Serializable)
import Control.Monad (unless, (<=<))
import Data.Binary (Binary)
import Data.Data (Typeable)
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Language.Marlowe.Runtime.Chain.Types (MarloweChainEvent (..), MarloweChainPoint (..))

sendSubscriber :: Serializable a => Set ProcessId -> Either MarloweChainPoint a -> Subscriber a -> Process ()
sendSubscriber deadProcesses msg Subscriber{..} = unless (Set.member pid deadProcesses) $ sendChan replyChan msg

sendResponse :: Serializable a => Set ProcessId -> a -> Subscriber a -> Process ()
sendResponse deadProcesses = sendSubscriber deadProcesses . Right

sendRollback :: Serializable a => Set ProcessId -> MarloweChainPoint -> Subscriber a -> Process ()
sendRollback deadProcesses = sendSubscriber deadProcesses . Left

getBlocks
  :: SendPort ChainStoreQuery
  -> MarloweChainPoint
  -> Process (ReceivePort (Either MarloweChainPoint Block))
getBlocks queryChan startingAt = do
  (replyChan, receiveResponse) <- newChan
  pid <- getSelfPid
  sendChan queryChan $ GetBlocks startingAt $ Subscriber pid replyChan
  pure receiveResponse

data ChainSyncStoreDependencies = ChainSyncStoreDependencies
  { dbChan        :: SendPort ChainSyncQuery
  , initStoreChan :: SendPort (SendPort ChainStoreQuery)
  }
  deriving (Generic, Typeable, Show, Eq)
  deriving anyclass Binary

data Subscriber a = Subscriber
  { pid       :: ProcessId
  , replyChan :: SendPort (Either MarloweChainPoint a)
  }
  deriving (Generic, Typeable, Show, Eq)
  deriving anyclass Binary

type BlockSubscriber = Subscriber Block

data ChainStoreQuery
  = GetBlocks MarloweChainPoint BlockSubscriber
  deriving (Generic, Typeable, Show, Eq)
  deriving anyclass Binary

data ChainSyncStoreState = ChainSyncStoreState
  { blockSubscribers :: Map ProcessId BlockSubscriber
  , deadSubscribers  :: Set ProcessId
  }

chainSyncStore :: ChainSyncStoreDependencies -> Process ()
chainSyncStore ChainSyncStoreDependencies{..} = do
  (sendQuery, receiveQuery) <- newChan
  sendChan initStoreChan sendQuery
  let
    initialState = ChainSyncStoreState
      { blockSubscribers = mempty
      , deadSubscribers = mempty
      }

    go state@ChainSyncStoreState{..} = receiveWait
      [ match \(ProcessMonitorNotification _ pid _) -> go state { deadSubscribers = Set.insert pid deadSubscribers }
      , matchChan receiveQuery $ go <=< \(GetBlocks startingAt subscriber) ->
          handleGetBlocks state startingAt subscriber
      , match \case
          ChainSyncDone -> pure ()
          ChainSyncEvent (MarloweRollForward header txs tip) -> do
            rollForward dbChan (Block header txs) tip
            go =<< rollForwardSubscribers state header txs
          ChainSyncEvent (MarloweRollBackward point tip) -> do
            rollBackward dbChan point tip
            go =<< rollbackSubscribers point state
          _ -> go state
      ]

    handleGetBlocks state@ChainSyncStoreState{..} startingAt subscriber@Subscriber{..} = do
      DB.getBlocksAfter dbChan startingAt >>= \case
        Nothing     -> sendRollback deadSubscribers MarloweChainPointAtGenesis subscriber
        Just blocks -> traverse_ (\block -> sendResponse deadSubscribers block subscriber) blocks
      void $ monitor pid
      pure state
        { blockSubscribers = Map.insert pid subscriber blockSubscribers
        }

    rollbackSubscribers point state@ChainSyncStoreState{..} = do
      traverse_ (sendRollback deadSubscribers point) blockSubscribers
      pure state

    rollForwardSubscribers state _ [] = pure state
    rollForwardSubscribers state@ChainSyncStoreState{..} header txs = do
      traverse_ (sendResponse deadSubscribers $ Block header txs) blockSubscribers
      pure state
  go initialState

remotable ['chainSyncStore]

process :: ChainSyncStoreDependencies -> Closure (Process ())
process = $(mkClosure 'chainSyncStore)
