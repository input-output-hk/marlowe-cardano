{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE EmptyCase                 #-}
{-# LANGUAGE EmptyDataDeriving         #-}
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
module History.Store where

import ChainSync.Client (ChainSyncMsg (..))
import Control.Distributed.Process (Closure, Process, ProcessId, SendPort, match, matchChan, newChan, receiveWait,
                                    sendChan)
import Control.Distributed.Process.Closure (mkClosure, remotable)
import Control.Distributed.Process.Serializable (Serializable)
import Control.Monad (unless, (<=<))
import Data.Binary (Binary)
import Data.Data (Typeable)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import History.Database (HistoryQuery, addEvents, rollBackward)
import Language.Marlowe.Runtime.Chain.Types (MarloweChainEvent (..), MarloweChainPoint (..))

sendSubscriber :: Serializable a => Set ProcessId -> Either MarloweChainPoint a -> Subscriber a -> Process ()
sendSubscriber deadProcesses msg Subscriber{..} = unless (Set.member pid deadProcesses) $ sendChan replyChan msg

sendResponse :: Serializable a => Set ProcessId -> a -> Subscriber a -> Process ()
sendResponse deadProcesses = sendSubscriber deadProcesses . Right

sendRollback :: Serializable a => Set ProcessId -> MarloweChainPoint -> Subscriber a -> Process ()
sendRollback deadProcesses = sendSubscriber deadProcesses . Left

data HistoryStoreDependencies = HistoryStoreDependencies
  { dbChan        :: SendPort HistoryQuery
  , initStoreChan :: SendPort HistoryStoreChan
  }
  deriving (Generic, Typeable, Show, Eq)
  deriving anyclass Binary

data Subscriber a = Subscriber
  { pid       :: ProcessId
  , replyChan :: SendPort (Either MarloweChainPoint a)
  }
  deriving (Generic, Typeable, Show, Eq)
  deriving anyclass Binary

data HistoryStoreQuery
  deriving (Generic, Typeable, Show, Eq)
  deriving anyclass Binary

type HistoryStoreChan = SendPort HistoryStoreQuery

data HistoryStoreState = HistoryStoreState

historyStore :: HistoryStoreDependencies -> Process ()
historyStore HistoryStoreDependencies{..} = do
  (sendQuery, receiveQuery) <- newChan
  sendChan initStoreChan sendQuery
  let
    initialState = HistoryStoreState
    go state = receiveWait
      [ matchChan receiveQuery $ go <=< \case
      , match \events -> addEvents dbChan events *> go state
      , match \case
          ChainSyncDone -> pure ()
          ChainSyncEvent (MarloweRollBackward point _) -> do
            rollBackward dbChan point
            go state
          _ -> go state
      ]
  go initialState

remotable ['historyStore]

process :: HistoryStoreDependencies -> Closure (Process ())
process = $(mkClosure 'historyStore)
