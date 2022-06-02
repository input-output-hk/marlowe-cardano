{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module History.Digest where

import Cardano.Api (NetworkId (Testnet))
import Cardano.Api.Byron (NetworkMagic (NetworkMagic))
import ChainSync.Database (Block (..))
import ChainSync.Store (ChainStoreQuery, getBlocks)
import Control.Distributed.Process (Closure, Process, RemoteTable, SendPort, receiveChan, say, sendChan)
import Control.Distributed.Process.Closure (mkClosure, remotable)
import Control.Distributed.Process.Extras (spawnLinkLocal)
import Control.Distributed.Process.Extras.Time (TimeInterval)
import Control.Distributed.Process.Supervisor (ChildSpec (..), ChildStart (..), ChildStopPolicy (..), ChildType (..),
                                               RegisteredName (..), RestartPolicy (..), ShutdownMode (ParallelShutdown),
                                               restartOne)
import qualified Control.Distributed.Process.Supervisor as Supervisor
import Control.Monad (forever)
import Data.Binary (Binary)
import Data.Data (Typeable)
import Data.Foldable (traverse_)
import GHC.Generics (Generic)
import qualified History.Digest.Worker as Worker
import Language.Marlowe.Runtime.Chain.Types
import Language.Marlowe.Runtime.History (creationToEvent, extractCreations)
import Language.Marlowe.Runtime.History.Types (ContractCreationTxOut (..), ContractId (..), Event)

worker :: String -> RestartPolicy -> Maybe TimeInterval -> ChildStopPolicy -> ChildStart -> ChildSpec
worker name restartPolicy restartDelay stopPolicy start =
  ChildSpec name Worker restartPolicy restartDelay stopPolicy start $ Just $ LocalName name

data HistoryDigestConfig = HistoryDigestConfig
  deriving (Generic, Typeable, Show, Eq)

instance Binary HistoryDigestConfig

data HistoryDigestDependencies = HistoryDigestDependencies
  { config         :: HistoryDigestConfig
  , sendEvent      :: SendPort Event
  , chainStoreChan :: SendPort ChainStoreQuery
  }
  deriving (Generic, Typeable, Show, Eq)

instance Binary HistoryDigestDependencies

historyDigest :: HistoryDigestDependencies -> Process ()
historyDigest HistoryDigestDependencies{..} = do
  say "starting"
  supervisor <- spawnLinkLocal $ Supervisor.run restartOne ParallelShutdown []
  say $ "started supervisor " <> show supervisor
  getNextBlock <- getBlocks chainStoreChan MarloweChainPointAtGenesis
  forever $ receiveChan getNextBlock >>= \case
    Left _ -> pure ()
    Right Block{..} -> do
      let creations = extractCreations (Testnet $ NetworkMagic 1566) header =<< txs
      workersToStart <- processCreations sendEvent chainStoreChan creations
      traverse_ (Supervisor.startNewChild supervisor) workersToStart

processCreations
  :: SendPort Event
  -> SendPort ChainStoreQuery
  -> [Either ([TxOutRef], String) ContractCreationTxOut]
  -> Process [ChildSpec]
processCreations sendEvent sendQuery = fmap concat . traverse \case
  Left err -> do
    say $ "found invalid creation transaction " <> show err
    pure []
  Right creation@ContractCreationTxOut{contractId=ContractId{currencySymbol=MarlowePolicyId policyId}} -> do
    sendChan sendEvent $ creationToEvent creation
    let workerName = "history.digest.worker." <> read (show policyId)
    pure [worker workerName Transient Nothing StopImmediately $ RunClosure $ Worker.process $ Worker.HistoryDigestWorkerDependencies creation sendEvent sendQuery]


remotable ['historyDigest]

remoteTable :: RemoteTable -> RemoteTable
remoteTable = __remoteTable . Worker.__remoteTable

process :: HistoryDigestDependencies -> Closure (Process ())
process = $(mkClosure 'historyDigest)
