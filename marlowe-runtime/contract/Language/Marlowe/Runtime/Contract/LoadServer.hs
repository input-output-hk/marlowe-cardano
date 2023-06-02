{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.Contract.LoadServer
  where

import Control.Concurrent.Component
import Control.Monad.Event.Class
import Data.Functor (void)
import Language.Marlowe.Core.V1.Semantics.Types
import Language.Marlowe.Protocol.Load.Server
import Language.Marlowe.Runtime.Contract.Store (ContractStagingArea(..), ContractStore(..))
import Network.Protocol.Connection
import Network.Protocol.Driver.Trace (HasSpanContext, runConnectionTraced)
import Network.TypedProtocol
import Observe.Event (NewEventArgs(..), addField)
import Observe.Event.Backend (simpleNewEventArgs)
import UnliftIO (MonadUnliftIO, bracketOnError)

data LoadServerSelector f where
  LoadContract :: LoadServerSelector Contract

data LoadServerDependencies r s m = forall n. LoadServerDependencies
  { batchSize :: Nat ('S n)
  , contractStore :: ContractStore m
  , loadSource :: SomeConnectionSourceTraced MarloweLoadServer r s m
  }

loadServer
  :: (MonadUnliftIO m, MonadInjectEvent r LoadServerSelector s m, HasSpanContext r)
  => Component m (LoadServerDependencies r s m) ()
loadServer = serverComponent "contract-load-server" worker \LoadServerDependencies{..} -> do
  connector <- acceptSomeConnectorTraced loadSource
  pure WorkerDependencies{..}

data WorkerDependencies r s m = forall n. WorkerDependencies
  { batchSize :: Nat ('S n)
  , contractStore :: ContractStore m
  , connector :: SomeServerConnectorTraced MarloweLoadServer r s m
  }

worker
  :: forall r s m.
    ( MonadUnliftIO m
    , MonadInjectEvent r LoadServerSelector s m
    , HasSpanContext r
    )
  => Component m (WorkerDependencies r s m) ()
worker = component_ "contract-load-worker" \WorkerDependencies{..} -> case connector of
  SomeConnectorTraced inj connector' -> do
    connection@ConnectionTraced{..} <- openConnectionTraced connector'
    let args = (simpleNewEventArgs LoadContract) { newEventParent = Just openRef }
    withEventArgs args \ev ->
      bracketOnError (createContractStagingArea contractStore) discard \ContractStagingArea{..} -> do
        mContract <- runConnectionTraced inj connection
          $ pullContract batchSize stageContract
          $ void flush
        case mContract of
          Nothing -> discard
          Just contract -> do
            addField ev contract
            void commit
