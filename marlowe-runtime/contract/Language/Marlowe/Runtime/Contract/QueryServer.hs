{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.Contract.QueryServer
  where

import Control.Concurrent.Component
import Control.Monad.Event.Class
import Language.Marlowe.Runtime.Contract.Api hiding (getContract, merkleizeInputs)
import Language.Marlowe.Runtime.Contract.Store (ContractStore(..))
import Network.Protocol.Connection
import Network.Protocol.Driver.Trace (HasSpanContext, runSomeConnectorTraced)
import Network.Protocol.Query.Server
import UnliftIO (MonadUnliftIO, concurrently)

data QueryServerDependencies r s m = QueryServerDependencies
  { contractStore :: ContractStore m
  , querySource :: SomeConnectionSourceTraced (QueryServer ContractRequest) r s m
  }

queryServer
  :: (MonadUnliftIO m, HasSpanContext r, MonadEvent r s m)
  => Component m (QueryServerDependencies r s m) ()
queryServer = serverComponent (component_ worker) \QueryServerDependencies{..} -> do
  connector <- acceptSomeConnectorTraced querySource
  pure WorkerDependencies{..}

data WorkerDependencies r s m = WorkerDependencies
  { contractStore :: ContractStore m
  , connector :: SomeServerConnectorTraced (QueryServer ContractRequest) r s m
  }

worker
  :: forall r s m
   . (MonadUnliftIO m, HasSpanContext r, MonadEvent r s m)
  => WorkerDependencies r s m
  -> m ()
worker WorkerDependencies{..} = runSomeConnectorTraced connector server
  where
    server = respond concurrently \case
      GetContract hash -> getContract contractStore hash
      MerkleizeInputs hash state interval inputs -> merkleizeInputs contractStore hash state interval inputs
