{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.Contract.QueryServer where

import Colog (Message, WithLog)
import Control.Concurrent.Component
import Language.Marlowe.Runtime.Contract.Api hiding (getContract, merkleizeInputs)
import Language.Marlowe.Runtime.Contract.Store (ContractStore(..))
import Network.Protocol.Connection
import Network.Protocol.Query.Server
import UnliftIO (MonadUnliftIO, concurrently)

data QueryServerDependencies m = QueryServerDependencies
  { contractStore :: ContractStore m
  , querySource :: ConnectionSource (QueryServer ContractRequest) m
  }

queryServer
  :: (MonadUnliftIO m, WithLog env Message m)
  => Component m (QueryServerDependencies m) ()
queryServer = serverComponent "contract-query-server" (component_ "contract-query-worker" worker) \QueryServerDependencies{..} -> do
  connector <- acceptConnector querySource
  pure WorkerDependencies{..}

data WorkerDependencies m = WorkerDependencies
  { contractStore :: ContractStore m
  , connector :: Connector (QueryServer ContractRequest) m
  }

worker :: forall m. MonadUnliftIO m => WorkerDependencies m -> m ()
worker WorkerDependencies{..} = runConnector connector server
  where
    server = respond concurrently \case
      GetContract hash -> getContract contractStore hash
      MerkleizeInputs hash state input -> merkleizeInputs contractStore hash state input
