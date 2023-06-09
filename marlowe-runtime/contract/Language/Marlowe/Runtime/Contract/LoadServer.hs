{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.Contract.LoadServer where

import Colog (WithLog)
import qualified Colog as C
import Control.Concurrent.Component
import Data.Functor (void)
import Language.Marlowe.Protocol.Load.Server
import Language.Marlowe.Runtime.Contract.Store (ContractStagingArea(..), ContractStore(..))
import Network.Protocol.Connection
import Network.TypedProtocol
import UnliftIO (MonadUnliftIO, bracketOnError)

data LoadServerDependencies m = forall n. LoadServerDependencies
  { batchSize :: Nat ('S n)
  , contractStore :: ContractStore m
  , loadSource :: ConnectionSource MarloweLoadServer m
  }

loadServer
  :: (MonadUnliftIO m, WithLog env C.Message m)
  => Component m (LoadServerDependencies m) ()
loadServer = serverComponent "contract-load-server" worker \LoadServerDependencies{..} -> do
  connector <- acceptConnector loadSource
  pure WorkerDependencies{..}

data WorkerDependencies m = forall n. WorkerDependencies
  { batchSize :: Nat ('S n)
  , contractStore :: ContractStore m
  , connector :: Connector MarloweLoadServer m
  }

worker :: forall env m. (MonadUnliftIO m, WithLog env C.Message m) => Component m (WorkerDependencies m) ()
worker = component_ "contract-load-worker" \WorkerDependencies{..} ->
  bracketOnError (createContractStagingArea contractStore) discard \ContractStagingArea{..} -> do
    mContract <- runConnector connector
      $ pullContract batchSize stageContract
      $ void flush
    case mContract of
      Nothing -> discard
      Just _ -> void commit
