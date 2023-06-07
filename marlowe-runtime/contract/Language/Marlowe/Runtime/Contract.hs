{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.Contract where

import Colog (WithLog)
import qualified Colog as C
import Control.Arrow (returnA)
import Control.Concurrent.Component (Component)
import Control.Concurrent.Component.Probes
import Language.Marlowe.Protocol.Load.Server (MarloweLoadServer)
import Language.Marlowe.Runtime.Contract.Api (ContractRequest)
import Language.Marlowe.Runtime.Contract.LoadServer
import Language.Marlowe.Runtime.Contract.QueryServer
import Language.Marlowe.Runtime.Contract.Store
import Network.Protocol.Connection (ConnectionSource)
import Network.Protocol.Query.Server (QueryServer)
import Network.TypedProtocol
import UnliftIO (MonadUnliftIO)

data ContractDependencies m = forall n. ContractDependencies
  { batchSize :: Nat ('S n)
  , contractStore :: ContractStore m
  , loadSource :: ConnectionSource MarloweLoadServer m
  , querySource :: ConnectionSource (QueryServer ContractRequest) m
  }

contract :: (MonadUnliftIO m, WithLog env C.Message m) => Component m (ContractDependencies m) Probes
contract = proc deps -> do
  loadServer -< case deps of
    ContractDependencies{..} -> LoadServerDependencies{..}
  queryServer -< case deps of
    ContractDependencies{..} -> QueryServerDependencies{..}
  returnA -< Probes
    { liveness = pure True
    , readiness = pure True
    , startup = pure True
    }
