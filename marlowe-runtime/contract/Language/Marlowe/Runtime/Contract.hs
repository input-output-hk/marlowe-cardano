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
import Control.Monad.Event.Class (Inject, MonadEvent)
import Language.Marlowe.Protocol.Load.Server (MarloweLoadServer)
import Language.Marlowe.Runtime.Contract.Api (ContractRequest)
import Language.Marlowe.Runtime.Contract.LoadServer
import Language.Marlowe.Runtime.Contract.QueryServer
import Language.Marlowe.Runtime.Contract.Store
import Network.Protocol.Connection (SomeConnectionSourceTraced)
import Network.Protocol.Driver.Trace (HasSpanContext)
import Network.Protocol.Query.Server (QueryServer)
import Network.TypedProtocol
import UnliftIO (MonadUnliftIO)

data ContractDependencies r s m = forall n. ContractDependencies
  { batchSize :: Nat ('S n)
  , contractStore :: ContractStore m
  , loadSource :: SomeConnectionSourceTraced MarloweLoadServer r s m
  , querySource :: SomeConnectionSourceTraced (QueryServer ContractRequest) r s m
  }

contract
  ::
    ( MonadUnliftIO m
    , MonadEvent r s m
    , Inject LoadServerSelector s
    , HasSpanContext r
    , WithLog env C.Message m
    )
  => Component m (ContractDependencies r s m) Probes
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
