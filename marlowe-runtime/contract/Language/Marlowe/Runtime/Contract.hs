{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.Contract
  where

import Control.Arrow (returnA)
import Control.Concurrent.Component (Component)
import Control.Concurrent.Component.Probes
import Control.Monad.Event.Class (Inject, MonadEvent)
import Language.Marlowe.Protocol.Load.Server (MarloweLoadServer)
import Language.Marlowe.Runtime.Contract.LoadServer
import Language.Marlowe.Runtime.Contract.Store
import Network.Protocol.Connection (SomeConnectionSourceTraced)
import Network.Protocol.Peer.Trace (HasSpanContext)
import Network.TypedProtocol
import UnliftIO (MonadUnliftIO)

data ContractDependencies r s m = forall n. ContractDependencies
  { batchSize :: Nat ('S n)
  , contractStore :: ContractStore m
  , loadSource :: SomeConnectionSourceTraced MarloweLoadServer r s m
  }

contract
  ::
    ( MonadUnliftIO m
    , MonadEvent r s m
    , Inject LoadServerSelector s
    , HasSpanContext r
    )
  => Component m (ContractDependencies r s m) Probes
contract = proc deps -> do
  loadServer -< case deps of
    ContractDependencies{..} -> LoadServerDependencies{..}
  returnA -< Probes
    { liveness = pure True
    , readiness = pure True
    , startup = pure True
    }
