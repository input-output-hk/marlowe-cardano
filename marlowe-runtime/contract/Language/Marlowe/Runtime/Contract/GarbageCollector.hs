{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.Contract.GarbageCollector (
  GarbageCollectorDependencies (..),
  garbageCollector,
) where

import Colog (Message, WithLog)
import Control.Concurrent.Component (Component, component_)
import Control.Monad (unless)
import Data.Foldable (Foldable (..))
import qualified Data.Marlowe.LiveContracts as LiveContracts
import Data.Set (Set)
import qualified Data.Set as Set
import Language.Marlowe.Core.V1.Plate (Extract (..))
import Language.Marlowe.Core.V1.Semantics.Types (Case (..), Contract)
import Language.Marlowe.Protocol.BulkSync.Client
import Language.Marlowe.Runtime.ChainSync.Api (
  BlockHeader (..),
  ChainSyncQuery (..),
  DatumHash (..),
 )
import Language.Marlowe.Runtime.Contract.Store (ContractStore (..))
import Network.Protocol.Connection (Connector, runConnector)
import Network.Protocol.Query.Client (QueryClient, request)
import PlutusLedgerApi.V1 (fromBuiltin)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Concurrent (threadDelay)

data GarbageCollectorDependencies m = GarbageCollectorDependencies
  { contractStore :: ContractStore m
  , marloweBulkSyncConnector :: Connector MarloweBulkSyncClient m
  , chainSyncQueryConnector :: Connector (QueryClient ChainSyncQuery) m
  }

garbageCollector :: (WithLog env Message m, MonadUnliftIO m) => Component m (GarbageCollectorDependencies m) ()
garbageCollector = component_ "garbage-collector" run
  where
    run GarbageCollectorDependencies{..} =
      runConnector marloweBulkSyncConnector $ MarloweBulkSyncClient do
        securityParameter <- runConnector chainSyncQueryConnector $ request GetSecurityParameter
        idle $ LiveContracts.create securityParameter
      where
        idle liveContracts = do
          pure $ SendMsgRequestNext 255 $ next False liveContracts

        next isPolling liveContracts =
          ClientStNext
            { recvMsgRollForward = rollForward liveContracts
            , recvMsgRollBackward = rollBackward liveContracts
            , recvMsgWait = wait isPolling liveContracts
            }

        rollForward liveContracts blocks tip = do
          idle $ foldl' (flip $ LiveContracts.rollForward (blockNo tip) getContractRoots) liveContracts blocks

        rollBackward liveContracts point tip =
          idle $ LiveContracts.rollBackward (blockNo <$> point) (blockNo <$> tip) liveContracts

        wait isPolling liveContracts = do
          unless isPolling do
            let liveRoots = fold $ LiveContracts.allContracts liveContracts
            setGCRoots contractStore liveRoots
          poll liveContracts

        poll liveContracts = do
          threadDelay 10_000_000 -- ten seconds
          pure $ SendMsgPoll $ next True liveContracts

getContractRoots :: Contract -> Set DatumHash
getContractRoots = foldMap getCaseRoots . extractAll @(Case Contract)

getCaseRoots :: Case a -> Set DatumHash
getCaseRoots = \case
  MerkleizedCase _ hash -> Set.singleton $ DatumHash $ fromBuiltin hash
  _ -> mempty
