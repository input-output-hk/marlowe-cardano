{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.Contract.GarbageCollector (
  GarbageCollectorDependencies (..),
  garbageCollector,
) where

import Colog (Message, WithLog)
import Control.Concurrent.Component (Component, component_)
import Control.Monad (when)
import Data.Foldable (Foldable (..))
import qualified Data.Marlowe.LiveContracts as LiveContracts
import qualified Data.Set as Set
import Language.Marlowe.Core.V1.Plate (Extract (..))
import Language.Marlowe.Core.V1.Semantics.Types (Case (..), Contract)
import Language.Marlowe.Protocol.BulkSync.Client
import Language.Marlowe.Runtime.ChainSync.Api (
  BlockHeader (..),
  ChainSyncQuery (..),
  DatumHash (..),
 )
import Language.Marlowe.Runtime.Contract.Api (ContractWithAdjacency (..))
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
        pure $ idle $ LiveContracts.create securityParameter
      where
        idle = SendMsgRequestNext 255 . next True

        next gcOnWait liveContracts =
          ClientStNext
            { recvMsgRollForward = rollForward liveContracts
            , recvMsgRollBackward = rollBackward liveContracts
            , recvMsgWait = wait gcOnWait liveContracts
            }

        rollForward liveContracts blocks tip =
          pure $ idle $ foldl' (flip $ LiveContracts.rollForward $ blockNo tip) liveContracts blocks

        rollBackward liveContracts point tip =
          pure $ idle $ LiveContracts.rollBackward (blockNo <$> point) (blockNo <$> tip) liveContracts

        wait gcOnWait liveContracts = do
          when gcOnWait do
            storeHashes <- getHashes contractStore
            let getClosure = fmap (foldMap closure) . getContract contractStore
            -- Collect the garbage collection roots of the live contracts.
            let getCaseRoots = \case
                  MerkleizedCase _ hash -> Set.singleton $ DatumHash $ fromBuiltin hash
                  _ -> mempty
                getContractRoots = foldMap getCaseRoots . extractAll @(Case Contract)
                liveRoots =
                  -- Only consider roots also found in the store to avoid wasteful file system access
                  Set.intersection storeHashes $
                    foldMap getContractRoots $
                      LiveContracts.allContracts liveContracts

                foldMapM f = foldl' (\mb a -> (<>) <$> mb <*> f a) $ pure mempty
            -- Get all live store hashes by taking the union of the closures of the live roots.
            liveHashes <- foldMapM getClosure liveRoots
            -- All store hashes that are not alive are garbage.
            let garbage = Set.difference storeHashes liveHashes
            deleteContracts contractStore garbage
          poll liveContracts

        poll liveContracts = do
          threadDelay 10_000_000 -- ten seconds
          pure $ SendMsgPoll $ next False liveContracts
