module Language.Marlowe.Runtime.Contract.Store.Memory
  where

import Cardano.Api (hashScriptData)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Conc (throwSTM)
import GHC.IO (mkUserError)
import Language.Marlowe.Core.V1.Plate (Extract(extractAll))
import Language.Marlowe.Core.V1.Semantics.Types (Case(..), Contract)
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoDatumHash, toCardanoScriptData)
import Language.Marlowe.Runtime.ChainSync.Api (DatumHash(DatumHash), toDatum)
import Language.Marlowe.Runtime.Contract.Api
import Language.Marlowe.Runtime.Contract.Store
import qualified Plutus.V2.Ledger.Api as PV2
import UnliftIO (STM, TVar, modifyTVar, newTVar, readTVar, writeTVar)

-- | An in-memory implementation of ContractStore for use in testing and as a
-- model implementation.
createContractStoreInMemory :: STM (ContractStore STM)
createContractStoreInMemory = do
  store <- newTVar mempty
  pure ContractStore
    { createContractStagingArea = createContractStagingAreaInMemory store
    , getContract = getContract store
    }
  where
    createContractStagingAreaInMemory store = do
      stagingArea <- newTVar mempty
      buffer <- newTVar mempty
      open <- newTVar True
      let
        close = writeTVar open False
        whenOpen m = readTVar open >>= \case
          False -> throwSTM $ mkUserError "Contract staging area is no longer open"
          True -> m
        flush = whenOpen do
          buffered <- readTVar buffer
          staged <- readTVar stagingArea
          writeTVar stagingArea $ Map.union staged buffered
          writeTVar buffer mempty
          pure $ Map.keysSet $ Map.difference staged buffered
      pure ContractStagingArea
        { stageContract = \contract -> whenOpen do
            let hash = fromCardanoDatumHash $ hashScriptData $ toCardanoScriptData $ toDatum contract
            modifyTVar buffer $ Map.insert hash contract
            pure hash
        , flush
        , commit = whenOpen do
            _ <- flush
            staged <- readTVar stagingArea
            stored <- readTVar store
            writeTVar store $ Map.union stored staged
            writeTVar stagingArea mempty
            close
            pure $ Map.keysSet $ Map.difference stored staged
        , discard = whenOpen do
            writeTVar buffer mempty
            writeTVar stagingArea mempty
            close
        }

    getContract :: TVar (Map DatumHash Contract) -> DatumHash -> STM (Maybe ContractWithAdjacency)
    getContract store = runMaybeT . go
      where
        go hash = do
          contract <- MaybeT $ Map.lookup hash <$> readTVar store
          adjacentContracts <- fmap Set.fromList $ traverse go $ Set.toList $ computeAdjacency contract
          pure ContractWithAdjacency
            { adjacency = Set.map contractHash adjacentContracts
            , closure = Set.insert hash $ foldMap closure adjacentContracts
            , contractHash = hash
            , ..
            }

computeAdjacency :: Contract -> Set DatumHash
computeAdjacency = foldMap getHash . extractAll
  where
  getHash :: Case Contract -> Set DatumHash
  getHash = \case
    MerkleizedCase _ hash -> Set.singleton $ DatumHash $ PV2.fromBuiltin hash
    _ -> mempty
