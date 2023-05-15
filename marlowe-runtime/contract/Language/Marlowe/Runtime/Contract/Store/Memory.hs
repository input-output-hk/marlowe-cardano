module Language.Marlowe.Runtime.Contract.Store.Memory
  where

import Cardano.Api (hashScriptData)
import qualified Data.Map as Map
import GHC.Conc (throwSTM)
import GHC.IO (mkUserError)
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoDatumHash, toCardanoScriptData)
import Language.Marlowe.Runtime.ChainSync.Api (toDatum)
import Language.Marlowe.Runtime.Contract.Store
import UnliftIO (STM, modifyTVar, newTVar, readTVar, writeTVar)

-- | An in-memory implementation of ContractStore for use in testing and as a
-- model implementation.
createContractStoreInMemory :: STM (ContractStore STM)
createContractStoreInMemory = do
  store <- newTVar mempty
  pure ContractStore
    { createContractStagingArea = createContractStagingAreaInMemory store
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
