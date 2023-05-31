module Language.Marlowe.Runtime.Contract.Store.Memory
  where

import Cardano.Api (hashScriptData)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (except, runExceptT, throwE)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Conc (throwSTM)
import GHC.IO (mkUserError)
import Language.Marlowe.Core.V1.Plate (Extract(extractAll))
import Language.Marlowe.Core.V1.Semantics
  (ApplyAction(..), ReduceResult(..), applyAction, fixInterval, reduceContractUntilQuiescent)
import Language.Marlowe.Core.V1.Semantics.Types
  (Case(..), Contract(..), Environment, Input(..), InputContent, IntervalResult(..), State, TimeInterval, getAction)
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoDatumHash, toCardanoScriptData)
import Language.Marlowe.Runtime.ChainSync.Api (DatumHash(..), toDatum)
import Language.Marlowe.Runtime.Contract.Api hiding (getContract)
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
    , getMerkleizedInputs =
        getMerkleizedInputsDefault
          $ (fmap . fmap) (\ContractWithAdjacency {..} -> contract)
          . getContract store
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

toPlutusDatumHash :: DatumHash -> PV2.BuiltinByteString
toPlutusDatumHash = PV2.toBuiltin . unDatumHash

getMerkleizedInputsDefault
  :: Monad m
  => (DatumHash -> m (Maybe Contract))
  -> DatumHash
  -> State
  -> TimeInterval
  -> [InputContent]
  -> m (Either GetMerkleizedInputsError [Input])
getMerkleizedInputsDefault getContract' = \hash state interval inputs -> runExceptT do
  case fixInterval interval state of
    IntervalTrimmed env state' -> do
      contract <- getContractExcept hash
      go env state' inputs contract []
    IntervalError err -> throwE $ GetMerkleizedInputsIntervalError err
  where
    getContractExcept hash = lift (getContract' hash) >>= \case
      Nothing -> throwE $ GetMerkleizedInputsContractNotFound hash
      Just c -> pure c

    go env state inputs contract acc = case inputs of
      [] -> pure $ reverse acc
      input : inputs' -> case reduceContractUntilQuiescent env state contract of
        ContractQuiescent _ _ _ state' contract' -> do
          (continuation, state'') <- except $ applyInputContent state' env input contract'
          case continuation of
            Left contract'' ->
              go env state'' inputs' contract'' (NormalInput input : acc)
            Right hash -> do
              contract'' <- getContractExcept $ DatumHash $ PV2.fromBuiltin hash
              go env state'' inputs' contract'' (MerkleizedInput input hash contract'' : acc)
        RRAmbiguousTimeIntervalError -> throwE $ GetMerkleizedInputsReduceAmbiguousInterval input

applyInputContent
  :: State
  -> Environment
  -> InputContent
  -> Contract
  -> Either GetMerkleizedInputsError (Either Contract PV2.BuiltinByteString, State)
applyInputContent state env input = \case
  When cases _ _ -> applyInputContentCases state env input cases
  _ -> Left $ GetMerkleizedInputsApplyNoMatch input

applyInputContentCases
  :: State
  -> Environment
  -> InputContent
  -> [Case Contract]
  -> Either GetMerkleizedInputsError (Either Contract PV2.BuiltinByteString, State)
applyInputContentCases state env input = \case
  [] -> Left $ GetMerkleizedInputsApplyNoMatch input
  c : cs ->
    let
      action = getAction c
      continuation = case c of
        Case _ contract -> Left contract
        MerkleizedCase _ hash -> Right hash
    in
      case applyAction env state input action of
        AppliedAction _ state' -> pure (continuation, state')
        NotAppliedAction -> applyInputContentCases state env input cs
