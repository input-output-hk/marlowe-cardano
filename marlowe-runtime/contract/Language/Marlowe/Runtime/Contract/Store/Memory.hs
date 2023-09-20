module Language.Marlowe.Runtime.Contract.Store.Memory where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (except, runExceptT, throwE)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Conc (throwSTM)
import GHC.IO (mkUserError)
import Language.Marlowe.Core.V1.Plate (Extract (extractAll))
import Language.Marlowe.Core.V1.Semantics (
  ApplyAction (..),
  ReduceResult (..),
  TransactionInput (..),
  applyAction,
  fixInterval,
  reduceContractUntilQuiescent,
 )
import Language.Marlowe.Core.V1.Semantics.Types (
  Case (..),
  Contract (..),
  Environment,
  Input (..),
  InputContent,
  IntervalResult (..),
  State,
  getAction,
 )
import Language.Marlowe.Runtime.ChainSync.Api (DatumHash (..))
import Language.Marlowe.Runtime.Contract.Api hiding (getContract)
import Language.Marlowe.Runtime.Contract.Store
import Language.Marlowe.Util (dataHash)
import qualified PlutusLedgerApi.V2 as PV2
import UnliftIO (STM, TVar, modifyTVar, newTVar, readTVar, writeTVar)

-- | An in-memory implementation of ContractStore for use in testing and as a
-- model implementation.
createContractStoreInMemory :: STM (ContractStore STM)
createContractStoreInMemory = do
  store <- newTVar mempty
  pure
    ContractStore
      { createContractStagingArea = createContractStagingAreaInMemory store
      , getContract = getContract store
      , merkleizeInputs =
          merkleizeInputsDefault $
            (fmap . fmap) (\ContractWithAdjacency{..} -> contract)
              . getContract store
      }
  where
    createContractStagingAreaInMemory store = do
      stagingArea <- newTVar mempty
      buffer <- newTVar mempty
      open <- newTVar True
      let close = writeTVar open False
          whenOpen m =
            readTVar open >>= \case
              False -> throwSTM $ mkUserError "Contract staging area is no longer open"
              True -> m
          flush = whenOpen do
            buffered <- readTVar buffer
            staged <- readTVar stagingArea
            writeTVar stagingArea $ Map.union staged buffered
            writeTVar buffer mempty
            pure $ Map.keysSet $ Map.difference staged buffered
      pure
        ContractStagingArea
          { stageContract = \contract -> whenOpen do
              let hash = DatumHash $ PV2.fromBuiltin $ dataHash contract
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
          , doesContractExist = \hash -> whenOpen do
              buffered <- readTVar buffer
              if Map.member hash buffered
                then pure True
                else do
                  staged <- readTVar stagingArea
                  if Map.member hash staged
                    then pure True
                    else do
                      stored <- readTVar store
                      pure $ Map.member hash stored
          }

    getContract :: TVar (Map DatumHash Contract) -> DatumHash -> STM (Maybe ContractWithAdjacency)
    getContract store = runMaybeT . go
      where
        go hash = do
          contract <- MaybeT $ Map.lookup hash <$> readTVar store
          adjacentContracts <- fmap Set.fromList $ traverse go $ Set.toList $ computeAdjacency contract
          pure
            ContractWithAdjacency
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

merkleizeInputsDefault
  :: (Monad m)
  => (DatumHash -> m (Maybe Contract))
  -> Contract
  -> State
  -> TransactionInput
  -> m (Either MerkleizeInputsError TransactionInput)
merkleizeInputsDefault getContract' initialContract initialState TransactionInput{..} = runExceptT do
  case fixInterval txInterval initialState of
    IntervalTrimmed env state' -> go env state' txInputs initialContract []
    IntervalError err -> throwE $ MerkleizeInputsIntervalError err
  where
    getContractExcept hash =
      lift (getContract' hash) >>= \case
        Nothing -> throwE $ MerkleizeInputsContractNotFound hash
        Just c -> pure c

    go env state inputs contract acc = case inputs of
      [] -> pure $ TransactionInput txInterval $ reverse acc
      input : inputs' -> case reduceContractUntilQuiescent env state contract of
        ContractQuiescent _ _ _ state' contract' -> do
          (content, continuation, state'') <- except $ applyInput' state' env input contract'
          case continuation of
            Left contract'' ->
              go env state'' inputs' contract'' (NormalInput content : acc)
            Right hash -> do
              contract'' <- getContractExcept $ DatumHash $ PV2.fromBuiltin hash
              go env state'' inputs' contract'' (MerkleizedInput content hash contract'' : acc)
        RRAmbiguousTimeIntervalError -> throwE $ MerkleizeInputsReduceAmbiguousInterval input

applyInput'
  :: State
  -> Environment
  -> Input
  -> Contract
  -> Either MerkleizeInputsError (InputContent, Either Contract PV2.BuiltinByteString, State)
applyInput' state env input = \case
  When cases _ _ -> applyInputCases state env input cases
  _ -> Left $ MerkleizeInputsApplyNoMatch input

applyInputCases
  :: State
  -> Environment
  -> Input
  -> [Case Contract]
  -> Either MerkleizeInputsError (InputContent, Either Contract PV2.BuiltinByteString, State)
applyInputCases state env input = \case
  [] -> Left $ MerkleizeInputsApplyNoMatch input
  c : cs ->
    let action = getAction c
        continuation = case c of
          Case _ contract -> Left contract
          MerkleizedCase _ hash -> Right hash
     in case input of
          NormalInput content -> case applyAction env state content action of
            AppliedAction _ state' -> pure (content, continuation, state')
            NotAppliedAction -> applyInputCases state env input cs
          MerkleizedInput content hash' contract -> case continuation of
            Right hash
              | hash == hash' && dataHash contract == hash -> case applyAction env state content action of
                  AppliedAction _ state' -> pure (content, continuation, state')
                  NotAppliedAction -> applyInputCases state env input cs
            _ -> Left $ MerkleizeInputsApplyNoMatch input
