-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

-- | Merkleize Marlowe contracts.
module Language.Marlowe.Core.V1.Merkle (
  -- * Types
  Continuations,
  MerkleizedContract (..),

  -- * Merkleization
  deepMerkleize,
  merkleizedContract,
  shallowMerkleize,

  -- * Demerkleization
  deepDemerkleize,
  demerkleizeContract,
  shallowDemerkleize,

  -- * Inputs
  merkleizeInput,
  merkleizeInputs,
) where

import Control.Monad (foldM)
import Control.Monad.Except (MonadError, catchError, throwError)
import Control.Monad.Fix (fix)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.String (IsString (..))
import Language.Marlowe.Core.V1.Semantics (
  ApplyResult (..),
  ReduceResult (..),
  TransactionInput (..),
  TransactionOutput (..),
  applyInput,
  computeTransaction,
  fixInterval,
  reduceContractUntilQuiescent,
 )
import Language.Marlowe.Core.V1.Semantics.Types (
  Case (..),
  Contract (..),
  Input (..),
  IntervalResult (..),
  State,
  TimeInterval,
 )
import Plutus.Script.Utils.Scripts (dataHash)
import Plutus.V1.Ledger.Api (DatumHash (..), toBuiltinData)

import qualified Data.Map.Strict as M (Map, lookup, singleton)

-- | Hashed continuations of a Marlowe contract.
type Continuations = M.Map DatumHash Contract

-- | A merkleized contract.
data MerkleizedContract = MerkleizedContract
  { mcContract :: Contract
  , mcContinuations :: Continuations
  }
  deriving (Show)

-- | Bundle the merkleized contract.
merkleizedContract
  :: Writer Continuations Contract
  -> MerkleizedContract
merkleizedContract = uncurry MerkleizedContract . runWriter

-- | Unbundled the merkleized contract.
demerkleizeContract
  :: Continuations
  -> ReaderT Continuations m Contract
  -> m Contract
demerkleizeContract = flip runReaderT

-- | Merkleize any top-level case statements in a contract.
shallowMerkleize
  :: Contract
  -- ^ The contract.
  -> Writer Continuations Contract
  -- ^ Action for the merkleized contract.
shallowMerkleize = merkleize' pure

-- | Merkleize all case statements in a contract.
deepMerkleize
  :: Contract
  -- ^ The contract.
  -> Writer Continuations Contract
  -- ^ Action for the merkleized contract.
deepMerkleize = fix merkleize'

-- | Merkleize selected case statements in a contract.
merkleize'
  :: (Contract -> Writer Continuations Contract)
  -- ^ Action to continue merkleization.
  -> Contract
  -- ^ The contract.
  -> Writer Continuations Contract
  -- ^ Action for merkleizing the selected case statements.
merkleize' _ Close = pure Close
merkleize' f (Pay accountId payee token value contract) = Pay accountId payee token value <$> merkleize' f contract
merkleize' f (If observation thenContract elseContract) = If observation <$> merkleize' f thenContract <*> merkleize' f elseContract
merkleize' f (Let valueId value contract) = Let valueId value <$> merkleize' f contract
merkleize' f (Assert observation contract) = Assert observation <$> merkleize' f contract
merkleize' f (When cases timeout contract) = When <$> mapM merkleizeCase cases <*> pure timeout <*> merkleize' f contract
  where
    merkleizeCase c@(Case _ Close) = pure c
    merkleizeCase (Case action continuation) =
      do
        continuation' <- f continuation
        let mh = dataHash $ toBuiltinData continuation'
        tell $ DatumHash mh `M.singleton` continuation'
        pure $ MerkleizedCase action mh
    merkleizeCase mc = pure mc

-- | Demerkleize any top-level case statements in a contract.
shallowDemerkleize
  :: (IsString e)
  => (MonadError e m)
  => Contract
  -- ^ The contract.
  -> ReaderT Continuations m Contract
  -- ^ Action for the demerkleized contract.
shallowDemerkleize = demerkleize' pure

-- | Demerkleize all case statements in a contract.
deepDemerkleize
  :: (IsString e)
  => (MonadError e m)
  => Contract
  -- ^ The contract.
  -> ReaderT Continuations m Contract
  -- ^ Action for the demerkleized contract.
deepDemerkleize = fix demerkleize'

-- | Demerkleize selected case statements in a contract.
demerkleize'
  :: (IsString e)
  => (MonadError e m)
  => (Contract -> ReaderT Continuations m Contract)
  -- ^ Action to continue demerkleization.
  -> Contract
  -- ^ The contract.
  -> ReaderT Continuations m Contract
  -- ^ Action for demerkleized the selected case statements.
demerkleize' _ Close = pure Close
demerkleize' f (Pay accountId payee token value contract) = Pay accountId payee token value <$> demerkleize' f contract
demerkleize' f (If observation thenContract elseContract) = If observation <$> demerkleize' f thenContract <*> demerkleize' f elseContract
demerkleize' f (Let valueId value contract) = Let valueId value <$> demerkleize' f contract
demerkleize' f (Assert observation contract) = Assert observation <$> demerkleize' f contract
demerkleize' f (When cases timeout contract) = When <$> mapM demerkleizeCase cases <*> pure timeout <*> demerkleize' f contract
  where
    demerkleizeCase (MerkleizedCase action mh) =
      do
        continuation' <- M.lookup (DatumHash mh) <$> ask
        continuation <- maybe (throwError . fromString $ "Missing continuation for hash " <> show mh <> ".") f continuation'
        pure $ Case action continuation
    demerkleizeCase c = pure c

-- | Merkleize whatever inputs need merkleization before application to a contract.
merkleizeInputs
  :: (IsString e)
  => (MonadError e m)
  => MerkleizedContract
  -- ^ The contract information.
  -> State
  -- ^ The prior state of the contract.
  -> TransactionInput
  -- ^ The input to the contract.
  -> m TransactionInput
  -- ^ Action for the merkleized input to the contract.
merkleizeInputs MerkleizedContract{..} state TransactionInput{..} =
  TransactionInput txInterval
    . snd
    <$> foldM (merkleizeInput txInterval mcContinuations) ((state, mcContract), []) txInputs

-- | Merkleize an input if needed before application to a contract.
merkleizeInput
  :: (IsString e)
  => (MonadError e m)
  => TimeInterval
  -- ^ The validity interval.
  -> Continuations
  -- ^ The available continuations.
  -> ((State, Contract), [Input])
  -- ^ The current state and contract, along with the prior inputs.
  -> Input
  -- ^ The input.
  -> m ((State, Contract), [Input])
  -- ^ The new state and contract, along with the prior inputs.
merkleizeInput txInterval continuations ((state, contract), inputs) input =
  do
    -- Apply input as it is.
    let attempt = computeTransaction (TransactionInput txInterval [input]) state contract
        result =
          case attempt of
            TransactionOutput{..} -> pure ((txOutState, txOutContract), inputs ++ [input])
            Error e -> throwError . fromString $ show e
    -- Try applying the input to the demerkleized contract.
    contract' <- shallowDemerkleize contract `runReaderT` continuations
    let attempt' = computeTransaction' txInterval input state contract'
        result' =
          case attempt' of
            Just (txOutState, txOutContract) ->
              do
                input' <-
                  case input of
                    NormalInput content ->
                      pure $
                        MerkleizedInput
                          content
                          (dataHash $ toBuiltinData txOutContract)
                          txOutContract
                    MerkleizedInput{} -> throwError $ fromString "Unexpected mekleized input."
                pure ((txOutState, txOutContract), inputs ++ [input'])
            Nothing -> throwError $ fromString "Failed to merkleize input."
    -- See if either as-is or merkleized input is accepted.
    result `catchError` const result'

-- | Compute one step in a transaction without reducing the quiescent state after the input is applied.
computeTransaction'
  :: TimeInterval
  -- ^ The validity interval.
  -> Input
  -- ^ The input to the contract.
  -> State
  -- ^ The current state of the contract.
  -> Contract
  -- ^ The current contract.
  -> Maybe (State, Contract)
  -- ^ The new state and contract, if the input could be applied.
computeTransaction' txInterval input state contract =
  case fixInterval txInterval state of
    IntervalTrimmed env fixState ->
      case reduceContractUntilQuiescent env fixState contract of
        ContractQuiescent _ _ _ curState cont ->
          case applyInput env curState input cont of
            Applied _ newState cont' -> Just (newState, cont')
            _ -> Nothing
        _ -> Nothing
    _ -> Nothing
