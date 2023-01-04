-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Merkelize a contract and/or its input.
--
-----------------------------------------------------------------------------


{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}


module Spec.Marlowe.Semantics.Merkle
  ( -- * Types
    Continuations
    -- * Merkleization
  , deepMerkleize
  , shallowMerkleize
    -- * Inputs
  , merkleizeInputs
  ) where


import Control.Monad (foldM)
import Control.Monad.Fix (fix)
import Control.Monad.Writer (Writer, runWriter, tell)
import Language.Marlowe.Core.V1.Semantics
  (ApplyResult(..), ReduceResult(..), TransactionInput(..), applyInput, fixInterval, reduceContractUntilQuiescent)
import Language.Marlowe.Core.V1.Semantics.Types
  (Case(..), Contract(..), Input(..), IntervalResult(..), State, TimeInterval)
import Plutus.Script.Utils.Scripts (dataHash)
import Plutus.V1.Ledger.Api (DatumHash(..), toBuiltinData)

import qualified Data.Map.Strict as M (Map, singleton, (!))


-- | Continuations for contracts.
type Continuations = M.Map DatumHash Contract


-- | Merkleize any top-level case statements in a contract.
shallowMerkleize :: Contract                   -- ^ The contract.
                 -> (Contract, Continuations)  -- ^ The merkleized contract.
shallowMerkleize = runWriter . merkleize' pure


-- | Merkleize all case statements in a contract.
deepMerkleize :: Contract                   -- ^ The contract.
              -> (Contract, Continuations)  -- ^ The merkleized contract.
deepMerkleize = runWriter . fix merkleize'


-- | Merkleize selected case statements in a contract.
merkleize' :: (Contract -> Writer Continuations Contract)  -- ^ Action to continue merkleization.
           -> Contract                                     -- ^ The contract.
           -> Writer Continuations Contract                -- ^ Action for merkleizing the selected case statements.
merkleize' _ Close = pure Close
merkleize' f (Pay accountId payee token value contract) = Pay accountId payee token value <$> merkleize' f contract
merkleize' f (If observation thenContract elseContract) = If observation <$> merkleize' f thenContract <*> merkleize' f elseContract
merkleize' f (Let valueId value contract) = Let valueId value <$> merkleize' f contract
merkleize' f (Assert observation contract) = Assert observation <$> merkleize' f contract
merkleize' f (When cases timeout contract) = When <$> mapM merkleizeCase cases <*> pure timeout <*> merkleize' f contract
  where
    merkleizeCase (Case action continuation) =
      do
        continuation' <- f continuation
        let
          mh = dataHash $ toBuiltinData continuation'
        tell $ DatumHash mh `M.singleton` continuation'
        pure $ MerkleizedCase action mh
    merkleizeCase mc = pure mc


-- | Merkleize whatever inputs need merkleization before application to a contract.
merkleizeInputs :: Continuations           -- ^ The merkliezed continuations.
                -> State                   -- ^ The initial state.
                -> Contract                -- ^ The initial contract.
                -> TransactionInput        -- ^ The input to the contract.
                -> Maybe TransactionInput  -- ^ The merkleized input to the contract, if they could be merkleized.
merkleizeInputs continuations state contract TransactionInput{..} =
  TransactionInput txInterval
    . snd
    <$> foldM (merkleizeInput continuations txInterval) ((state, contract), []) txInputs


-- | Merkleize an input if needed before application to a contract.
merkleizeInput :: Continuations
               -> TimeInterval                        -- ^ The validity interval.
               -> ((State, Contract), [Input])        -- ^ The current state and contract, along with the prior inputs.
               -> Input                               -- ^ The input.
               -> Maybe ((State, Contract), [Input])  -- ^ The new state and contract, along with the prior inputs.
merkleizeInput continuations txInterval ((state, contract), inputs) input =
  let
    demerkleize Close = Close
    demerkleize (Pay accountId payee token value contract'') = Pay accountId payee token value (demerkleize contract'')
    demerkleize (If observation thenContract elseContract) = If observation (demerkleize thenContract) (demerkleize elseContract)
    demerkleize (Let valueId value contract'') = Let valueId value (demerkleize contract'')
    demerkleize (Assert observation contract'') = Assert observation (demerkleize contract'')
    demerkleize (When cases timeout contract'') = When (demerkleizeCase <$> cases) timeout (demerkleize contract'')
    demerkleizeCase (MerkleizedCase action mh) = Case action (continuations M.! DatumHash mh)
    demerkleizeCase c = c
    contract' = demerkleize contract
    attempt =
      case fixInterval txInterval state of
        IntervalTrimmed env fixState ->
          case reduceContractUntilQuiescent env fixState contract' of
            ContractQuiescent _ _ _ curState cont ->
              case applyInput env curState input cont of
                Applied _ newState cont' -> Just (newState, cont')
                _                        -> Nothing
            _ -> Nothing
        _ -> Nothing
  in
    case attempt of
      Nothing                          -> Nothing
      Just (txOutState, txOutContract) ->
        case input of
          NormalInput content -> pure
                                   (
                                     (txOutState, txOutContract)
                                   , inputs <> [MerkleizedInput content (dataHash $ toBuiltinData txOutContract) txOutContract]
                                   )
          MerkleizedInput{}   -> Nothing
