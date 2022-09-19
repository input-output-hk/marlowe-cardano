-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Merkleize Marlowe contracts.
--
-----------------------------------------------------------------------------


{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}


module Language.Marlowe.CLI.Merkle
  ( -- * Merkleization
    deepMerkleize
  , merkleize
  , merkleizeMarlowe
  , shallowMerkleize
    -- * Demerkleization
  , deepDemerkleize
  , demerkleize
  , shallowDemerkleize
    -- * Inputs
  , merkleizeInput
  , merkleizeInputs
  ) where


import Control.Monad (foldM)
import Control.Monad.Except (MonadError, MonadIO, catchError, throwError)
import Control.Monad.Fix (fix)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Writer (Writer, runWriter, tell)
import Language.Marlowe.CLI.IO (decodeFileStrict, maybeWriteJson)
import Language.Marlowe.CLI.Orphans ()
import Language.Marlowe.CLI.Types (CliError(..), Continuations, MarloweTransaction(..), SomeMarloweTransaction(..))
import Language.Marlowe.Core.V1.Semantics
  ( ApplyResult(..)
  , ReduceResult(..)
  , TransactionInput(..)
  , TransactionOutput(..)
  , applyInput
  , computeTransaction
  , fixInterval
  , reduceContractUntilQuiescent
  )
import Language.Marlowe.Core.V1.Semantics.Types
  (Case(..), Contract(..), Input(..), IntervalResult(..), State, TimeInterval)
import Plutus.Script.Utils.Scripts (dataHash)
import Plutus.V1.Ledger.Api (DatumHash(..), toBuiltinData)

import qualified Data.Map.Strict as M (lookup, singleton)


-- | Merkleize all of the cases in a contract.
merkleize :: MonadError CliError m
          => MonadIO m
          => FilePath        -- ^ The JSON file containing the Marlowe transaction information.
          -> Maybe FilePath  -- ^ Output JSON file containing the merkleized Marlowe transaction information.
          -> m ()            -- ^ Action to perform the merkleization.
merkleize marloweFile outputFile =
  do
    SomeMarloweTransaction lang era marlowe <- decodeFileStrict marloweFile
    maybeWriteJson outputFile
      . SomeMarloweTransaction lang era
      $ merkleizeMarlowe marlowe


-- | Deeply merkleize a Marlowe transaction.
merkleizeMarlowe :: MarloweTransaction lang era  -- ^ The Marlowe transaction information.
                 -> MarloweTransaction lang era  -- ^ Action for the merkleized Marlowe transaction information.
merkleizeMarlowe marlowe =
  let
    (contract, continuations) =
      runWriter
       . deepMerkleize
       $ mtContract marlowe
  in
    marlowe
     {
       mtContract      = contract
     , mtContinuations = continuations
     }


-- | Merkleize any top-level case statements in a contract.
shallowMerkleize :: Contract                       -- ^ The contract.
                 -> Writer Continuations Contract  -- ^ Action for the merkleized contract.
shallowMerkleize = merkleize' pure


-- | Merkleize all case statements in a contract.
deepMerkleize :: Contract                       -- ^ The contract.
              -> Writer Continuations Contract  -- ^ Action for the merkleized contract.
deepMerkleize = fix merkleize'


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
    merkleizeCase c@(Case _ Close) = pure c
    merkleizeCase (Case action continuation) =
      do
        continuation' <- f continuation
        let
          mh = dataHash $ toBuiltinData continuation'
        tell $ DatumHash mh `M.singleton` continuation'
        pure $ MerkleizedCase action mh
    merkleizeCase mc = pure mc


-- | Demerkleize all of the case statements in a contract.
demerkleize :: MonadError CliError m
            => MonadIO m
            => FilePath        -- ^ The JSON file containing the Marlowe transaction information.
            -> Maybe FilePath  -- ^ Output JSON file containing the demerkleized Marlowe transaction information.
            -> m ()
demerkleize marloweFile outputFile =
  do
    SomeMarloweTransaction lang era marlowe <- decodeFileStrict marloweFile
    contract <-
      runReaderT
       (deepDemerkleize $ mtContract marlowe)
       $ mtContinuations marlowe
    let
      marlowe' =
        marlowe
        {
          mtContract      = contract
        , mtContinuations = mempty
        }
    maybeWriteJson outputFile $ SomeMarloweTransaction lang era marlowe'


-- | Demerkleize any top-level case statements in a contract.
shallowDemerkleize :: MonadError CliError m
                   => Contract                          -- ^ The contract.
                   -> ReaderT Continuations m Contract  -- ^ Action for the demerkleized contract.
shallowDemerkleize = demerkleize' pure


-- | Demerkleize all case statements in a contract.
deepDemerkleize :: MonadError CliError m
                => Contract                          -- ^ The contract.
                -> ReaderT Continuations m Contract  -- ^ Action for the demerkleized contract.
deepDemerkleize = fix demerkleize'


-- | Demerkleize selected case statements in a contract.
demerkleize' :: MonadError CliError m
             => (Contract -> ReaderT Continuations m Contract)  -- ^ Action to continue demerkleization.
             -> Contract                                        -- ^ The contract.
             -> ReaderT Continuations m Contract                -- ^ Action for demerkleized the selected case statements.
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
        continuation <- maybe (throwError . CliError $ "Missing continuation for hash " <> show mh <> ".") f continuation'
        pure $ Case action continuation
    demerkleizeCase c = pure c


-- | Merkleize whatever inputs need merkleization before application to a contract.
merkleizeInputs :: MonadError CliError m
                => MarloweTransaction lang era  -- ^ The transaction information.
                -> TransactionInput             -- ^ The input to the contract.
                -> m TransactionInput           -- ^ Action for the merkleized input to the contract.
merkleizeInputs MarloweTransaction{..} TransactionInput{..} =
  TransactionInput txInterval
    . snd
    <$> foldM (merkleizeInput txInterval mtContinuations) ((mtState, mtContract), []) txInputs


-- | Merkleize an input if needed before application to a contract.
merkleizeInput :: MonadError CliError m
               => TimeInterval                   -- ^ The validity interval.
               -> Continuations                  -- ^ The available continuations.
               -> ((State, Contract), [Input])   -- ^ The current state and contract, along with the prior inputs.
               -> Input                          -- ^ The input.
               -> m ((State, Contract), [Input]) -- ^ The new state and contract, along with the prior inputs.
merkleizeInput txInterval continuations ((state, contract), inputs) input =
  do
    -- Apply input as it is.
    let
      attempt = computeTransaction (TransactionInput txInterval [input]) state contract
      result =
        case attempt of
          TransactionOutput{..} -> pure ((txOutState, txOutContract), inputs ++ [input])
          Error e               -> throwError . CliError $ show e
    -- Try applying the input to the demerkleized contract.
    contract' <- shallowDemerkleize contract `runReaderT` continuations
    let
      attempt' = computeTransaction' txInterval input state contract'
      result' =
        case attempt' of
          Just (txOutState, txOutContract) ->
            do
              input' <-
                case input of
                  NormalInput content -> pure
                                           $ MerkleizedInput
                                             content
                                             (dataHash $ toBuiltinData txOutContract)
                                             txOutContract
                  MerkleizedInput{}   -> throwError $ CliError "Unexpected mekleized input."
              pure ((txOutState, txOutContract), inputs ++ [input'])
          Nothing               -> throwError $ CliError "Failed to merkleize input."
    -- See if either as-is or merkleized input is accepted.
    result `catchError` const result'


-- | Compute one step in a transaction without reducing the quiescent state after the input is applied.
computeTransaction' :: TimeInterval             -- ^ The validity interval.
                    -> Input                    -- ^ The input to the contract.
                    -> State                    -- ^ The current state of the contract.
                    -> Contract                 -- ^ The current contract.
                    -> Maybe (State, Contract)  -- ^ The new state and contract, if the input could be applied.
computeTransaction' txInterval input state contract =
  case fixInterval txInterval state of
    IntervalTrimmed env fixState ->
      case reduceContractUntilQuiescent env fixState contract of
        ContractQuiescent _ _ _ curState cont ->
          case applyInput env curState input cont of
            Applied _ newState cont' -> Just (newState, cont')
            _                        -> Nothing
        _ -> Nothing
    _ -> Nothing
