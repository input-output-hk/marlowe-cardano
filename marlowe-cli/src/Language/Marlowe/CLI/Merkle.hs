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


module Language.Marlowe.CLI.Merkle (
-- * Merkleization
  merkleize
, deepMerkleize
, shallowMerkleize
-- * Demerkleization
, demerkleize
, deepDemerkleize
, shallowDemerkleize
) where


import Cardano.Api (AlonzoEra)
import Control.Monad.Except (MonadError, MonadIO, throwError)
import Control.Monad.Fix (fix)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Writer (Writer, runWriter, tell)
import Language.Marlowe.CLI.IO (decodeFileStrict, maybeWriteJson)
import Language.Marlowe.CLI.Orphans ()
import Language.Marlowe.CLI.Types (CliError (..), Continuations, MarloweTransaction (..))
import Language.Marlowe.Semantics.Types (Case (..), Contract (..))
import Ledger.Scripts (dataHash)
import Plutus.V1.Ledger.Api (DatumHash (..), toBuiltinData)

import qualified Data.Map.Strict as M (lookup, singleton)


-- | Merkleize all of the cases in a contract.
merkleize :: MonadError CliError m
          => MonadIO m
          => FilePath        -- ^ The JSON file containing the Marlowe transaction information.
          -> Maybe FilePath  -- ^ Output JSON file containing the merkleized Marlowe transaction information.
          -> m ()            -- ^ Action to perform the merkleization.
merkleize contractFile outputFile =
  do
    marlowe <- decodeFileStrict contractFile
    let
      (contract, continuations) =
        runWriter
         . deepMerkleize
         $ mtContract marlowe
      marlowe' =
        marlowe
        {
          mtContract      = contract
        , mtContinuations = continuations
        }
        :: MarloweTransaction AlonzoEra
    maybeWriteJson outputFile marlowe'


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
          mh = dataHash $ toBuiltinData continuation
        tell $ M.singleton (DatumHash mh) continuation'
        pure $ MerkleizedCase action mh
    merkleizeCase mc = pure mc


-- | Demerkleize all of the case statements in a contract.
demerkleize :: MonadError CliError m
            => MonadIO m
            => FilePath        -- ^ The JSON file containing the Marlowe transaction information.
            -> Maybe FilePath  -- ^ Output JSON file containing the demerkleized Marlowe transaction information.
            -> m ()
demerkleize contractFile outputFile =
  do
    marlowe <- decodeFileStrict contractFile
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
        :: MarloweTransaction AlonzoEra
    maybeWriteJson outputFile marlowe'


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
