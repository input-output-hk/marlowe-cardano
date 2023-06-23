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

-- | Merkleize Marlowe contracts.
module Language.Marlowe.CLI.Merkle (
  -- * Merkleization
  merkleize,
  merkleizeMarlowe,

  -- * Demerkleization
  demerkleize,
) where

import Control.Monad.Except (MonadError, MonadIO)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Writer (runWriter)
import Language.Marlowe.CLI.IO (decodeFileStrict, maybeWriteJson)
import Language.Marlowe.CLI.Types (CliError (..), MarloweTransaction (..), SomeMarloweTransaction (..))
import Language.Marlowe.Core.V1.Merkle (deepDemerkleize, deepMerkleize)

-- | Merkleize all of the cases in a contract.
merkleize
  :: (MonadError CliError m)
  => (MonadIO m)
  => FilePath
  -- ^ The JSON file containing the Marlowe transaction information.
  -> Maybe FilePath
  -- ^ Output JSON file containing the merkleized Marlowe transaction information.
  -> m ()
  -- ^ Action to perform the merkleization.
merkleize marloweFile outputFile =
  do
    SomeMarloweTransaction lang era marlowe <- decodeFileStrict marloweFile
    maybeWriteJson outputFile
      . SomeMarloweTransaction lang era
      $ merkleizeMarlowe marlowe

-- | Deeply merkleize a Marlowe transaction.
merkleizeMarlowe
  :: MarloweTransaction lang era
  -- ^ The Marlowe transaction information.
  -> MarloweTransaction lang era
  -- ^ Action for the merkleized Marlowe transaction information.
merkleizeMarlowe marlowe =
  let (contract, continuations) =
        runWriter
          . deepMerkleize
          $ mtContract marlowe
   in marlowe
        { mtContract = contract
        , mtContinuations = continuations
        }

-- | Demerkleize all of the case statements in a contract.
demerkleize
  :: (MonadError CliError m)
  => (MonadIO m)
  => FilePath
  -- ^ The JSON file containing the Marlowe transaction information.
  -> Maybe FilePath
  -- ^ Output JSON file containing the demerkleized Marlowe transaction information.
  -> m ()
demerkleize marloweFile outputFile =
  do
    SomeMarloweTransaction lang era marlowe <- decodeFileStrict marloweFile
    contract <-
      runReaderT
        (deepDemerkleize $ mtContract marlowe)
        $ mtContinuations marlowe
    let marlowe' =
          marlowe
            { mtContract = contract
            , mtContinuations = mempty
            }
    maybeWriteJson outputFile $ SomeMarloweTransaction lang era marlowe'
