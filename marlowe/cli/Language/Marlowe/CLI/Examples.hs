-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Example contracts.
--
-----------------------------------------------------------------------------


{-# LANGUAGE FlexibleContexts #-}


module Language.Marlowe.CLI.Examples (
-- * Contracts
  makeExample
) where


import           Control.Monad.Except       (MonadIO)
import           Language.Marlowe.CLI.IO    (maybeWriteJson)
import           Language.Marlowe.Semantics (MarloweData (..))


-- | Serialise an example contract to JSON.
makeExample :: MonadIO m
            => Maybe FilePath  -- ^ The output JSON file for the Marlowe data.
            -> MarloweData     -- ^ The contract and state data.
            -> m ()            -- ^ Action to serialise the Marlowe data.
makeExample = maybeWriteJson
