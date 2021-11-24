-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Input/output functions for Marlowe CLI tool.
--
-----------------------------------------------------------------------------


{-# LANGUAGE FlexibleContexts #-}


module Language.Marlowe.CLI.IO (
-- * IO
  decodeFileStrict
, decodeFileBuiltinData
) where


import           Cardano.Api                (ScriptDataJsonSchema (..), scriptDataFromJson)
import           Cardano.Api.Shelley        (toPlutusData)
import           Control.Monad.Except       (MonadError, MonadIO, liftEither, liftIO)
import           Data.Aeson                 (FromJSON (..), eitherDecodeFileStrict)
import           Data.Bifunctor             (first)
import           Language.Marlowe.CLI.Types (CliError (..), liftCli)
import           Plutus.V1.Ledger.Api       (BuiltinData)
import           PlutusTx                   (dataToBuiltinData)


-- | Decode a JSON file in an error monad.
decodeFileStrict :: MonadError CliError m
                 => MonadIO m
                 => FromJSON a
                 => FilePath -- ^ The JSON file.
                 -> m a      -- ^ Action to decode the file.
decodeFileStrict filePath =
  do
    result <- liftIO $ eitherDecodeFileStrict filePath
    liftEither $ first CliError result


-- | Decode, in an error mondad, a JSON file containing built-in data.
decodeFileBuiltinData :: MonadError CliError m
                      => MonadIO m
                      => FilePath       -- ^ The JSON file.
                      -> m BuiltinData  -- ^ Action to decode the data.
decodeFileBuiltinData file =
  do
    value <- decodeFileStrict file
    liftCli
      . fmap (dataToBuiltinData . toPlutusData)
      $ scriptDataFromJson ScriptDataJsonDetailedSchema value
