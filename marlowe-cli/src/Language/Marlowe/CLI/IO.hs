-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Input/output functions for the Marlowe CLI tool.
--
-----------------------------------------------------------------------------


{-# LANGUAGE FlexibleContexts #-}


module Language.Marlowe.CLI.IO (
-- * IO
  decodeFileStrict
, decodeFileBuiltinData
, readVerificationKey
, readSigningKey
, maybeWriteTextEnvelope
, maybeWriteJson
-- * Lifting
, liftCli
, liftCliIO
) where


import Cardano.Api (AsType (..), FromSomeType (..), HasTextEnvelope, ScriptDataJsonSchema (..),
                    readFileTextEnvelopeAnyOf, scriptDataFromJson, serialiseToTextEnvelope, writeFileTextEnvelope)
import Cardano.Api.Shelley (toPlutusData)
import Control.Monad ((<=<))
import Control.Monad.Except (MonadError, MonadIO, liftEither, liftIO)
import Data.Aeson (FromJSON (..), ToJSON, eitherDecodeFileStrict)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Bifunctor (first)
import Language.Marlowe.CLI.Types (CliError (..), SomePaymentSigningKey, SomePaymentVerificationKey)
import Plutus.V1.Ledger.Api (BuiltinData)
import PlutusTx (dataToBuiltinData)

import qualified Data.ByteString.Lazy as LBS (writeFile)
import qualified Data.ByteString.Lazy.Char8 as LBS8 (putStrLn)


-- | Lift an 'Either' result into the CLI.
liftCli :: MonadError CliError m
        => Show e
        => Either e a  -- ^ The result.
        -> m a         -- ^ The lifted result.
liftCli = liftEither . first (CliError . show)


-- | Lift an 'IO' 'Either' result into the CLI.
liftCliIO :: MonadError CliError m
          => MonadIO m
          => Show e
          => IO (Either e a)  -- ^ Action for the result.
          -> m a              -- ^ The lifted result.
liftCliIO = liftCli <=< liftIO


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


-- | Read a verification key.
readVerificationKey :: MonadError CliError m
               => MonadIO m
               => FilePath                      -- ^ The file.
               -> m SomePaymentVerificationKey  -- ^ Action to read the key.
readVerificationKey =
  liftCliIO
    . readFileTextEnvelopeAnyOf
      [
        FromSomeType (AsVerificationKey AsPaymentKey        ) Left
      , FromSomeType (AsVerificationKey AsPaymentExtendedKey) Right
      ]


-- | Read a signing key.
readSigningKey :: MonadError CliError m
               => MonadIO m
               => FilePath                 -- ^ The file.
               -> m SomePaymentSigningKey  -- ^ Action to read the key.
readSigningKey =
  liftCliIO
    . readFileTextEnvelopeAnyOf
      [
        FromSomeType (AsSigningKey AsPaymentKey        ) Left
      , FromSomeType (AsSigningKey AsPaymentExtendedKey) Right
      ]


-- | Optionally write a text envelope file, otherwise write to standard output.
maybeWriteTextEnvelope :: HasTextEnvelope a
                       => MonadError CliError m
                       => MonadIO m
                       => Maybe FilePath  -- ^ The output file, if any.
                       -> a               -- ^ The object to be written.
                       -> m ()            -- ^ Action for writing the file.
maybeWriteTextEnvelope Nothing           = liftIO    . LBS8.putStrLn . encodePretty . serialiseToTextEnvelope Nothing
maybeWriteTextEnvelope (Just outputFile) = liftCliIO . writeFileTextEnvelope outputFile Nothing


-- | Optional write a JSON file, otherwise write to standard output.
maybeWriteJson :: MonadIO m
               => ToJSON a
               => Maybe FilePath
               -> a
               -> m ()
maybeWriteJson Nothing           = liftIO . LBS8.putStrLn            . encodePretty
maybeWriteJson (Just outputFile) = liftIO . LBS.writeFile outputFile . encodePretty
