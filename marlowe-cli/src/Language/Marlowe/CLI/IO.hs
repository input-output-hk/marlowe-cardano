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


module Language.Marlowe.CLI.IO
  ( -- * IO
    decodeFileBuiltinData
  , decodeFileStrict
  , getProtocolVersion
  , maybeWriteJson
  , maybeWriteTextEnvelope
  , queryInEra
  , readMaybeMetadata
  , readSigningKey
  , readVerificationKey
    -- * Environment
  , getDefaultCostModel
  , getNetworkMagic
  , getNodeSocketPath
    -- * Lifting
  , liftCli
  , liftCliIO
  , liftCliMaybe
  ) where


import Cardano.Api
  ( AsType(..)
  , CardanoMode
  , FromSomeType(..)
  , HasTextEnvelope
  , LocalNodeConnectInfo
  , NetworkId(..)
  , NetworkMagic(..)
  , QueryInEra(QueryInShelleyBasedEra)
  , QueryInMode(QueryInEra)
  , QueryInShelleyBasedEra(QueryProtocolParameters)
  , ScriptDataJsonSchema(..)
  , TxMetadataInEra(..)
  , TxMetadataJsonSchema(..)
  , metadataFromJson
  , queryNodeLocalState
  , readFileTextEnvelopeAnyOf
  , scriptDataFromJson
  , serialiseToTextEnvelope
  , writeFileTextEnvelope
  )
import Cardano.Api.Shelley (ProtocolParameters(protocolParamProtocolVersion), toPlutusData)
import Control.Monad ((<=<))
import Control.Monad.Except (MonadError, MonadIO, liftEither, liftIO)
import Data.Aeson (FromJSON(..), ToJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Bifunctor (first)
import Data.Yaml (decodeFileEither)
import Language.Marlowe.CLI.Types
  ( CliEnv
  , CliError(..)
  , SigningKeyFile(unSigningKeyFile)
  , SomePaymentSigningKey
  , SomePaymentVerificationKey
  , askEra
  , asksEra
  , toEraInMode
  , toShelleyBasedEra
  , toTxMetadataSupportedInEra
  )
import Plutus.V1.Ledger.Api (BuiltinData)
import PlutusTx (dataToBuiltinData)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import Control.Monad.Reader (MonadReader)
import qualified Data.ByteString.Lazy as LBS (writeFile)
import qualified Data.ByteString.Lazy.Char8 as LBS8 (putStrLn)
import Language.Marlowe.CLI.Cardano.Api (toPlutusProtocolVersion)
import Plutus.ApiCommon (ProtocolVersion)
import Plutus.V2.Ledger.Api (CostModelParams)
import PlutusCore.Evaluation.Machine.ExBudgetingDefaults (defaultCostModelParams)


-- | Lift an 'Either' result into the CLI.
liftCli :: MonadError CliError m
        => Show e
        => Either e a  -- ^ The result.
        -> m a         -- ^ The lifted result.
liftCli = liftEither . first (CliError . show)


-- | Lift an 'Maybe' result into the CLI.
liftCliMaybe :: MonadError CliError m
             => String   -- ^ The error message.
             -> Maybe a  -- ^ The result.
             -> m a      -- ^ The lifted result.
liftCliMaybe message = liftCli . maybe (Left $ CliError message) Right


-- | Lift an 'IO' 'Either' result into the CLI.
liftCliIO :: MonadError CliError m
          => MonadIO m
          => Show e
          => IO (Either e a)  -- ^ Action for the result.
          -> m a              -- ^ The lifted result.
liftCliIO = liftCli <=< liftIO


-- | Decode a JSON or YAML file in an error monad.
decodeFileStrict :: MonadError CliError m
                 => MonadIO m
                 => FromJSON a
                 => FilePath -- ^ The JSON or YAML file.
                 -> m a      -- ^ Action to decode the file.
decodeFileStrict filePath =
  do
    result <- liftIO $ decodeFileEither filePath
    liftEither $ first (CliError . show) result


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
               => SigningKeyFile                 -- ^ The file.
               -> m SomePaymentSigningKey  -- ^ Action to read the key.
readSigningKey =
  liftCliIO
    . readFileTextEnvelopeAnyOf
      [
        FromSomeType (AsSigningKey AsPaymentKey        ) Left
      , FromSomeType (AsSigningKey AsPaymentExtendedKey) Right
      ]
    . unSigningKeyFile


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


-- | Read optional metadata.
readMaybeMetadata :: MonadError CliError m
                  => MonadIO m
                  => MonadReader (CliEnv era) m
                  => Maybe FilePath                 -- ^ The metadata file, if any.
                  -> m (TxMetadataInEra era)  -- ^ Action for reading the metadata.
readMaybeMetadata file =
  do
    metadata <- sequence $ decodeFileStrict <$> file
    era <- asksEra toTxMetadataSupportedInEra
    maybe
      (pure TxMetadataNone)
      (fmap (TxMetadataInEra era) . liftCli . metadataFromJson TxMetadataJsonNoSchema)
      metadata


-- | Read the CARDANO_TESTNET_MAGIC environment variable for the default network magic.
getNetworkMagic :: IO (Maybe NetworkId)
getNetworkMagic =
  fmap (Testnet . NetworkMagic)
    . (readMaybe =<<)
    <$> lookupEnv "CARDANO_TESTNET_MAGIC"


-- | Read the CARDANO_NODE_SOCKET_PATH environment variable for the default node socket path.
getNodeSocketPath :: IO (Maybe FilePath)
getNodeSocketPath =
  do
    path <- lookupEnv "CARDANO_NODE_SOCKET_PATH"
    pure
      $ if path == Just ""
          then Nothing
          else path

getDefaultCostModel :: MonadError CliError m => m CostModelParams
getDefaultCostModel = liftCliMaybe "Missing default cost model." defaultCostModelParams

-- | Query a node in an era.
queryInEra :: MonadError CliError m
            => MonadIO m
            => MonadReader (CliEnv era) m
            => LocalNodeConnectInfo CardanoMode     -- ^ The connection info for the local node.
            -> QueryInShelleyBasedEra era a         -- ^ The query.
            -> m a                                  -- ^ Action for running the query.
queryInEra connection q = do
  era <- askEra
  res <- liftCliIO
    $ queryNodeLocalState connection Nothing
    $ QueryInEra (toEraInMode era)
    $ QueryInShelleyBasedEra (toShelleyBasedEra era) q
  liftCli res

getProtocolVersion :: MonadError CliError m
                   => MonadIO m
                   => MonadReader (CliEnv era) m
                   => LocalNodeConnectInfo CardanoMode
                   -> m ProtocolVersion
getProtocolVersion connection = do
  protocol <- queryInEra connection QueryProtocolParameters
  pure $ toPlutusProtocolVersion $ protocolParamProtocolVersion protocol
