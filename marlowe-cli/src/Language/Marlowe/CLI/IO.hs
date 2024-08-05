{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
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
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

-- | Input/output functions for the Marlowe CLI tool.
module Language.Marlowe.CLI.IO (
  -- * IO
  decodeFileBuiltinData,
  decodeFileStrict,
  getEraHistory,
  getProtocolParams,
  getMajorProtocolVersion,
  getPV2CostModelParams,
  getSystemStart,
  maybeWriteJson,
  maybeWriteYaml,
  maybeWriteTextEnvelope,
  queryInEra,
  queryUTxOs,
  queryByAddress,
  readMaybeMetadata,
  readSigningKey,
  readVerificationKey,
  submitTxBody,
  submitTxBody',

  -- * Tx helpers
  txResourceUsage,

  -- * Environment
  getDefaultCostModel,
  getNetworkMagic,
  getNodeSocketPath,

  -- * Lifting
  liftCli,
  liftCliIO,
  liftCliExceptT,
  liftCliMaybe,
) where

import Cardano.Api (
  AsType (..),
  BabbageEraOnwards (..),
  File (..),
  FromSomeType (..),
  HasTextEnvelope,
  LocalNodeConnectInfo,
  MaryEraOnwards (..),
  NetworkId (..),
  NetworkMagic (..),
  QueryInEra (..),
  QueryInMode (..),
  QueryInShelleyBasedEra (..),
  ScriptDataJsonSchema (..),
  ShelleyBasedEra (..),
  TxMetadataInEra (..),
  TxMetadataJsonSchema (..),
  babbageEraOnwardsToShelleyBasedEra,
  fromLedgerPParams,
  getScriptData,
  getTxId,
  metadataFromJson,
  queryNodeLocalState,
  readFileTextEnvelopeAnyOf,
  scriptDataFromJson,
  serialiseToTextEnvelope,
  shelleyBasedEraConstraints,
  signShelleyTransaction,
  submitTxToNodeLocal,
  toLedgerValue,
  writeFileTextEnvelope,
 )
import Cardano.Api.Ledger qualified as Ledger
import Cardano.Api.Shelley (ProtocolParameters (protocolParamProtocolVersion), toPlutusData)
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..))
import Contrib.Cardano.TxBody qualified as T
import Contrib.Cardano.UTxO qualified as U
import Contrib.Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically, readTVar, readTVarIO, writeTVar)
import Control.Error (note)
import Control.Monad (when, (<=<))
import Control.Monad.Except (ExceptT, MonadError (..), MonadIO, liftEither, liftIO, runExceptT)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (FromJSON (..), ToJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Bifunctor (first)
import Data.ByteString qualified as BS (length)
import Data.ByteString.Char8 qualified as BS8 (putStrLn)
import Data.ByteString.Lazy qualified as LBS (writeFile)
import Data.ByteString.Lazy.Char8 qualified as LBS8 (putStrLn)
import Data.Functor ((<&>))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Time.Units (Second, TimeUnit, toMicroseconds)
import Data.Yaml as Yaml (decodeFileEither, encode, encodeFile)
import GHC.Natural (naturalFromInteger)
import Language.Marlowe.CLI.Cardano.Api (toPlutusMajorProtocolVersion)
import Language.Marlowe.CLI.Types (
  CliEnv,
  CliError (..),
  ExecutionLimitsExceeded (..),
  NodeStateInfo (..),
  Percent (..),
  QueryExecutionContext (..),
  SigningKeyFile (unSigningKeyFile),
  SomePaymentSigningKey (..),
  SomePaymentVerificationKey (..),
  SubmitMode (..),
  TxBuildupContext (..),
  TxResourceUsage (..),
  askEra,
  asksEra,
  somePaymentSigningKeyToTxWitness,
  toAddressAny',
 )
import Ouroboros.Network.Protocol.LocalStateQuery.Type
import Ouroboros.Network.Protocol.LocalTxSubmission.Client (SubmitResult (..))
import PlutusCore.Evaluation.Machine.ExBudgetingDefaults
import PlutusLedgerApi.Common (MajorProtocolVersion)
import PlutusLedgerApi.V1 (BuiltinData)
import PlutusLedgerApi.V2 (CostModelParams)
import PlutusTx (dataToBuiltinData)
import System.Directory.Internal.Prelude (fromMaybe)
import System.Environment (lookupEnv)
import System.IO (hPrint, hPutStrLn, stderr)
import Text.Read (readMaybe)

-- | Lift an 'Either' result into the CLI.
liftCli
  :: (MonadError CliError m)
  => (Show e)
  => Either e a
  -- ^ The result.
  -> m a
  -- ^ The lifted result.
liftCli = liftEither . first (CliError . show)

-- | Lift an 'Maybe' result into the CLI.
liftCliMaybe
  :: (MonadError CliError m)
  => String
  -- ^ The error message.
  -> Maybe a
  -- ^ The result.
  -> m a
  -- ^ The lifted result.
liftCliMaybe message = liftCli . maybe (Left $ CliError message) Right

-- | Lift an 'IO' 'Either' result into the CLI.
liftCliIO
  :: (MonadError CliError m)
  => (MonadIO m)
  => (Show e)
  => IO (Either e a)
  -- ^ Action for the result.
  -> m a
  -- ^ The lifted result.
liftCliIO = liftCli <=< liftIO

liftCliExceptT
  :: (MonadError CliError m)
  => (MonadIO m)
  => (Show e)
  => ExceptT e IO a
  -- ^ Action for the result.
  -> m a
  -- ^ The lifted result.
liftCliExceptT = liftCliIO . runExceptT

-- | Decode a JSON or YAML file in an error monad.
decodeFileStrict
  :: (MonadError CliError m)
  => (MonadIO m)
  => (FromJSON a)
  => FilePath
  -- ^ The JSON or YAML file.
  -> m a
  -- ^ Action to decode the file.
decodeFileStrict filePath =
  do
    result <- liftIO $ Yaml.decodeFileEither filePath
    liftEither $ first (CliError . show) result

-- | Decode, in an error monad, a JSON file containing built-in data.
decodeFileBuiltinData
  :: (MonadError CliError m)
  => (MonadIO m)
  => FilePath
  -- ^ The JSON file.
  -> m BuiltinData
  -- ^ Action to decode the data.
decodeFileBuiltinData file =
  do
    value <- decodeFileStrict file
    liftCli
      . fmap (dataToBuiltinData . toPlutusData . getScriptData)
      $ scriptDataFromJson ScriptDataJsonDetailedSchema value

-- | Read a verification key.
readVerificationKey
  :: (MonadError CliError m)
  => (MonadIO m)
  => FilePath
  -- ^ The file.
  -> m SomePaymentVerificationKey
  -- ^ Action to read the key.
readVerificationKey =
  liftCliIO
    . readFileTextEnvelopeAnyOf
      [ FromSomeType (AsVerificationKey AsPaymentKey) SomePaymentVerificationKeyPayment
      , FromSomeType (AsVerificationKey AsPaymentExtendedKey) SomePaymentVerificationKeyPaymentExtended
      , FromSomeType (AsVerificationKey AsGenesisUTxOKey) SomePaymentVerificationKeyGenesisUTxO
      ]
    . File

-- | Read a signing key.
readSigningKey
  :: (MonadError CliError m)
  => (MonadIO m)
  => SigningKeyFile
  -- ^ The file.
  -> m SomePaymentSigningKey
  -- ^ Action to read the key.
readSigningKey =
  liftCliIO
    . readFileTextEnvelopeAnyOf
      [ FromSomeType (AsSigningKey AsPaymentKey) SomePaymentSigningKeyPayment
      , FromSomeType (AsSigningKey AsPaymentExtendedKey) SomePaymentSigningKeyPaymentExtended
      , FromSomeType (AsSigningKey AsGenesisUTxOKey) SomePaymentSigningKeyGenesisUTxO
      ]
    . File
    . unSigningKeyFile

-- | Optionally write a text envelope file, otherwise write to standard output.
maybeWriteTextEnvelope
  :: (HasTextEnvelope a)
  => (MonadError CliError m)
  => (MonadIO m)
  => Maybe FilePath
  -- ^ The output file, if any.
  -> a
  -- ^ The object to be written.
  -> m ()
  -- ^ Action for writing the file.
maybeWriteTextEnvelope Nothing = liftIO . LBS8.putStrLn . encodePretty . serialiseToTextEnvelope Nothing
maybeWriteTextEnvelope (Just outputFile) = liftCliIO . writeFileTextEnvelope (File outputFile) Nothing

-- | Optional write a JSON file, otherwise write to standard output.
maybeWriteJson
  :: (MonadIO m)
  => (ToJSON a)
  => Maybe FilePath
  -> a
  -> m ()
maybeWriteJson Nothing = liftIO . LBS8.putStrLn . encodePretty
maybeWriteJson (Just outputFile) = liftIO . LBS.writeFile outputFile . encodePretty

-- | Optional write a JSON file, otherwise write to standard output.
maybeWriteYaml
  :: (MonadIO m)
  => (ToJSON a)
  => Maybe FilePath
  -> a
  -> m ()
maybeWriteYaml Nothing = liftIO . BS8.putStrLn . Yaml.encode
maybeWriteYaml (Just outputFile) = liftIO . Yaml.encodeFile outputFile

-- | Read optional metadata.
readMaybeMetadata
  :: (MonadError CliError m)
  => (MonadIO m)
  => (MonadReader (CliEnv era) m)
  => Maybe FilePath
  -- ^ The metadata file, if any.
  -> m (TxMetadataInEra era)
  -- ^ Action for reading the metadata.
readMaybeMetadata file =
  do
    metadata <- mapM decodeFileStrict file
    era <- asksEra babbageEraOnwardsToShelleyBasedEra
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
    pure $
      if path == Just ""
        then Nothing
        else path

getDefaultCostModel :: (MonadError CliError m) => m CostModelParams
getDefaultCostModel = liftCliMaybe "Missing default cost model." defaultCostModelParamsForTesting

-- | Query a node in an era.
-- TODO: At the end we would like to make this private and proxy everything through
-- `QueryExecutionContext` parametrized functions.
queryInEra
  :: (MonadError CliError m)
  => (MonadIO m)
  => (MonadReader (CliEnv era) m)
  => LocalNodeConnectInfo
  -- ^ The connection info for the local node.
  -> QueryInShelleyBasedEra era a
  -- ^ The query.
  -> m a
  -- ^ Action for running the query.
queryInEra connection q = do
  era <- askEra
  res <-
    liftCliExceptT $
      queryNodeLocalState connection VolatileTip $
        QueryInEra $
          QueryInShelleyBasedEra (babbageEraOnwardsToShelleyBasedEra era) q
  liftCli res

queryUTxOs
  :: (MonadError CliError m)
  => (MonadIO m)
  => (MonadReader (CliEnv era) m)
  => QueryExecutionContext era
  -- ^ The query context.
  -> C.QueryUTxOFilter
  -- ^ The query.
  -> m (C.UTxO era)
  -- ^ Action for running the query.
queryUTxOs (PureQueryContext utxosVar _) queryFilterFilter = do
  utxos <- liftIO $ readTVarIO utxosVar
  case queryFilterFilter of
    C.QueryUTxOByAddress addresses -> do
      let utxosList = U.toList utxos
          filterStep (_, C.TxOut address _ _ _) =
            Set.member (toAddressAny' address) addresses
      pure $ U.fromList $ filter filterStep utxosList
    C.QueryUTxOWhole -> pure utxos
    C.QueryUTxOByTxIn txins -> do
      let utxosList = U.toList utxos
          filterStep (txin, _) = Set.member txin txins
      pure $ U.fromList $ filter filterStep utxosList
queryUTxOs (QueryNode connection) queryFilterFilter = queryInEra connection $ C.QueryUTxO queryFilterFilter

queryByAddress
  :: (MonadError CliError m)
  => (MonadIO m)
  => (MonadReader (CliEnv era) m)
  => QueryExecutionContext era
  -> C.AddressInEra era
  -- ^ Address to query.
  -> m (C.UTxO era)
  -- ^ Action for running the query.
queryByAddress queryCtx = queryUTxOs queryCtx . C.QueryUTxOByAddress . Set.singleton . toAddressAny'

getProtocolParams
  :: (MonadError CliError m)
  => (MonadIO m)
  => (MonadReader (CliEnv era) m)
  => QueryExecutionContext era
  -> m C.ProtocolParameters
getProtocolParams (QueryNode connection) = do
  era <- asksEra babbageEraOnwardsToShelleyBasedEra
  fromLedgerPParams era <$> queryInEra connection QueryProtocolParameters
getProtocolParams (PureQueryContext _ NodeStateInfo{nsiProtocolParameters}) = pure nsiProtocolParameters

queryAny :: (MonadError CliError m, MonadIO m) => LocalNodeConnectInfo -> QueryInMode a -> m a
queryAny connection = liftCliExceptT . queryNodeLocalState connection VolatileTip

getSystemStart
  :: (MonadError CliError m)
  => (MonadIO m)
  => QueryExecutionContext era
  -> m C.SystemStart
getSystemStart (QueryNode connection) = queryAny connection QuerySystemStart
getSystemStart (PureQueryContext _ NodeStateInfo{nsiSystemStart}) = pure nsiSystemStart

getEraHistory
  :: (MonadError CliError m)
  => (MonadIO m)
  => QueryExecutionContext era
  -> m C.EraHistory
getEraHistory (QueryNode connection) = queryAny connection QueryEraHistory
getEraHistory (PureQueryContext _ NodeStateInfo{nsiEraHistory}) = pure nsiEraHistory

getMajorProtocolVersion
  :: (MonadError CliError m)
  => (MonadIO m)
  => (MonadReader (CliEnv era) m)
  => QueryExecutionContext era
  -> m MajorProtocolVersion
getMajorProtocolVersion queryCtx = do
  protocol <- getProtocolParams queryCtx
  pure $ toPlutusMajorProtocolVersion $ protocolParamProtocolVersion protocol

getPV2CostModelParams
  :: (MonadError CliError m)
  => (MonadIO m)
  => (MonadReader (CliEnv era) m)
  => QueryExecutionContext era
  -> m [Integer]
getPV2CostModelParams queryCtx = do
  protocolParams <- getProtocolParams queryCtx
  let pv2 = C.AnyPlutusScriptVersion C.PlutusScriptV2
  C.CostModel costModel <- do
    let costModels = C.protocolParamCostModels protocolParams
    liftCli $ note ("Missing PV2 cost model" :: String) $ Map.lookup pv2 costModels
  pure $ fromIntegral <$> costModel

-- | Wait for transactions to be confirmed as UTxOs.
waitForUtxos
  :: (TimeUnit a)
  => (MonadError CliError m)
  => (MonadIO m)
  => (MonadReader (CliEnv era) m)
  => LocalNodeConnectInfo
  -- ^ The connection info for the local node.
  -> a
  -- ^ The time interval to wait for the transaction to be confirmed.
  -> [C.TxIn]
  -- ^ The transactions to wait for.
  -> m ()
  -- ^ Action to wait for the transaction confirmations.
waitForUtxos connection timeout txIns = do
  let timeoutMicroseconds = toMicroseconds timeout
      pause = 5 :: Second
      pauseMicroseconds = toMicroseconds pause
      txIns' = Set.fromList txIns
      go 0 = throwError "Timeout waiting for transaction to be confirmed."
      go n = do
        liftIO . threadDelay $ pause
        utxos <- do
          queryInEra connection
            . C.QueryUTxO
            . C.QueryUTxOByTxIn
            $ txIns'
        if Map.keysSet (C.unUTxO utxos) == txIns'
          then do
            pure ()
          else go (n - 1 :: Int)
  go . ceiling $ (fromIntegral timeoutMicroseconds / fromIntegral pauseMicroseconds :: Double)

txResourceUsage
  :: C.BabbageEraOnwards era
  -> C.ProtocolParameters
  -> C.TxBody era
  -> TxResourceUsage
txResourceUsage era pp txBody =
  let size =
        naturalFromInteger $
          toInteger $
            BS.length $
              shelleyBasedEraConstraints (babbageEraOnwardsToShelleyBasedEra era) $
                C.serialiseToCBOR txBody
      maxSize = C.protocolParamMaxTxSize pp
      fractionSize = 100 * size `div` maxSize

      ExUnits memory steps = T.exUnits txBody

      maxExecutionUnits = C.protocolParamMaxTxExUnits pp
      fractionMemory = 100 * memory `div` maybe 0 C.executionMemory maxExecutionUnits
      fractionSteps = 100 * steps `div` maybe 0 C.executionSteps maxExecutionUnits
   in TxResourceUsage
        { elMemory = (memory, Percent fractionMemory)
        , elSteps = (steps, Percent fractionSteps)
        , elSize = (size, Percent fractionSize)
        }

checkTxLimits
  :: C.BabbageEraOnwards era
  -- ^ The era to serialise the transaction in.
  -> C.ProtocolParameters
  -- ^ The protocol params to check against.
  -> C.TxBody era
  -- ^ The transaction body.
  -> Maybe ExecutionLimitsExceeded
  -- ^ The error if the transaction exceeds the limits.
checkTxLimits era pp txBody = do
  let -- we are not able to define this helper in the same `let` block below :-)
      unPercent' = unPercent . snd
  let usage = txResourceUsage era pp txBody
      TxResourceUsage
        { elMemory = unPercent' -> elMemoryPercent
        , elSteps = unPercent' -> elStepsPercent
        , elSize = unPercent' -> elSizePercent
        } = usage
  if elMemoryPercent >= 100 || elStepsPercent >= 100 || elSizePercent >= 100
    then Just $ ExecutionLimitsExceeded usage
    else Nothing

-- | Sign and submit a transaction.
submitTxBody
  :: (MonadError CliError m)
  => (MonadIO m)
  => (MonadReader (CliEnv era) m)
  => TxBuildupContext era
  -- ^ The connection info for the local node.
  -> C.TxBody era
  -- ^ The transaction body.
  -> [SomePaymentSigningKey]
  -- ^ The signing keys.
  -> m C.TxId
  -- ^ The action to submit the transaction.
submitTxBody txBuildupContext txBody signings =
  do
    era <- askEra
    let shelleyEra = babbageEraOnwardsToShelleyBasedEra era
        tx =
          signShelleyTransaction shelleyEra txBody $
            somePaymentSigningKeyToTxWitness <$> signings
        txId = getTxId txBody
    case txBuildupContext of
      (NodeTxBuildup _ DontSubmit) -> pure txId
      (NodeTxBuildup connection (DoSubmit timeout)) -> do
        result <- liftIO . submitTxToNodeLocal connection $ C.TxInMode shelleyEra tx
        case result of
          SubmitSuccess -> do
            when (toMicroseconds timeout > 0) $
              waitForUtxos connection timeout [C.TxIn txId $ C.TxIx 0]
            pure txId
          SubmitFail reason -> do
            liftIO $ hPutStrLn stderr "Submission of the transaction failed:"
            liftIO $ hPrint stderr txBody
            throwError . CliError $ show reason
      (PureTxBuildup utxosVar NodeStateInfo{nsiProtocolParameters}) -> do
        case checkTxLimits era nsiProtocolParameters txBody of
          Just exceeded -> do
            throwError . CliError $ show exceeded
          Nothing -> pure ()
        let C.TxBody txBodyContent = txBody

            txIns = map fst . C.txIns $ txBodyContent
            txInsRef = case C.txInsReference txBodyContent of
              C.TxInsReferenceNone -> []
              C.TxInsReference _ ins -> ins
            txOuts = map C.toCtxUTxOTxOut . C.txOuts $ txBodyContent
            newUTxOs = zip [0 ..] txOuts <&> \(txIx, txOut) -> (C.TxIn txId (C.TxIx txIx), txOut)
        liftIO $ hPutStrLn stderr "Submitting transaction to pure context.."
        liftIO $ hPutStrLn stderr $ "txIns: " <> show txIns
        liftIO $ hPutStrLn stderr $ "txInsRef: " <> show txInsRef
        res <- liftIO $ atomically do
          utxos <- readTVar utxosVar
          let utxosMap = C.unUTxO utxos
          if any (flip Map.notMember utxosMap) (txIns ++ txInsRef)
            then
              pure $
                Left $
                  "UTxO missing:"
                    <> show (filter (flip Map.notMember utxosMap) (txIns ++ txInsRef))
                    <> " . All UTxOs: "
                    <> show (Map.keys utxosMap)
            else do
              let utxosList = U.toList utxos
                  utxosList' =
                    newUTxOs
                      <> filter (\(txIn, _) -> txIn `notElem` txIns) utxosList
                  utxos' = U.fromList utxosList'
              writeTVar utxosVar utxos'
              pure $ Right txId
        liftCli res

-- A version of submit which performs an attempt to extra fee balancing on failure
-- (we experienced failures on the cardano-node for already balanced transactions).
-- TODO: Refactor this function so it doesn't require the TxBodyContent to be passed in.
submitTxBody'
  :: (MonadError CliError m)
  => (MonadIO m)
  => (MonadReader (CliEnv era) m)
  => TxBuildupContext era
  -- ^ The connection info for the local node.
  -> C.TxBody era
  -- ^ The transaction body.
  -> C.TxBodyContent C.BuildTx era
  -- ^ The same transaction body content - used to rebuild the transaction with adjusted fees.
  -> C.AddressInEra era
  -- ^ The change address.
  -> [SomePaymentSigningKey]
  -- ^ The signing keys.
  -> m (C.TxId, C.TxBody era)
  -- ^ The action to submit the transaction.
submitTxBody' txBuildupCtx body bodyContent changeAddress signingKeys = do
  era <- askEra
  ((,body) <$> submitTxBody txBuildupCtx body signingKeys) `catchError` \err -> do
    liftIO $ hPutStrLn stderr "Adjusting the fees and resubmitting failing transaction."
    liftIO $ hPrint stderr err
    let feeBalancingMargin = Ledger.Coin 20000
        C.TxBodyContent{..} = bodyContent
        -- Find change UTxO and subtract the fee margin.
        step (C.TxOut addr value datum refScript) (False, outs)
          | addr == changeAddress && (fromMaybe 0 . C.valueToLovelace $ C.txOutValueToValue value) > feeBalancingMargin = do
              let value' = C.txOutValueToValue value <> C.negateValue (C.lovelaceToValue feeBalancingMargin)
                  out' =
                    C.TxOut
                      addr
                      ( case era of
                          BabbageEraOnwardsBabbage ->
                            C.TxOutValueShelleyBased ShelleyBasedEraBabbage $ toLedgerValue MaryEraOnwardsBabbage value'
                          BabbageEraOnwardsConway ->
                            C.TxOutValueShelleyBased ShelleyBasedEraConway $ toLedgerValue MaryEraOnwardsConway value'
                      )
                      datum
                      refScript
              (True, out' : outs)
        step out (flag, outs) = (flag, out : outs)

        (adjusted, txOuts') = foldr step (False, []) txOuts
    -- Increase the transaction fee by the chosen margin.
    txFee' <- case (adjusted, txFee) of
      (True, C.TxFeeExplicit feesInEra value) -> pure $ C.TxFeeExplicit feesInEra (value + feeBalancingMargin)
      _ -> throwError . CliError $ "Unable to adjust the change during resubmission attempt."

    case C.createAndValidateTransactionBody
      (babbageEraOnwardsToShelleyBasedEra era)
      (C.TxBodyContent{..}{C.txOuts = txOuts', C.txFee = txFee'}) of
      Left err' -> throwError . CliError $ "Failure during reconstruction of the failing tx body: " <> show err'
      Right body' -> do
        txId <- submitTxBody txBuildupCtx body' signingKeys
        pure (txId, body')
