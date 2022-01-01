-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Submitting Marlowe transactions.
--
-----------------------------------------------------------------------------


{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE TypeFamilies       #-}


module Language.Marlowe.CLI.Transaction (
-- * Building
  buildSimple
, buildIncoming
, buildContinuing
, buildOutgoing
-- * Submitting
, submit
-- * Low-Level Functions
, buildBody
, buildPayFromScript
, buildPayToScript
, hashSigningKey
, queryAlonzo
, submitBody
) where


import           Cardano.Api                                       (AddressAny, AddressInEra, AlonzoEra, AsType (..),
                                                                    BalancedTxBody (..), BuildTx, BuildTxWith (..),
                                                                    CardanoEra (..), CardanoMode,
                                                                    CollateralSupportedInEra (..),
                                                                    ConsensusModeIsMultiEra (..), CtxTx, EraInMode (..),
                                                                    ExecutionUnits (..), Hash, KeyWitnessInCtx (..),
                                                                    LocalNodeConnectInfo, MultiAssetSupportedInEra (..),
                                                                    PaymentKey, PlutusScript, PlutusScriptV1,
                                                                    PlutusScriptVersion (..), QueryInEra (..),
                                                                    QueryInMode (..), QueryInShelleyBasedEra (..),
                                                                    QueryUTxOFilter (..), ScriptDataSupportedInEra (..),
                                                                    ScriptDatum (..), ScriptLanguageInEra (..),
                                                                    ScriptValidity (ScriptInvalid), ScriptWitness (..),
                                                                    ScriptWitnessInCtx (..), ShelleyBasedEra (..),
                                                                    ShelleyWitnessSigningKey (..), SlotNo,
                                                                    TxAuxScripts (..), TxBody (..), TxBodyContent (..),
                                                                    TxBodyErrorAutoBalance (..), TxBodyScriptData (..),
                                                                    TxCertificates (..), TxExtraKeyWitnesses (..),
                                                                    TxExtraKeyWitnessesSupportedInEra (..), TxFee (..),
                                                                    TxFeesExplicitInEra (..), TxId, TxIn (..),
                                                                    TxInMode (..), TxInsCollateral (..), TxIx (..),
                                                                    TxMetadataInEra (..), TxMintValue (..), TxOut (..),
                                                                    TxOutDatum (..), TxOutValue (..),
                                                                    TxScriptValidity (..),
                                                                    TxScriptValiditySupportedInEra (TxScriptValiditySupportedInAlonzoEra),
                                                                    TxUpdateProposal (..), TxValidityLowerBound (..),
                                                                    TxValidityUpperBound (..), TxWithdrawals (..),
                                                                    UTxO (..), ValidityLowerBoundSupportedInEra (..),
                                                                    ValidityNoUpperBoundSupportedInEra (..),
                                                                    ValidityUpperBoundSupportedInEra (..), Value,
                                                                    WitCtxTxIn, Witness (..), anyAddressInEra,
                                                                    castVerificationKey, getTxId, getVerificationKey,
                                                                    hashScriptData, lovelaceToValue,
                                                                    makeTransactionBodyAutoBalance, queryNodeLocalState,
                                                                    readFileTextEnvelope, serialiseToCBOR,
                                                                    signShelleyTransaction, submitTxToNodeLocal,
                                                                    verificationKeyHash, writeFileTextEnvelope)
import           Cardano.Api.Shelley                               (TxBody (ShelleyTxBody), fromPlutusData,
                                                                    protocolParamMaxTxExUnits, protocolParamMaxTxSize)
import           Cardano.Ledger.Alonzo.Scripts                     (ExUnits (..))
import           Cardano.Ledger.Alonzo.TxWitness                   (Redeemers (..))
import           Control.Concurrent                                (threadDelay)
import           Control.Monad                                     (forM_, when, (<=<))
import           Control.Monad.Except                              (MonadError, MonadIO, liftIO, throwError)
import           Data.Maybe                                        (maybeToList)
import           Language.Marlowe.CLI.IO                           (decodeFileBuiltinData, liftCli, liftCliIO,
                                                                    readSigningKey)
import           Language.Marlowe.CLI.Types                        (CliError (..), PayFromScript (..), PayToScript (..),
                                                                    SomePaymentSigningKey)
import           Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult (..))
import           Plutus.V1.Ledger.Api                              (Datum (..), Redeemer (..), toData)
import           System.IO                                         (hPutStrLn, stderr)

import qualified Data.ByteString                                   as BS (length)
import qualified Data.Map.Strict                                   as M (elems, keysSet)
import qualified Data.Set                                          as S (empty, fromList)


-- | Build a non-Marlowe transaction.
buildSimple :: MonadError CliError m
            => MonadIO m
            => LocalNodeConnectInfo CardanoMode    -- ^ The connection info for the local node.
            -> [FilePath]                          -- ^ The files for required signing keys.
            -> [TxIn]                              -- ^ The transaction inputs.
            -> [(AddressAny, Maybe Datum, Value)]  -- ^ The transaction outputs.
            -> AddressAny                          -- ^ The change address.
            -> FilePath                            -- ^ The output file for the transaction body.
            -> Maybe Int                           -- ^ Number of seconds to wait for the transaction to be confirmed, if it is to be confirmed.
            -> Bool                                -- ^ Whether to print statistics about the transaction.
            -> Bool                                -- ^ Assertion that the transaction is invalid.
            -> m TxId                              -- ^ Action to build the transaction body.
buildSimple connection signingKeyFiles inputs outputs changeAddress bodyFile timeout printStats invalid =
  do
    signingKeys <- mapM readSigningKey signingKeyFiles
    body <-
      buildBody connection
        []
        Nothing
        inputs outputs Nothing changeAddress
        Nothing
        []
        printStats
        invalid
    liftCliIO
      $ writeFileTextEnvelope bodyFile Nothing body
    forM_ timeout
      $ if invalid
          then const $ throwError "Refusing to submit an invalid transaction: collateral would be lost."
          else submitBody connection body signingKeys
    pure
      $ getTxId body


-- | Build a transaction paying into a Marlowe contract.
buildIncoming :: MonadError CliError m
              => MonadIO m
              => LocalNodeConnectInfo CardanoMode    -- ^ The connection info for the local node.
              -> AddressAny                          -- ^ The script address.
              -> [FilePath]                          -- ^ The files for required signing keys.
              -> FilePath                            -- ^ The file containing the datum for the payment to the script.
              -> Value                               -- ^ The value to be paid to the script.
              -> [TxIn]                              -- ^ The transaction inputs.
              -> [(AddressAny, Maybe Datum, Value)]  -- ^ The transaction outputs.
              -> AddressAny                          -- ^ The change address.
              -> FilePath                            -- ^ The output file for the transaction body.
              -> Maybe Int                           -- ^ Number of seconds to wait for the transaction to be confirmed, if it is to be confirmed.
              -> Bool                                -- ^ Whether to print statistics about the transaction.
              -> Bool                                -- ^ Assertion that the transaction is invalid.
              -> m TxId                              -- ^ Action to build the transaction body.
buildIncoming connection scriptAddress signingKeyFiles outputDatumFile outputValue inputs outputs changeAddress bodyFile timeout printStats invalid =
  do
    scriptAddress' <- asAlonzoAddress "Failed to converting script address to Alonzo era." scriptAddress
    outputDatum <- Datum <$> decodeFileBuiltinData outputDatumFile
    signingKeys <- mapM readSigningKey signingKeyFiles
    body <-
      buildBody connection
        []
        (Just $ buildPayToScript scriptAddress' outputValue outputDatum)
        inputs outputs Nothing changeAddress
        Nothing
        []
        printStats
        invalid
    liftCliIO
      $ writeFileTextEnvelope bodyFile Nothing body
    forM_ timeout
      $ if invalid
          then const $ throwError "Refusing to submit an invalid transaction: collateral would be lost."
          else submitBody connection body signingKeys
    pure
      $ getTxId body


-- | Build a transaction that spends from and pays to a Marlowe contract.
buildContinuing :: MonadError CliError m
                => MonadIO m
                => LocalNodeConnectInfo CardanoMode    -- ^ The connection info for the local node.
                -> AddressAny                          -- ^ The script address.
                -> FilePath                            -- ^ The file containing the script validator.
                -> FilePath                            -- ^ The file containing the redeemer.
                -> FilePath                            -- ^ The file containing the datum for spending from the script.
                -> [FilePath]                          -- ^ The files for required signing keys.
                -> TxIn                                -- ^ The script eUTxO to be spent.
                -> FilePath                            -- ^ The file containing the datum for the payment to the script.
                -> Value                               -- ^ The value to be paid to the script.
                -> [TxIn]                              -- ^ The transaction inputs.
                -> [(AddressAny, Maybe Datum, Value)]  -- ^ The transaction outputs.
                -> TxIn                                -- ^ The collateral.
                -> AddressAny                          -- ^ The change address.
                -> SlotNo                              -- ^ The first valid slot for the transaction.
                -> SlotNo                              -- ^ The last valid slot for the transaction.
                -> FilePath                            -- ^ The output file for the transaction body.
                -> Maybe Int                           -- ^ Number of seconds to wait for the transaction to be confirmed, if it is to be confirmed.
                -> Bool                                -- ^ Whether to print statistics about the transaction.
                -> Bool                                -- ^ Assertion that the transaction is invalid.
                -> m TxId                              -- ^ Action to build the transaction body.
buildContinuing connection scriptAddress validatorFile redeemerFile inputDatumFile signingKeyFiles txIn outputDatumFile outputValue inputs outputs collateral changeAddress minimumSlot maximumSlot bodyFile timeout printStats invalid =
  do
    scriptAddress' <- asAlonzoAddress "Failed to converting script address to Alonzo era." scriptAddress
    validator <- liftCliIO (readFileTextEnvelope (AsPlutusScript AsPlutusScriptV1) validatorFile)
    redeemer <- Redeemer <$> decodeFileBuiltinData redeemerFile
    inputDatum <- Datum <$> decodeFileBuiltinData inputDatumFile
    outputDatum <- Datum <$> decodeFileBuiltinData outputDatumFile
    signingKeys <- mapM readSigningKey signingKeyFiles
    body <-
      buildBody connection
        [buildPayFromScript validator inputDatum redeemer txIn]
        (Just $ buildPayToScript scriptAddress' outputValue outputDatum)
        inputs outputs (Just collateral) changeAddress
        (Just (minimumSlot, maximumSlot))
        (hashSigningKey <$> signingKeys)
        printStats
        invalid
    liftCliIO
      $ writeFileTextEnvelope bodyFile Nothing body
    forM_ timeout
      $ if invalid
          then const $ throwError "Refusing to submit an invalid transaction: collateral would be lost."
          else submitBody connection body signingKeys
    pure
      $ getTxId body


-- | Build a transaction spending from a Marlowe contract.
buildOutgoing :: MonadError CliError m
              => MonadIO m
              => LocalNodeConnectInfo CardanoMode    -- ^ The connection info for the local node.
              -> FilePath                            -- ^ The file containing the script validator.
              -> FilePath                            -- ^ The file containing the redeemer.
              -> FilePath                            -- ^ The file containing the datum for spending from the script.
              -> [FilePath]                          -- ^ The files for required signing keys.
              -> TxIn                                -- ^ The script eUTxO to be spent.
              -> [TxIn]                              -- ^ The transaction inputs.
              -> [(AddressAny, Maybe Datum, Value)]  -- ^ The transaction outputs.
              -> TxIn                                -- ^ The collateral.
              -> AddressAny                          -- ^ The change address.
              -> SlotNo                              -- ^ The first valid slot for the transaction.
              -> SlotNo                              -- ^ The last valid slot for the transaction.
              -> FilePath                            -- ^ The output file for the transaction body.
              -> Maybe Int                           -- ^ Number of seconds to wait for the transaction to be confirmed, if it is to be confirmed.
              -> Bool                                -- ^ Whether to print statistics about the transaction.
              -> Bool                                -- ^ Assertion that the transaction is invalid.
              -> m TxId                              -- ^ Action to build the transaction body.
buildOutgoing connection validatorFile redeemerFile inputDatumFile signingKeyFiles txIn inputs outputs collateral changeAddress minimumSlot maximumSlot bodyFile timeout printStats invalid =
  do
    validator <- liftCliIO (readFileTextEnvelope (AsPlutusScript AsPlutusScriptV1) validatorFile)
    redeemer <- Redeemer <$> decodeFileBuiltinData redeemerFile
    inputDatum <- Datum <$> decodeFileBuiltinData inputDatumFile
    signingKeys <- mapM readSigningKey signingKeyFiles
    body <-
      buildBody connection
        [buildPayFromScript validator inputDatum redeemer txIn]
        Nothing
        inputs outputs (Just collateral) changeAddress
        (Just (minimumSlot, maximumSlot))
        (hashSigningKey <$> signingKeys)
        printStats
        invalid
    liftCliIO
      $ writeFileTextEnvelope bodyFile Nothing body
    forM_ timeout
      $ if invalid
          then const $ throwError "Refusing to submit an invalid transaction: collateral would be lost."
          else submitBody connection body signingKeys
    pure $ getTxId body


-- | Collect information on paying from a script.
buildPayFromScript :: PlutusScript PlutusScriptV1  -- ^ The script.
                   -> Datum                        -- ^ The datum.
                   -> Redeemer                     -- ^ The redeemer.
                   -> TxIn                         -- ^ The eUTxO to be spent.
                   -> PayFromScript                -- ^ Payment information.
buildPayFromScript script datum redeemer txIn = PayFromScript{..}


-- | Collect information on paying to a script.
buildPayToScript :: AddressInEra era  -- ^ The script address.
                 -> Value             -- ^ The value to be paid.
                 -> Datum             -- ^ The datum.
                 -> PayToScript era   -- ^ The payment information.
buildPayToScript address value datum =
  let
    datumOut  = fromPlutusData . toData $ datum
    datumHash = hashScriptData datumOut
  in
    PayToScript{..}


-- | Hash a signing key.
hashSigningKey :: SomePaymentSigningKey  -- ^ The key.
               -> Hash PaymentKey        -- ^ The hash.
hashSigningKey =
  verificationKeyHash
    . either
        getVerificationKey
        (castVerificationKey . getVerificationKey)


-- | Build a balanced transaction body.
buildBody :: MonadError CliError m
          => MonadIO m
          => LocalNodeConnectInfo CardanoMode    -- ^ The connection info for the local node.
          -> [PayFromScript]                     -- ^ Payment information from the script, if any.
          -> Maybe (PayToScript AlonzoEra)       -- ^ Payment information to the script, if any.
          -> [TxIn]                              -- ^ Transaction inputs.
          -> [(AddressAny, Maybe Datum, Value)]  -- ^ Transaction outputs.
          -> Maybe TxIn                          -- ^ Collateral, if any.
          -> AddressAny                          -- ^ The change address.
          -> Maybe (SlotNo, SlotNo)              -- ^ The valid slot range, if any.
          -> [Hash PaymentKey]                   -- ^ The extra required signatures.
          -> Bool                                -- ^ Whether to print statistics about the transaction.
          -> Bool                                -- ^ Assertion that the transaction is invalid.
          -> m (TxBody AlonzoEra)                -- ^ The action to build the transaction body.
buildBody connection payFromScript payToScript inputs outputs collateral changeAddress slotRange extraSigners printStats invalid =
  do
    changeAddress' <- asAlonzoAddress "Failed converting change address to Alonzo era." changeAddress
    start    <- queryAny    connection   QuerySystemStart
    history  <- queryAny    connection $ QueryEraHistory CardanoModeIsMultiEra
    protocol <- queryAlonzo connection   QueryProtocolParameters
    let
      txInsCollateral   = TxInsCollateral CollateralInAlonzoEra $ maybeToList collateral
      txFee             = TxFeeExplicit TxFeesExplicitInAlonzoEra 0
      txValidityRange   = (
                            maybe
                              TxValidityNoLowerBound
                              (TxValidityLowerBound ValidityLowerBoundInAlonzoEra . fst)
                              slotRange
                          , maybe
                              (TxValidityNoUpperBound ValidityNoUpperBoundInAlonzoEra)
                              (TxValidityUpperBound ValidityUpperBoundInAlonzoEra . snd)
                              slotRange
                          )
      txMetadata        = TxMetadataNone
      txAuxScripts      = TxAuxScriptsNone
      txExtraKeyWits    = TxExtraKeyWitnesses ExtraKeyWitnessesInAlonzoEra extraSigners
      txProtocolParams  = BuildTxWith $ Just protocol
      txWithdrawals     = TxWithdrawalsNone
      txCertificates    = TxCertificatesNone
      txUpdateProposal  = TxUpdateProposalNone
      txMintValue       = TxMintNone
      txScriptValidity  = if invalid
                            then TxScriptValidity TxScriptValiditySupportedInAlonzoEra ScriptInvalid
                            else TxScriptValidityNone
      scriptTxIn = redeemScript <$> payFromScript
      txIns = scriptTxIn <> fmap makeTxIn inputs
      scriptTxOut = maybe [] payScript payToScript
    txOuts <- (scriptTxOut <>) <$> mapM (uncurry3 makeTxOut) outputs
    utxo <-
      queryAlonzo connection
        . QueryUTxO
        . QueryUTxOByTxIn
        . S.fromList
        $ fst
        <$> txIns
    -- Compute the change.
    BalancedTxBody _ change _ <-
      liftCli
        $ makeTransactionBodyAutoBalance
            AlonzoEraInCardanoMode
            start
            history
            protocol
            S.empty
            utxo
            TxBodyContent{..}
            changeAddress'
            Nothing
    let
      -- Recompute execution units with full set of UTxOs, including change.
      trial =
        makeTransactionBodyAutoBalance
          AlonzoEraInCardanoMode
          start
          history
          protocol
          S.empty
          utxo
          (TxBodyContent{..} {txOuts = change : txOuts})
          changeAddress'
          Nothing
      -- Correct for a negative balance in cases where execution units, and hence fees, have increased.
      change' =
        case (change, trial) of
          (TxOut _ (TxOutValue _ value) _, Left (TxBodyErrorAdaBalanceNegative delta)) -> TxOut
                                                                                            changeAddress'
                                                                                            (
                                                                                              TxOutValue MultiAssetInAlonzoEra
                                                                                                $ value <> lovelaceToValue delta
                                                                                            )
                                                                                            TxOutDatumNone
          _                                                                            -> change
    -- Construct the body with correct execution units and fees.
    BalancedTxBody txBody _ lovelace <-
      liftCli
        $ makeTransactionBodyAutoBalance
            AlonzoEraInCardanoMode
            start
            history
            protocol
            S.empty
            utxo
            (TxBodyContent{..} {txOuts = change' : txOuts})
            changeAddress'
            Nothing
    when printStats
      . liftIO
      $ do
        hPutStrLn stderr ""
        hPutStrLn stderr $ "Fee: " <> show lovelace
        let
          size = BS.length $ serialiseToCBOR txBody
          maxSize = fromIntegral $ protocolParamMaxTxSize protocol
          fractionSize = 100 * size `div` maxSize
        hPutStrLn stderr $ "Size: " <> show size <> " / " <> show maxSize <> " = " <> show fractionSize <> "%"
        let
          ExUnits memory steps = findExUnits txBody
          maxExecutionUnits = protocolParamMaxTxExUnits protocol
          fractionMemory = 100 * memory `div` maybe 0 executionMemory maxExecutionUnits
          fractionSteps  = 100 * steps  `div` maybe 0 executionSteps  maxExecutionUnits
        hPutStrLn stderr "Execution units:"
        hPutStrLn stderr $ "  Memory: " <> show memory <> " / " <> show (maybe 0 executionMemory maxExecutionUnits) <> " = " <> show fractionMemory <> "%"
        hPutStrLn stderr $ "  Steps: "  <> show steps  <> " / " <> show (maybe 0 executionSteps  maxExecutionUnits) <> " = " <> show fractionSteps  <> "%"
    return txBody


-- | Total the execution units in a transaction.
findExUnits :: TxBody AlonzoEra  -- ^ The transaction body.
            -> ExUnits           -- ^ The execution units.
findExUnits (ShelleyTxBody ShelleyBasedEraAlonzo  _ _ (TxBodyScriptData _ _ (Redeemers redeemers)) _ _) =
  mconcat . fmap snd . M.elems $ redeemers
findExUnits _ = mempty


-- | Sign and submit a transaction.
submit :: MonadError CliError m
       => MonadIO m
       => LocalNodeConnectInfo CardanoMode  -- ^ The connection info for the local node.
       -> FilePath                          -- ^ The transaction body file.
       -> [FilePath]                        -- ^ The signing key files.
       -> Int                               -- ^ Number of seconds to wait for the transaction to be confirmed.
       -> m TxId                            -- ^ The action to submit the transaction.
submit connection bodyFile signingKeyFiles timeout =
  do
    body <- liftCliIO $ readFileTextEnvelope (AsTxBody AsAlonzoEra) bodyFile
    signings <- mapM readSigningKey signingKeyFiles
    submitBody connection body signings timeout


-- | Sign and submit a transaction.
submitBody :: MonadError CliError m
           => MonadIO m
           => LocalNodeConnectInfo CardanoMode  -- ^ The connection info for the local node.
           -> TxBody AlonzoEra                  -- ^ The transaction body.
           -> [SomePaymentSigningKey]           -- ^ The signing keys.
           -> Int                               -- ^ Number of seconds to wait for the transaction to be confirmed.
           -> m TxId                            -- ^ The action to submit the transaction.
submitBody connection body signings timeout =
  do
    let
      tx =
        signShelleyTransaction body
          $ either WitnessPaymentKey WitnessPaymentExtendedKey
          <$> signings
    result <-
      liftIO
        . submitTxToNodeLocal connection
        $ TxInMode tx AlonzoEraInCardanoMode
    case result of
      SubmitSuccess     -> do
                             let
                               txId = getTxId body
                             when (timeout > 0)
                               $ waitForUtxos connection timeout [TxIn txId $ TxIx 0]
                             pure txId
      SubmitFail reason -> throwError . CliError $ show reason


-- | Wait for transactions to be confirmed as UTxOs.
waitForUtxos :: MonadError CliError m
             => MonadIO m
             => LocalNodeConnectInfo CardanoMode  -- ^ The connection info for the local node.
             -> Int                               -- ^ Number of seconds to wait for the transaction to be confirmed.
             -> [TxIn]                            -- ^ The transactions to wait for.
             -> m ()                              -- ^ Action to wait for the transaction confirmations.
waitForUtxos connection timeout txIns =
  let
    pause = 5
    txIns' = S.fromList txIns
    go 0 = throwError "Timeout waiting for transaction to be confirmed."
    go n = do
             liftIO . threadDelay $ pause * 1_000_000
             utxos <-
               queryAlonzo connection
                 . QueryUTxO
                 . QueryUTxOByTxIn
                 $ txIns'
             if M.keysSet (unUTxO utxos) == txIns'
               then pure ()
               else go (n - 1 :: Int)
  in
    go . ceiling $ fromIntegral timeout / (fromIntegral pause :: Double)


-- | Compute the transaction input for paying from a script.
redeemScript :: PayFromScript                                               -- ^ The payment information.
             -> (TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn AlonzoEra))  -- ^ The transaction input.
redeemScript PayFromScript{..} =
  (
    txIn
  , BuildTxWith
      . ScriptWitness ScriptWitnessForSpending
      $ PlutusScriptWitness
        PlutusScriptV1InAlonzo
        PlutusScriptV1
        script
        (ScriptDatumForTxIn . fromPlutusData $ toData datum)
        (fromPlutusData $ toData redeemer)
        (ExecutionUnits 0 0)
  )


-- | Compute the transaction output for paying to a script.
payScript :: PayToScript AlonzoEra  -- ^ The payment information.
          -> [TxOut CtxTx AlonzoEra]      -- ^ The transaction input.
payScript PayToScript{..} =
  [
    TxOut
      address
      (TxOutValue MultiAssetInAlonzoEra value)
      (TxOutDatum ScriptDataInAlonzoEra datumOut)
  ]


-- | Compute transaction input for building a transaction.
makeTxIn :: TxIn                                                        -- ^ The transaction input.
         -> (TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn AlonzoEra))  -- ^ The building for the transaction input.
makeTxIn = (, BuildTxWith $ KeyWitness KeyWitnessForSpending)


-- | Uncurry a triplet.
uncurry3 :: (a1 -> a2 -> a3 -> b)
         -> (a1, a2, a3)
         -> b
uncurry3 f (x, y, z) = f x y z


-- | Compute transaction output for building a transaction.
makeTxOut :: MonadError CliError m
          => AddressAny                 -- ^ The output address.
          -> Maybe Datum                -- ^ The datum, if any.
          -> Value                      -- ^ The output value.
          -> m (TxOut CtxTx AlonzoEra)  -- ^ Action for building the transaction output.
makeTxOut address datum value =
  do
    address' <- asAlonzoAddress "Failed converting output address to Alonzo era." address
    pure
      $ TxOut
        address'
        (TxOutValue MultiAssetInAlonzoEra value)
        (maybe TxOutDatumNone (TxOutDatum ScriptDataInAlonzoEra . fromPlutusData . toData) datum)


-- | Convert an address to Alonzo era.
asAlonzoAddress :: MonadError CliError m
                => String                     -- ^ The error message.
                -> AddressAny                 -- ^ The address.
                -> m (AddressInEra AlonzoEra) -- ^ Action for converting the address.
asAlonzoAddress message =
  liftCli
    . maybe (Left message) Right
    . anyAddressInEra AlonzoEra


-- | Query a node.
queryAny :: MonadError CliError m
         => MonadIO m
         => LocalNodeConnectInfo CardanoMode -- ^ The connection info for the local node.
         -> QueryInMode CardanoMode a        -- ^ The query.
         -> m a                              -- ^ Action for running the query.
queryAny connection =
 liftCliIO
   . queryNodeLocalState connection Nothing


-- | Query an Alonzo-era node.
queryAlonzo :: MonadError CliError m
            => MonadIO m
            => LocalNodeConnectInfo CardanoMode   -- ^ The connection info for the local node.
            -> QueryInShelleyBasedEra AlonzoEra a -- ^ The query.
            -> m a                                -- ^ Action for running the query.
queryAlonzo connection =
  liftCli
    <=< (
          liftCliIO
          . queryNodeLocalState connection Nothing
          . QueryInEra AlonzoEraInCardanoMode
          . QueryInShelleyBasedEra ShelleyBasedEraAlonzo
        )
