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


{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}


module Language.Marlowe.CLI.Transaction (
-- * Building
  buildSimple
, buildIncoming
, buildContinuing
, buildOutgoing
-- * Submitting
, submit
) where


import           Cardano.Api                                       (AddressAny, AddressInEra, AlonzoEra, AsType (..),
                                                                    BalancedTxBody (..), BuildTx, BuildTxWith (..),
                                                                    CardanoEra (..), CardanoMode,
                                                                    CollateralSupportedInEra (..),
                                                                    ConsensusModeIsMultiEra (..), EraInMode (..),
                                                                    ExecutionUnits (..), KeyWitnessInCtx (..),
                                                                    LocalNodeConnectInfo, MultiAssetSupportedInEra (..),
                                                                    PaymentKey, PlutusScript, PlutusScriptV1,
                                                                    PlutusScriptVersion (..), QueryInEra (..),
                                                                    QueryInMode (..), QueryInShelleyBasedEra (..),
                                                                    QueryUTxOFilter (..), ScriptDataSupportedInEra (..),
                                                                    ScriptDatum (..), ScriptLanguageInEra (..),
                                                                    ScriptWitness (..), ScriptWitnessInCtx (..),
                                                                    ShelleyBasedEra (..), ShelleyWitnessSigningKey (..),
                                                                    SigningKey, SlotNo, TxAuxScripts (..), TxBody (..),
                                                                    TxBodyContent (..), TxBodyErrorAutoBalance (..),
                                                                    TxCertificates (..), TxExtraKeyWitnesses (..),
                                                                    TxExtraScriptData (..), TxFee (..),
                                                                    TxFeesExplicitInEra (..), TxId, TxIn, TxInMode (..),
                                                                    TxInsCollateral (..), TxMetadataInEra (..),
                                                                    TxMintValue (..), TxOut (..), TxOutDatumHash (..),
                                                                    TxOutValue (..), TxScriptValidity (..),
                                                                    TxUpdateProposal (..), TxValidityLowerBound (..),
                                                                    TxValidityUpperBound (..), TxWithdrawals (..),
                                                                    ValidityLowerBoundSupportedInEra (..),
                                                                    ValidityNoUpperBoundSupportedInEra (..),
                                                                    ValidityUpperBoundSupportedInEra (..), Value,
                                                                    WitCtxTxIn, Witness (..), anyAddressInEra, getTxId,
                                                                    hashScriptData, lovelaceToValue,
                                                                    makeTransactionBodyAutoBalance, queryNodeLocalState,
                                                                    readFileTextEnvelope, signShelleyTransaction,
                                                                    submitTxToNodeLocal, writeFileTextEnvelope)
import           Cardano.Api.Shelley                               (fromPlutusData)
import           Control.Monad                                     ((<=<))
import           Control.Monad.Except                              (MonadError, MonadIO, liftIO, throwError)
import           Data.Maybe                                        (maybeToList)
import           Language.Marlowe.CLI.IO                           (decodeFileBuiltinData)
import           Language.Marlowe.CLI.Types                        (CliError (..), PayFromScript (..), PayToScript (..),
                                                                    liftCli, liftCliIO)
import           Language.Marlowe.Scripts                          (MarloweInput)
import           Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult (..))
import           Plutus.V1.Ledger.Api                              (Datum (..), Redeemer (..), toData)
import           Plutus.V1.Ledger.Slot                             (Slot (..))
import           PlutusTx                                          (fromBuiltinData)

import qualified Data.Set                                          as S (empty, fromList)


buildSimple :: MonadError CliError m
            => MonadIO m
            => LocalNodeConnectInfo CardanoMode -- ^ The connection info for the local node.
            -> [TxIn]
            -> [(AddressAny, Value)]
            -> AddressAny
            -> FilePath
            -> m TxId
buildSimple connection inputs outputs changeAddress bodyFile =
  do
    body <-
      buildBody connection
        Nothing
        Nothing
        inputs outputs Nothing changeAddress
        Nothing
    liftCliIO
      $ writeFileTextEnvelope bodyFile Nothing body
    pure
      $ getTxId body


buildIncoming :: MonadError CliError m
              => MonadIO m
              => LocalNodeConnectInfo CardanoMode -- ^ The connection info for the local node.
              -> AddressAny
              -> FilePath
              -> Value
              -> [TxIn]
              -> [(AddressAny, Value)]
              -> TxIn
              -> AddressAny
              -> FilePath
              -> m TxId
buildIncoming connection scriptAddress outputDatumFile outputValue inputs outputs collateral changeAddress bodyFile =
  do
    scriptAddress' <- asAlonzoAddress "Failed to converting script address to Alonzo era." scriptAddress
    outputDatum <- Datum <$> decodeFileBuiltinData outputDatumFile
    body <-
      buildBody connection
        Nothing
        (Just $ buildPayToScript scriptAddress' outputValue outputDatum)
        inputs outputs (Just collateral) changeAddress
        Nothing
    liftCliIO
      $ writeFileTextEnvelope bodyFile Nothing body
    pure
      $ getTxId body


buildContinuing :: MonadError CliError m
                => MonadIO m
                => LocalNodeConnectInfo CardanoMode -- ^ The connection info for the local node.
                -> AddressAny
                -> FilePath
                -> FilePath
                -> FilePath
                -> TxIn
                -> FilePath
                -> Value
                -> [TxIn]
                -> [(AddressAny, Value)]
                -> TxIn
                -> AddressAny
                -> FilePath
                -> m TxId
buildContinuing connection scriptAddress validatorFile redeemerFile inputDatumFile txIn outputDatumFile outputValue inputs outputs collateral changeAddress bodyFile =
  do
    scriptAddress' <- asAlonzoAddress "Failed to converting script address to Alonzo era." scriptAddress
    validator <- liftCliIO (readFileTextEnvelope (AsPlutusScript AsPlutusScriptV1) validatorFile)
    redeemer <- Redeemer <$> decodeFileBuiltinData redeemerFile
    inputDatum <- Datum <$> decodeFileBuiltinData inputDatumFile
    outputDatum <- Datum <$> decodeFileBuiltinData outputDatumFile
    slotRange <- extractSlotRange redeemer
    body <-
      buildBody connection
        (Just $ buildPayFromScript validator inputDatum redeemer txIn)
        (Just $ buildPayToScript scriptAddress' outputValue outputDatum)
        inputs outputs (Just collateral) changeAddress
        (Just slotRange)
    liftCliIO
      $ writeFileTextEnvelope bodyFile Nothing body
    pure
      $ getTxId body


buildOutgoing :: MonadError CliError m
              => MonadIO m
              => LocalNodeConnectInfo CardanoMode -- ^ The connection info for the local node.
              -> FilePath
              -> FilePath
              -> FilePath
              -> TxIn
              -> [TxIn]
              -> [(AddressAny, Value)]
              -> TxIn
              -> AddressAny
              -> FilePath
              -> m TxId
buildOutgoing connection validatorFile redeemerFile inputDatumFile txIn inputs outputs collateral changeAddress bodyFile =
  do
    validator <- liftCliIO (readFileTextEnvelope (AsPlutusScript AsPlutusScriptV1) validatorFile)
    redeemer <- Redeemer <$> decodeFileBuiltinData redeemerFile
    inputDatum <- Datum <$> decodeFileBuiltinData inputDatumFile
    slotRange <- extractSlotRange redeemer
    body <-
      buildBody connection
        (Just $ buildPayFromScript validator inputDatum redeemer txIn)
        Nothing
        inputs outputs (Just collateral) changeAddress
        (Just slotRange)
    liftCliIO
      $ writeFileTextEnvelope bodyFile Nothing body
    pure
      $ getTxId body


extractSlotRange :: MonadError CliError m
                 => Redeemer
                 -> m (SlotNo, SlotNo)
extractSlotRange (Redeemer redeemer) =
  case PlutusTx.fromBuiltinData redeemer :: Maybe MarloweInput of
     Just ((Slot minimumSlot, Slot maximumSlot), _) -> pure (fromIntegral minimumSlot, fromIntegral maximumSlot)
     Nothing                                        -> throwError "Failed to deserialise redeemer."


buildPayFromScript :: PlutusScript PlutusScriptV1
                   -> Datum
                   -> Redeemer
                   -> TxIn
                   -> PayFromScript
buildPayFromScript script datum redeemer txIn =
  PayFromScript{..}


buildPayToScript :: AddressInEra era
                 -> Value
                 -> Datum
                 -> PayToScript era
buildPayToScript address value datum =
  let
    datumHash = hashScriptData . fromPlutusData $ toData datum
  in
    PayToScript{..}


buildBody :: MonadError CliError m
          => MonadIO m
          => LocalNodeConnectInfo CardanoMode -- ^ The connection info for the local node.
          -> Maybe PayFromScript
          -> Maybe (PayToScript AlonzoEra)
          -> [TxIn]
          -> [(AddressAny, Value)]
          -> Maybe TxIn
          -> AddressAny                       -- ^ The change address.
          -> Maybe (SlotNo, SlotNo)
          -> m (TxBody AlonzoEra)    -- ^ The action to buildBody the transaction.
buildBody connection payFromScript payToScript inputs outputs collateral changeAddress slotRange =
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
      txExtraScriptData = BuildTxWith TxExtraScriptDataNone
      txExtraKeyWits    = TxExtraKeyWitnessesNone
      txProtocolParams  = BuildTxWith $ Just protocol
      txWithdrawals     = TxWithdrawalsNone
      txCertificates    = TxCertificatesNone
      txUpdateProposal  = TxUpdateProposalNone
      txMintValue       = TxMintNone
      txScriptValidity  = TxScriptValidityNone
      scriptTxIn = maybe [] redeemScript payFromScript
      txIns = scriptTxIn <> fmap makeTxIn inputs
      scriptTxOut = maybe [] payScript payToScript
    txOuts <- (scriptTxOut <>) <$> mapM (uncurry makeTxOut) outputs
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
                                                                                            TxOutDatumHashNone
          _                                                                            -> change
    -- Construct the body with correct execution units and fees.
    BalancedTxBody txBody _ _ <-
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
    return txBody


-- | Sign and submit a transaction.
submit :: MonadError CliError m
       => MonadIO m
       => LocalNodeConnectInfo CardanoMode -- ^ The connection info for the local node.
       -> TxBody AlonzoEra                 -- ^ The transaction body.
       -> [SigningKey PaymentKey]          -- ^ The signing key.
       -> m TxId                           -- ^ The action to submit the transaction.
submit connection body signings =
  do
    let
      tx =
        signShelleyTransaction body
          $ WitnessPaymentKey
          <$> signings
    result <-
      liftIO
        . submitTxToNodeLocal connection
        $ TxInMode tx AlonzoEraInCardanoMode
    case result of
      SubmitSuccess     -> pure $ getTxId body
      SubmitFail reason -> throwError . CliError $ show reason


redeemScript :: PayFromScript
             -> [(TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn AlonzoEra))]
redeemScript PayFromScript{..} =
  [
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
  ]


payScript :: PayToScript AlonzoEra
          -> [TxOut AlonzoEra]
payScript PayToScript{..} =
  [
    TxOut
      address
      (TxOutValue MultiAssetInAlonzoEra value)
      (TxOutDatumHash ScriptDataInAlonzoEra datumHash)
  ]


makeTxIn :: TxIn
         -> (TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn AlonzoEra))
makeTxIn = (, BuildTxWith $ KeyWitness KeyWitnessForSpending)


makeTxOut :: MonadError CliError m
          => AddressAny
          -> Value
          -> m (TxOut AlonzoEra)
makeTxOut address value =
  do
    address' <- asAlonzoAddress "Failed converting output address to Alonzo era." address
    pure
      $ TxOut
        address'
        (TxOutValue MultiAssetInAlonzoEra value)
        TxOutDatumHashNone


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
