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
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Run Marlowe contracts.
module Language.Marlowe.CLI.Run (
  -- * Computation
  adjustMinimumUTxO,
  autoRunTransaction,
  autoRunTransactionImpl,
  initializeTransaction,
  initializeTransactionImpl,
  initializeTransactionUsingScriptRefsImpl,
  makeMarlowe,
  prepareTransaction,
  prepareTransactionImpl,
  runTransaction,
  runTransactionImpl,

  -- * Roles
  autoWithdrawFunds,
  autoWithdrawFundsImpl,
  withdrawFunds,

  -- * Input
  makeChoice,
  makeDeposit,
  makeNotification,

  -- * Party Addresses
  marloweAddressFromCardanoAddress,
  marloweAddressToCardanoAddress,

  -- * Utilities
  toCardanoAddressInEra,
  toCardanoValue,
  toCardanoPolicyId,
) where

import Cardano.Api (
  AddressInEra (..),
  CardanoMode,
  LocalNodeConnectInfo (..),
  NetworkId (..),
  NetworkMagic (..),
  PlutusScriptVersion (PlutusScriptV1, PlutusScriptV2),
  QueryInShelleyBasedEra (..),
  QueryUTxOFilter (..),
  ScriptDataSupportedInEra (..),
  SerialiseAsRawBytes (..),
  SlotNo (..),
  StakeAddressReference (..),
  TxBody,
  TxId,
  TxIn,
  TxMetadataInEra,
  TxMintValue (..),
  TxOut (..),
  TxOutDatum (..),
  UTxO (..),
  getTxId,
  lovelaceToValue,
  txOutValueToValue,
  writeFileTextEnvelope,
 )
import Cardano.Api qualified as Api (Value)
import Cardano.Api qualified as C
import Cardano.Api.Shelley (ReferenceScript (ReferenceScriptNone))
import Cardano.Api.Shelley qualified as CS
import Cardano.Ledger.BaseTypes qualified as LC (Network (..))
import Control.Monad (forM_, guard, unless, void, when)
import Control.Monad.Except (MonadError, MonadIO, catchError, liftIO, throwError)
import Control.Monad.Reader (MonadReader)
import Data.Bifunctor (bimap)
import Data.ByteString qualified as BS
import Data.Function (on)
import Data.Functor ((<&>))
import Data.List (groupBy, sortBy)
import Data.Map.Strict qualified as M (toList)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set qualified as S (fromList, singleton)
import Data.Time.Units (Second)
import Data.Traversable (for)
import Data.Tuple.Extra (uncurry3)
import Language.Marlowe.CLI.Cardano.Api (adjustMinimumUTxO, toTxOutDatumInTx)
import Language.Marlowe.CLI.Cardano.Api.Address (toShelleyStakeReference)
import Language.Marlowe.CLI.Cardano.Api.PlutusScript (IsPlutusScriptLanguage (plutusScriptVersion))
import Language.Marlowe.CLI.Export (
  buildMarloweDatum,
  buildRedeemer,
  buildRoleDatum,
  buildRoleRedeemer,
  marloweValidatorInfo,
  openRoleValidatorInfo,
  payoutValidatorInfo,
 )
import Language.Marlowe.CLI.IO (
  decodeFileStrict,
  getProtocolParams,
  liftCli,
  liftCliIO,
  maybeWriteJson,
  queryByAddress,
  queryInEra,
  queryUTxOs,
  readMaybeMetadata,
  readSigningKey,
 )
import Language.Marlowe.CLI.Merkle (merkleizeMarlowe)
import Language.Marlowe.CLI.Orphans ()
import Language.Marlowe.CLI.Sync (toPlutusAddress)
import Language.Marlowe.CLI.Transaction (
  buildBody,
  buildPayFromScript,
  buildPayToScript,
  ensureMinUtxo,
  findMarloweScriptsRefs,
  hashSigningKey,
  makeTxOut',
  selectCoins,
  submitBody,
 )
import Language.Marlowe.CLI.Types (
  AnUTxO (AnUTxO, unAnUTxO),
  CliEnv,
  CliError (..),
  DatumInfo (..),
  MarlowePlutusVersion,
  MarloweScriptsRefs (MarloweScriptsRefs, mrMarloweValidator, mrRolePayoutValidator),
  MarloweTransaction (..),
  PayFromScript (..),
  PrintStats (PrintStats),
  PublishingStrategy,
  QueryExecutionContext (..),
  RedeemerInfo (..),
  SigningKeyFile (..),
  SomeMarloweTransaction (..),
  SomePaymentSigningKey,
  TxBodyFile (TxBodyFile, unTxBodyFile),
  TxBuildupContext (..),
  ValidatorInfo (..),
  askEra,
  defaultCoinSelectionStrategy,
  doWithCardanoEra,
  mkNodeTxBuildup,
  mrOpenRoleValidator,
  toAddressAny',
  toQueryContext,
  toShelleyAddress,
  txIn,
  validatorInfoScriptOrReference,
  withShelleyBasedEra,
 )
import Language.Marlowe.Client qualified as MC
import Language.Marlowe.Core.V1.Merkle (MerkleizedContract (..), merkleizeInputs)
import Language.Marlowe.Core.V1.Semantics (
  MarloweParams (rolesCurrency),
  Payment (..),
  TransactionInput (..),
  TransactionOutput (..),
  TransactionWarning,
  computeTransaction,
  paymentMoney,
 )
import Language.Marlowe.Core.V1.Semantics.Types (
  AccountId,
  ChoiceId (..),
  ChoiceName,
  ChosenNum,
  Contract,
  Input (..),
  InputContent (..),
  Party (..),
  Payee (..),
  State (accounts),
  Token (..),
  TokenName (..),
  getInputContent,
 )
import Language.Marlowe.Core.V1.Semantics.Types.Address (Network, mainnet, testnet)
import Plutus.ApiCommon (ProtocolVersion)
import Plutus.V1.Ledger.Ada (fromValue)
import Plutus.V1.Ledger.Api (Credential (..), DatumHash (..), fromBuiltin)
import Plutus.V1.Ledger.Api qualified as P
import Plutus.V1.Ledger.SlotConfig (SlotConfig, posixTimeToEnclosingSlot, slotToBeginPOSIXTime, slotToEndPOSIXTime)
import Plutus.V1.Ledger.Value (
  AssetClass (..),
  Value (..),
  assetClass,
  assetClassValue,
  currencyMPSHash,
  flattenValue,
  singleton,
 )
import Plutus.V2.Ledger.Api (
  Address,
  CostModelParams,
  Datum (..),
  MintingPolicyHash (..),
  POSIXTime,
  adaSymbol,
  adaToken,
  toBuiltinData,
 )
import PlutusTx.AssocMap qualified as AM (toList)
import Prettyprinter (Pretty (..))
import System.IO (hPutStrLn, stderr)

-- | Serialise a deposit input to a file.
makeDeposit
  :: (MonadIO m)
  => AccountId
  -- ^ The account for the deposit.
  -> Party
  -- ^ The party making the deposit.
  -> Maybe Token
  -- ^ The token being deposited.
  -> Integer
  -- ^ The amount of the token deposited.
  -> Maybe FilePath
  -- ^ The output JSON file representing the input.
  -> m ()
  -- ^ Action to write the input to the file.
makeDeposit accountId party token amount outputFile =
  maybeWriteJson outputFile
    . NormalInput
    $ IDeposit accountId party (fromMaybe (Token adaSymbol adaToken) token) amount

-- | Serialise a choice input to a file.
makeChoice
  :: (MonadIO m)
  => ChoiceName
  -- ^ The name of the choice made.
  -> Party
  -- ^ The party making the choice.
  -> ChosenNum
  -- ^ The number chosen.
  -> Maybe FilePath
  -- ^ The output JSON file representing the input.
  -> m ()
  -- ^ Action to write the input to the file.
makeChoice name party chosen outputFile =
  maybeWriteJson outputFile
    . NormalInput
    $ IChoice (ChoiceId name party) chosen

-- | Serialise a notification input to a file.
makeNotification
  :: (MonadIO m)
  => Maybe FilePath
  -- ^ The output JSON file representing the input.
  -> m ()
  -- ^ Action to write the input to the file.
makeNotification outputFile =
  maybeWriteJson outputFile $
    NormalInput INotify

-- | Create an initial Marlowe transaction.
initializeTransaction
  :: (MonadError CliError m)
  => (MonadIO m)
  => (MonadReader (CliEnv era) m)
  => LocalNodeConnectInfo CardanoMode
  -> MarloweParams
  -- ^ The Marlowe contract parameters.
  -> SlotConfig
  -- ^ The POSIXTime-to-slot configuration.
  -> ProtocolVersion
  -> CostModelParams
  -- ^ The cost model parameters.
  -> NetworkId
  -- ^ The network ID.
  -> StakeAddressReference
  -- ^ The stake address.
  -> FilePath
  -- ^ The JSON file containing the contract.
  -> FilePath
  -- ^ The JSON file containing the contract's state.
  -> Maybe (PublishingStrategy era)
  -> Maybe FilePath
  -- ^ The output JSON file for the validator information.
  -> Bool
  -- ^ Whether to deeply merkleize the contract.
  -> Bool
  -- ^ Whether to print statistics about the validator.
  -> m ()
  -- ^ Action to export the validator information to a file.
initializeTransaction connection marloweParams slotConfig protocolVersion costModelParams network stake contractFile stateFile publishingStrategy outputFile merkleize printStats =
  do
    era <- askEra
    refs <- case publishingStrategy of
      Nothing -> pure Nothing
      Just publishingStrategy' ->
        withShelleyBasedEra era $
          findMarloweScriptsRefs (QueryNode connection) publishingStrategy' (PrintStats printStats)
    contract <- decodeFileStrict contractFile
    state <- decodeFileStrict stateFile
    marloweTransaction <-
      withShelleyBasedEra era $
        initializeTransactionImpl
          marloweParams
          slotConfig
          protocolVersion
          costModelParams
          network
          stake
          contract
          state
          refs
          merkleize
          printStats
    maybeWriteJson outputFile $
      SomeMarloweTransaction
        (plutusScriptVersion :: PlutusScriptVersion MarlowePlutusVersion)
        era
        marloweTransaction

-- | Create an initial Marlowe transaction.
initializeTransactionImpl
  :: forall lang m era
   . (MonadError CliError m)
  => (MonadIO m)
  => (C.IsShelleyBasedEra era)
  => (MonadReader (CliEnv era) m)
  => (IsPlutusScriptLanguage lang)
  => MarloweParams
  -- ^ The Marlowe contract parameters.
  -> SlotConfig
  -- ^ The POSIXTime-to-slot configuration.
  -> ProtocolVersion
  -> CostModelParams
  -- ^ The cost model parameters.
  -> NetworkId
  -- ^ The network ID.
  -> StakeAddressReference
  -- ^ The stake address.
  -> Contract
  -- ^ The initial Marlowe contract.
  -> State
  -- ^ The initial Marlowe state.
  -> Maybe (MarloweScriptsRefs lang era)
  -> Bool
  -- ^ Whether to deeply merkleize the contract.
  -> Bool
  -- ^ Whether to print statistics about the validator.
  -> m (MarloweTransaction lang era)
  -- ^ Action to return a MarloweTransaction
initializeTransactionImpl marloweParams mtSlotConfig protocolVersion costModelParams network stake mtContract mtState refs merkleize printStats = case plutusScriptVersion @lang of
  PlutusScriptV1 -> throwError "Plutus Script V1 not supported"
  PlutusScriptV2 -> do
    era <- askEra
    let mtRolesCurrency = rolesCurrency marloweParams
    (mtValidator, mtRoleValidator, mtOpenRoleValidator) <-
      case refs of
        Nothing -> do
          mv <- liftCli $ marloweValidatorInfo era protocolVersion costModelParams network stake
          rv <- liftCli $ payoutValidatorInfo era protocolVersion costModelParams network stake
          ov <- liftCli $ openRoleValidatorInfo era protocolVersion costModelParams network stake
          pure (mv, rv, ov)
        Just MarloweScriptsRefs{..} -> do
          let vi = snd mrMarloweValidator
          vi' <-
            case toShelleyAddress $ viAddress vi of
              Nothing -> throwError "Expecting shelley address in reference validator info"
              Just (CS.ShelleyAddress n p _) ->
                pure $
                  vi
                    { viAddress =
                        C.shelleyAddressInEra $
                          CS.ShelleyAddress n p $
                            toShelleyStakeReference stake
                    }
          pure (vi', snd mrRolePayoutValidator, snd mrOpenRoleValidator)
    let ValidatorInfo{..} = mtValidator
        mtContinuations = mempty
        mtRange = Nothing
        mtInputs = []
        mtPayments = []
    liftIO $
      when printStats $
        do
          hPutStrLn stderr ""
          hPutStrLn stderr $ "Validator size: " <> show viSize
          hPutStrLn stderr $ "Base-validator cost: " <> show viCost
    let marloweTransaction = MarloweTransaction{..}
    pure $ if merkleize then merkleizeMarlowe marloweTransaction else marloweTransaction

-- | Create an initial Marlowe transaction using reference scripts.
initializeTransactionUsingScriptRefsImpl
  :: forall era lang m
   . (MonadError CliError m)
  => (MonadIO m)
  => (C.IsShelleyBasedEra era)
  => MarloweParams
  -- ^ The Marlowe contract parameters.
  -> SlotConfig
  -- ^ The POSIXTime-to-slot configuration.
  -> MarloweScriptsRefs lang era
  -- ^ The reference scripts.
  -> StakeAddressReference
  -- ^ The stake address.
  -> Contract
  -- ^ The initial Marlowe contract.
  -> State
  -- ^ The initial Marlowe state.
  -> Bool
  -- ^ Whether to deeply merkleize the contract.
  -> Bool
  -- ^ Whether to print statistics about the validator.
  -> m (MarloweTransaction lang era)
  -- ^ Action to return a MarloweTransaction
initializeTransactionUsingScriptRefsImpl marloweParams mtSlotConfig scriptRefs stake mtContract mtState merkleize printStats =
  do
    let mtRolesCurrency = rolesCurrency marloweParams
        setupStaking vi@ValidatorInfo{viAddress} = do
          case toShelleyAddress viAddress of
            Nothing -> throwError "Expecting shelley address in reference validator info"
            Just (CS.ShelleyAddress n p _) -> do
              let viAddress' =
                    C.shelleyAddressInEra $
                      CS.ShelleyAddress
                        n
                        p
                        (toShelleyStakeReference stake)
              pure $ vi{viAddress = viAddress'}

        MarloweScriptsRefs{mrMarloweValidator = (_, mv), mrRolePayoutValidator = (_, pv), mrOpenRoleValidator = (_, ov)} = scriptRefs

    mtValidator <- setupStaking mv
    mtRoleValidator <- setupStaking pv
    mtOpenRoleValidator <- setupStaking ov

    let ValidatorInfo{..} = mtValidator
        mtContinuations = mempty
        mtRange = Nothing
        mtInputs = []
        mtPayments = []
    liftIO $
      when printStats $
        do
          hPutStrLn stderr ""
          hPutStrLn stderr $ "Validator size: " <> show viSize
          hPutStrLn stderr $ "Base-validator cost: " <> show viCost
    let marloweTransaction = MarloweTransaction{..}
    pure $ if merkleize then merkleizeMarlowe marloweTransaction else marloweTransaction

-- | Prepare the next step in a Marlowe contract.
prepareTransaction
  :: (MonadError CliError m)
  => (MonadIO m)
  => FilePath
  -- ^ The JSON file with the Marlowe initial state and initial contract.
  -> [Input]
  -- ^ The contract's inputs.
  -> POSIXTime
  -- ^ The first valid time for the transaction.
  -> POSIXTime
  -- ^ The last valid time for the transaction.
  -> Maybe FilePath
  -- ^ The output JSON file with the results of the computation.
  -> Bool
  -- ^ Whether to print statistics about the result.
  -> m ()
  -- ^ Action to compute the next step in the contract.
prepareTransaction marloweFile txInputs minimumTime maximumTime outputFile printStats =
  do
    SomeMarloweTransaction lang era marloweIn <- decodeFileStrict marloweFile
    marloweOut <- prepareTransactionImpl marloweIn txInputs minimumTime maximumTime printStats
    maybeWriteJson outputFile $ SomeMarloweTransaction lang era marloweOut

-- | Implementation of Prepare function
prepareTransactionImpl
  :: (MonadError CliError m)
  => (MonadIO m)
  => MarloweTransaction lang era
  -- ^ Marlowe transaction to be prepared.
  -> [Input]
  -- ^ The contract's inputs.
  -> POSIXTime
  -- ^ The first valid time for the transaction.
  -> POSIXTime
  -- ^ The last valid time for the transaction.
  -> Bool
  -- ^ Whether to print statistics about the result.
  -> m (MarloweTransaction lang era)
  -- ^ Action to compute the next step in the contract.
prepareTransactionImpl marloweIn txInputs minimumTime maximumTime printStats =
  do
    let txInterval = (minimumTime, maximumTime)
        mp = MC.marloweParams $ mtRolesCurrency marloweIn
    (warnings, marloweOut@MarloweTransaction{..}) <- makeMarlowe marloweIn (TransactionInput txInterval txInputs)
    liftIO $
      do
        when printStats $
          do
            hPutStrLn stderr ""
            unless (null warnings) $
              do
                hPutStrLn stderr "Warnings:"
                forM_ warnings $
                  hPutStrLn stderr . ("  " <>) . show
            hPutStrLn stderr $ "Datum size: " <> show (diSize $ buildMarloweDatum mp mtContract mtState)
        sequence_
          [ do
            when printStats $
              do
                hPutStrLn stderr $ "Payment " <> show (i :: Int)
                hPutStrLn stderr $ "  Acccount: " <> show accountId
                hPutStrLn stderr $ "  Payee: " <> show payee
                hPutStrLn stderr $ "  Ada: " <> show (fromValue money)
            sequence_
              [ hPutStrLn stderr $ "  " <> show (pretty symbol) <> "." <> show (pretty token) <> ": " <> show amount
              | (symbol, tokenAmounts) <- AM.toList $ getValue money
              , symbol /= adaSymbol
              , (token, amount) <- AM.toList tokenAmounts
              ]
          | (i, payment@(Payment accountId payee _ _)) <- zip [1 ..] mtPayments
          , let money = paymentMoney payment
          ]
    pure marloweOut

-- | Prepare the next step in a Marlowe contract.
makeMarlowe
  :: (MonadError CliError m)
  => (MonadIO m)
  => MarloweTransaction lang era
  -- ^ The Marlowe initial state and initial contract.
  -> TransactionInput
  -- ^ The transaction input.
  -> m ([TransactionWarning], MarloweTransaction lang era)
  -- ^ Action to compute the next step in the contract.
makeMarlowe marloweIn@MarloweTransaction{..} transactionInput =
  do
    let toSlot = posixTimeToEnclosingSlot mtSlotConfig
        toSlotNo = SlotNo . fromIntegral . toSlot

        roundTxInterval ti@TransactionInput{txInterval = txInterval@(minimumTime, maximumTime)} = do
          let txInterval' =
                ( slotToBeginPOSIXTime mtSlotConfig . toSlot $ minimumTime
                , slotToEndPOSIXTime mtSlotConfig . toSlot $ maximumTime
                )
          when (txInterval' /= txInterval) $
            liftIO $
              hPutStrLn stderr $
                "Rounding  `TransactionInput` txInterval boundries to:" <> show txInterval'
          pure ti{txInterval = txInterval'}

    transactionInput' <- roundTxInterval transactionInput
    liftIO $ print transactionInput'
    transactionInput''@TransactionInput{..} <-
      merkleizeInputs (MerkleizedContract mtContract mtContinuations) mtState transactionInput'
        `catchError` const (pure transactionInput') -- TODO: Consider not catching errors here.
    case computeTransaction transactionInput'' mtState mtContract of
      Error message -> throwError . CliError . show $ message
      TransactionOutput{..} ->
        pure
          ( txOutWarnings
          , marloweIn
              { mtState = txOutState
              , mtContract = txOutContract
              , mtRange = Just $ bimap toSlotNo toSlotNo txInterval
              , mtInputs = txInputs
              , mtPayments = txOutPayments
              }
          )

readMarloweTransactionFile
  :: forall era lang m
   . (MonadError CliError m)
  => (MonadIO m)
  => (C.IsCardanoEra era)
  => PlutusScriptVersion lang
  -> FilePath
  -> m (MarloweTransaction lang era)
readMarloweTransactionFile lang marloweInFile = do
  SomeMarloweTransaction lang' era' marloweIn <- decodeFileStrict marloweInFile
  case (C.cardanoEra :: C.CardanoEra era, era', lang, lang') of
    (C.AlonzoEra, ScriptDataInAlonzoEra, PlutusScriptV1, PlutusScriptV1) -> pure marloweIn
    (C.BabbageEra, C.ScriptDataInBabbageEra, PlutusScriptV2, PlutusScriptV2) -> pure marloweIn
    (C.AlonzoEra, C.ScriptDataInBabbageEra, _, _) -> throwError "Running in Alonzo era, read file in Babbage era"
    (C.BabbageEra, C.ScriptDataInAlonzoEra, _, _) -> throwError "Running in Babbage era, read file in Alonzo era"
    (_, _, _, _) ->
      throwError . CliError $
        "Expecting a different version of plutus in the Marlowe file. Expected: " <> show lang <> " but got: " <> show lang'

-- | Run a Marlowe transaction using FS.
runTransaction
  :: forall era m
   . (MonadError CliError m)
  => (MonadIO m)
  => (MonadReader (CliEnv era) m)
  => LocalNodeConnectInfo CardanoMode
  -- ^ The connection info for the local node.
  -> Maybe (FilePath, TxIn, TxIn)
  -- ^ The JSON file with the Marlowe initial state and initial contract, along with the script eUTxO being spent and the collateral, unless the transaction opens the contract.
  -> FilePath
  -- ^ The JSON file with the Marlowe inputs, final state, and final contract.
  -> [TxIn]
  -- ^ The transaction inputs.
  -> [(AddressInEra era, C.TxOutDatum C.CtxTx era, Api.Value)]
  -- ^ The transaction outputs.
  -> AddressInEra era
  -- ^ The change address.
  -> [SigningKeyFile]
  -- ^ The files for required signing keys.
  -> Maybe FilePath
  -- ^ The file containing JSON metadata, if any.
  -> TxBodyFile
  -- ^ The output file for the transaction body.
  -> Maybe Second
  -- ^ Number of seconds to wait for the transaction to be confirmed, if it is to be confirmed.
  -> Bool
  -- ^ Whether to print statistics about the transaction.
  -> Bool
  -- ^ Assertion that the transaction is invalid.
  -> m TxId
  -- ^ Action to build the transaction body.
runTransaction connection marloweInBundle marloweOutFile inputs outputs changeAddress signingKeyFiles metadataFile (TxBodyFile bodyFile) timeout printStats invalid =
  do
    metadata <- readMaybeMetadata metadataFile
    SomeMarloweTransaction _ era' marloweOut' <- decodeFileStrict marloweOutFile
    era <- askEra @era
    signingKeys <- mapM readSigningKey signingKeyFiles
    let go :: forall lang. (IsPlutusScriptLanguage lang) => (C.IsCardanoEra era) => MarloweTransaction lang era -> m TxId
        go marloweOut'' = do
          marloweInBundle' <- case marloweInBundle of
            Nothing -> pure Nothing
            Just (marloweInFile, marloweTxIn, collateralTxIn) -> do
              marloweIn <- readMarloweTransactionFile (plutusScriptVersion :: PlutusScriptVersion lang) marloweInFile
              pure $ Just (marloweIn, marloweTxIn, collateralTxIn)

          (body :: TxBody era) <-
            runTransactionImpl
              (mkNodeTxBuildup connection timeout)
              marloweInBundle'
              marloweOut''
              inputs
              outputs
              changeAddress
              signingKeys
              metadata
              printStats
              invalid
          doWithCardanoEra $ liftCliIO $ writeFileTextEnvelope bodyFile Nothing body
          pure $ getTxId body

    case (era, era') of
      (ScriptDataInAlonzoEra, ScriptDataInAlonzoEra) -> go marloweOut'
      (ScriptDataInBabbageEra, ScriptDataInBabbageEra) -> go marloweOut'
      (ScriptDataInAlonzoEra, ScriptDataInBabbageEra) -> throwError "Running in Alonzo era, read file in Babbage era"
      (ScriptDataInBabbageEra, ScriptDataInAlonzoEra) -> throwError "Running in Babbage era, read file in Alonzo era"

-- | Run a Marlowe transaction.
runTransactionImpl
  :: forall era lang m
   . (MonadError CliError m)
  => (IsPlutusScriptLanguage lang)
  => (MonadIO m)
  => (MonadReader (CliEnv era) m)
  => TxBuildupContext era
  -- ^ The connection info for the local node.
  -> Maybe (MarloweTransaction lang era, TxIn, TxIn)
  -- ^ The Marlowe initial state and initial contract, along with the script eUTxO being spent and the collateral, unless the transaction opens the contract.
  -> MarloweTransaction lang era
  -- ^ The JSON file with the Marlowe inputs, final state, and final contract.
  -> [TxIn]
  -- ^ The transaction inputs.
  -> [(AddressInEra era, C.TxOutDatum C.CtxTx era, Api.Value)]
  -- ^ The transaction outputs.
  -> AddressInEra era
  -- ^ The change address.
  -> [SomePaymentSigningKey]
  -- ^ Required signing keys.
  -> TxMetadataInEra era
  -- ^ Tx metadata.
  -> Bool
  -- ^ Whether to print statistics about the transaction.
  -> Bool
  -- ^ Assertion that the transaction is invalid.
  -> m (TxBody era)
  -- ^ Action to build the transaction body.
runTransactionImpl txBuildupCtx marloweInBundle marloweOut' inputs outputs changeAddress signingKeys metadata printStats invalid =
  do
    let queryCtx = toQueryContext txBuildupCtx
    protocol <- getProtocolParams queryCtx
    era <- askEra @era
    let marloweParams = MC.marloweParams $ mtRolesCurrency marloweOut'
        go :: MarloweTransaction lang era -> m (TxBody era)
        go marloweOut = do
          (spend, collateral, datumOutputs) <-
            case marloweInBundle of
              Nothing -> pure ([], Nothing, [])
              Just (marloweIn, spend, collateral) -> do
                let validatorInfo = mtValidator marloweIn
                    validator = validatorInfoScriptOrReference validatorInfo

                    marloweParams' = MC.marloweParams $ mtRolesCurrency marloweIn
                    redeemer = riRedeemer $ buildRedeemer (mtInputs marloweOut)
                    inputDatum = diDatum $ buildMarloweDatum marloweParams (mtContract marloweIn) (mtState marloweIn)
                    spend' = buildPayFromScript validator inputDatum redeemer spend
                    -- SCP-3610: Remove when Babbage era features become available and the validator is revised.
                    merkles =
                      catMaybes
                        [ case input of
                          NormalInput _ -> Nothing
                          MerkleizedInput _ _ continuation ->
                            Just
                              ( -- Send the ancillary datum to the change address.
                                changeAddress
                              , -- Astonishing that this eUTxO can be spent without script or redeemer!
                                toTxOutDatumInTx era . Datum $ toBuiltinData continuation
                              , -- FIXME: Replace with protocol-dependent min-Ada.
                                lovelaceToValue 1_500_000
                              )
                        | input <- mtInputs marloweOut
                        ]
                when (marloweParams /= marloweParams') $
                  throwError "MarloweParams value is not preserved in continuation"
                pure ([spend'], Just collateral, merkles)
          let scriptAddress = viAddress $ mtValidator marloweOut
              outputDatum = diDatum $ buildMarloweDatum marloweParams (mtContract marloweOut) (mtState marloweOut)
          outputValue <-
            mconcat
              <$> sequence
                [ liftCli . toCardanoValue $ assetClassValue (AssetClass (currency, name)) amount
                | ((_, Token currency name), amount) <- AM.toList . accounts $ mtState marloweOut
                ]
          let continue =
                do
                  guard (outputValue /= mempty)
                  pure $
                    buildPayToScript era scriptAddress outputValue outputDatum

              roleAddress = viAddress $ mtRoleValidator marloweOut
          (payments :: [(AddressInEra era, C.TxOutDatum C.CtxTx era, Api.Value)]) <-
            catMaybes
              <$> sequence
                [ case payee of
                  Party (Address network address) -> do
                    address' <- withShelleyBasedEra era $ marloweAddressToCardanoAddress network address
                    money' <-
                      liftCli $
                        toCardanoValue money
                    (_, money'') <- liftCli $ adjustMinimumUTxO era protocol address' C.TxOutDatumNone money' ReferenceScriptNone
                    pure $ Just (address', C.TxOutDatumNone, money'')
                  Party (Role role) -> do
                    money' <-
                      liftCli $
                        toCardanoValue money
                    let datum = toTxOutDatumInTx era $ diDatum $ buildRoleDatum (Token (rolesCurrency marloweParams) role)
                    (_, money'') <- liftCli $ adjustMinimumUTxO era protocol roleAddress datum money' ReferenceScriptNone
                    pure $ Just (roleAddress, datum, money'')
                  Account _ -> pure Nothing
                | (payee, money) <-
                    bimap head mconcat . unzip
                      <$> (groupBy ((==) `on` fst) . sortBy (compare `on` fst))
                        [ (payee, money)
                        | payment@(Payment _ payee _ _) <- mtPayments marloweOut
                        , let money = paymentMoney payment
                        ]
                ]
          outputs' <- for (payments <> outputs <> datumOutputs) (uncurry3 makeTxOut')
          body <-
            buildBody
              queryCtx
              spend
              continue
              []
              inputs
              outputs'
              collateral
              changeAddress
              (mtRange marloweOut)
              (hashSigningKey <$> signingKeys)
              TxMintNone
              metadata
              printStats
              invalid
          void $ submitBody txBuildupCtx body signingKeys invalid
          pure body
    go marloweOut'

-- | Withdraw funds for a specific role from the role address.
withdrawFunds
  :: (MonadError CliError m, MonadReader (CliEnv era) m)
  => (MonadIO m)
  => LocalNodeConnectInfo CardanoMode
  -- ^ The connection info for the local node.
  -> FilePath
  -- ^ The JSON file with the Marlowe state and contract.
  -> TokenName
  -- ^ The role name for the redemption.
  -> TxIn
  -- ^ The collateral.
  -> [TxIn]
  -- ^ The transaction inputs.
  -> [(AddressInEra era, C.TxOutDatum C.CtxTx era, Api.Value)]
  -- ^ The transaction outputs.
  -> AddressInEra era
  -- ^ The change address.
  -> [SigningKeyFile]
  -- ^ The files for required signing keys.
  -> Maybe FilePath
  -- ^ The file containing JSON metadata, if any.
  -> TxBodyFile
  -- ^ The output file for the transaction body.
  -> Maybe Second
  -- ^ Number of seconds to wait for the transaction to be confirmed, if it is to be confirmed.
  -> Bool
  -- ^ Whether to print statistics about the transaction.
  -> Bool
  -- ^ Assertion that the transaction is invalid.
  -> m TxId
  -- ^ Action to build the transaction body.
withdrawFunds connection marloweOutFile roleName collateral inputs outputs changeAddress signingKeyFiles metadataFile (TxBodyFile bodyFile) timeout printStats invalid =
  do
    metadata <- readMaybeMetadata metadataFile
    SomeMarloweTransaction _ _ marloweOut <- decodeFileStrict marloweOutFile
    let rolesCurrency = mtRolesCurrency marloweOut
        roleToken = Token rolesCurrency roleName
    signingKeys <- mapM readSigningKey signingKeyFiles
    roleHash <- liftCli . toCardanoScriptDataHash . diHash $ buildRoleDatum roleToken
    let validatorInfo = mtRoleValidator marloweOut
        roleScript = validatorInfoScriptOrReference validatorInfo
        roleAddress = viAddress validatorInfo
        roleDatum = diDatum $ buildRoleDatum roleToken
        roleRedeemer = riRedeemer buildRoleRedeemer
        checkRole (TxOut _ _ datum _) =
          case datum of
            TxOutDatumInline _ _ -> False
            TxOutDatumNone -> False
            TxOutDatumHash _ datumHash -> datumHash == roleHash
    utxos <-
      fmap (filter (checkRole . snd) . M.toList . unUTxO)
        . queryInEra connection
        . QueryUTxO
        . QueryUTxOByAddress
        . S.singleton
        . toAddressAny'
        $ roleAddress
    let spend = buildPayFromScript roleScript roleDatum roleRedeemer . fst <$> utxos
        withdrawal = (changeAddress, C.TxOutDatumNone, mconcat [txOutValueToValue value | (_, TxOut _ value _ _) <- utxos])
    outputs' <- mapM (uncurry3 makeTxOut') $ withdrawal : outputs
    body <-
      buildBody
        (QueryNode connection)
        spend
        Nothing
        []
        inputs
        outputs'
        (Just collateral)
        changeAddress
        (mtRange marloweOut)
        (hashSigningKey <$> signingKeys)
        TxMintNone
        metadata
        printStats
        invalid
    doWithCardanoEra $ liftCliIO $ writeFileTextEnvelope bodyFile Nothing body
    let txBuildupCtx = mkNodeTxBuildup connection timeout
    submitBody txBuildupCtx body signingKeys invalid

-- | Run a Marlowe transaction using FS, without selecting inputs or outputs.
autoRunTransaction
  :: forall era m
   . (MonadError CliError m)
  => (MonadIO m)
  => (MonadReader (CliEnv era) m)
  => LocalNodeConnectInfo CardanoMode
  -- ^ The connection info for the local node.
  -> Maybe (FilePath, TxIn)
  -- ^ The JSON file with the Marlowe initial state and initial contract, along with the script eUTxO being spent and the collateral, unless the transaction opens the contract.
  -> FilePath
  -- ^ The JSON file with the Marlowe inputs, final state, and final contract.
  -> AddressInEra era
  -- ^ The change address.
  -> [SigningKeyFile]
  -- ^ The files for required signing keys.
  -> Maybe FilePath
  -- ^ The file containing JSON metadata, if any.
  -> TxBodyFile
  -- ^ The output file for the transaction body.
  -> Maybe Second
  -- ^ Number of seconds to wait for the transaction to be confirmed, if it is to be confirmed.
  -> Bool
  -- ^ Whether to print statistics about the transaction.
  -> Bool
  -- ^ Assertion that the transaction is invalid.
  -> m TxId
  -- ^ Action to build the transaction body.
autoRunTransaction connection marloweInBundle marloweOutFile changeAddress signingKeyFiles metadataFile (TxBodyFile bodyFile) timeout printStats invalid =
  do
    metadata <- readMaybeMetadata metadataFile
    SomeMarloweTransaction _ era' marloweOut' <- decodeFileStrict marloweOutFile
    era <- askEra @era
    signingKeys <- mapM readSigningKey signingKeyFiles
    let go :: forall lang. (IsPlutusScriptLanguage lang) => (C.IsCardanoEra era) => MarloweTransaction lang era -> m TxId
        go marloweOut'' = do
          marloweInBundle' <- case marloweInBundle of
            Nothing -> pure Nothing
            Just (marloweInFile, marloweTxIn) -> do
              marloweIn <- readMarloweTransactionFile (plutusScriptVersion :: PlutusScriptVersion lang) marloweInFile
              pure $ Just (marloweIn, marloweTxIn)

          (body :: TxBody era) <-
            autoRunTransactionImpl
              (mkNodeTxBuildup connection timeout)
              marloweInBundle'
              marloweOut''
              []
              changeAddress
              signingKeys
              metadata
              printStats
              invalid
          -- Write the transaction file.
          doWithCardanoEra $ liftCliIO $ writeFileTextEnvelope bodyFile Nothing body
          pure $ getTxId body

    case (era, era') of
      (ScriptDataInAlonzoEra, ScriptDataInAlonzoEra) -> go marloweOut'
      (ScriptDataInBabbageEra, ScriptDataInBabbageEra) -> go marloweOut'
      (ScriptDataInAlonzoEra, ScriptDataInBabbageEra) -> throwError "Running in Alonzo era, read file in Babbage era"
      (ScriptDataInBabbageEra, ScriptDataInAlonzoEra) -> throwError "Running in Babbage era, read file in Alonzo era"

-- | Run a Marlowe transaction, without selecting inputs or outputs.
autoRunTransactionImpl
  :: forall era lang m
   . (MonadError CliError m)
  => (IsPlutusScriptLanguage lang)
  => (MonadIO m)
  => (MonadReader (CliEnv era) m)
  => TxBuildupContext era
  -- ^ The connection info for the local node.
  -> Maybe (MarloweTransaction lang era, TxIn)
  -- ^ The JSON file with the Marlowe initial state and initial contract, unless the transaction opens the contract.
  -> MarloweTransaction lang era
  -- ^ The JSON file with the Marlowe inputs, final state, and final contract.
  -> [PayFromScript lang]
  -> AddressInEra era
  -- ^ The change address.
  -> [SomePaymentSigningKey]
  -> TxMetadataInEra era
  -- ^ The file containing JSON metadata, if any.
  -> Bool
  -- ^ Whether to print statistics about the transaction.
  -> Bool
  -- ^ Assertion that the transaction is invalid.
  -> m (TxBody era)
  -- ^ Action to build the transaction body.
autoRunTransactionImpl txBuildupCtx marloweInBundle marloweOut' extraSpend changeAddress signingKeys metadata printStats invalid =
  do
    let queryCtx = toQueryContext txBuildupCtx
    protocol <- getProtocolParams queryCtx
    -- Read the Marlowe transaction information for the output.
    -- Fetch the era.
    era <- askEra @era
    let go :: MarloweTransaction lang era -> m (TxBody era)
        go marloweOut = do
          let rolesCurrency = mtRolesCurrency marloweOut
              marloweParams = MC.marloweParams $ mtRolesCurrency marloweOut
          -- Find the script UTxO to spend and the datum for outputs to the role payout address.
          (spend, datumOutputs) <-
            case marloweInBundle of
              -- This is a creation transaction.
              Nothing -> pure (extraSpend, [])
              -- This is a non-creation transaction.
              Just (marloweIn, spend) -> do
                -- Find the results of the previous transaction.
                let -- Fetch the validator.
                    validatorInfo = mtValidator marloweIn
                    validator = validatorInfoScriptOrReference validatorInfo
                    -- Build the redeemer.
                    redeemer = riRedeemer $ buildRedeemer (mtInputs marloweOut)
                    -- Build the datum.
                    inputDatum = diDatum $ buildMarloweDatum marloweParams (mtContract marloweIn) (mtState marloweIn)
                    -- Build the spending witness.
                    spend' = buildPayFromScript validator inputDatum redeemer spend
                    -- Handle merkleization.
                    merkles =
                      catMaybes
                        [ case input of
                          NormalInput _ -> Nothing
                          MerkleizedInput _ _ continuation ->
                            Just
                              ( -- Send the ancillary datum to the change address.
                                changeAddress
                              , -- Astonishing that this eUTxO can be spent without script or redeemer!
                                toTxOutDatumInTx era . Datum $ toBuiltinData continuation
                              , -- FIXME: Replace with protocol-dependent min-Ada.
                                lovelaceToValue 1_500_000
                              )
                        | input <- mtInputs marloweOut
                        ]
                -- Return the spending witness and the extra datum for demerkleization.
                pure (spend' : extraSpend, merkles)
          let -- Compute the script address.
              scriptAddress = viAddress $ mtValidator marloweOut
              -- Build the datum output to the script.
              outputDatum = diDatum $ buildMarloweDatum marloweParams (mtContract marloweOut) (mtState marloweOut)
          -- Determine how much value the script should receive.
          outputValue <-
            mconcat
              <$> sequence
                [ liftCli . toCardanoValue $ assetClassValue (AssetClass (currency, name)) amount
                | ((_, Token currency name), amount) <- AM.toList . accounts $ mtState marloweOut
                ]
          let -- Build the continuing output, if any.
              continue =
                do
                  guard (outputValue /= mempty)
                  pure $
                    buildPayToScript era scriptAddress outputValue outputDatum

              -- Compute the role-payout address.
              roleAddress = viAddress $ mtRoleValidator marloweOut
          -- Build the payments to the role-payout address.
          payments <-
            catMaybes
              <$> sequence
                [ case payee of
                  Party (Address network address) -> do
                    address' <- withShelleyBasedEra era $ marloweAddressToCardanoAddress network address
                    money' <- liftCli $ toCardanoValue money
                    Just <$> ensureMinUtxo protocol (address', C.TxOutDatumNone, money')
                  Party (Role role) -> do
                    money' <- liftCli $ toCardanoValue money
                    let datum = toTxOutDatumInTx era . diDatum $ buildRoleDatum (Token rolesCurrency role)
                    Just <$> ensureMinUtxo protocol (roleAddress, datum, money')
                  Account _ -> pure Nothing
                | (payee, money) <-
                    bimap head mconcat . unzip
                      <$> (groupBy ((==) `on` fst) . sortBy (compare `on` fst))
                        [ (payee, money)
                        | payment@(Payment _ payee _ _) <- mtPayments marloweOut
                        , let money = paymentMoney payment
                        ]
                ]
          let -- Extract the roles needed to authorize the inputs.
              roles [] = []
              roles (input : inputs) =
                case getInputContent input of
                  IDeposit _ (Role role) _ _ -> role : roles inputs
                  IChoice (ChoiceId _ (Role role)) _ -> role : roles inputs
                  _ -> roles inputs
              -- Extract the value coming from a UTxO.
              incomingValue (txIn', TxOut _ value _ _)
                | txIn' `elem` fmap txIn spend = txOutValueToValue value
                | otherwise = mempty
          -- Compute the value coming from the script.
          incoming <- do
            let txIns = S.fromList $ spend <&> \PayFromScript{txIn} -> txIn
            fmap (mconcat . fmap incomingValue . M.toList . unUTxO)
              . queryUTxOs queryCtx
              . QueryUTxOByTxIn
              $ txIns

          -- Build the outputs for role tokens.
          roleOutputs <-
            sequence
              [ do
                value <- liftCli . toCardanoValue $ singleton (mtRolesCurrency marloweOut) role 1
                ensureMinUtxo protocol (changeAddress, C.TxOutDatumNone, value)
              | role <- roles $ mtInputs marloweOut
              ]
          txOuts <-
            traverse
              (uncurry3 makeTxOut')
              (payments <> roleOutputs <> datumOutputs)

          -- Select the coins.
          (collateral, extraInputs, revisedOutputs) <-
            selectCoins
              queryCtx
              incoming
              txOuts
              continue
              changeAddress
              defaultCoinSelectionStrategy
              Nothing
          -- Build the transaction body.
          body <-
            buildBody
              queryCtx
              spend
              continue
              []
              extraInputs
              revisedOutputs
              (Just collateral)
              changeAddress
              (mtRange marloweOut)
              (hashSigningKey <$> signingKeys)
              TxMintNone
              metadata
              printStats
              invalid
          void $ submitBody txBuildupCtx body signingKeys invalid
          -- Return the transaction body.
          pure body
    go marloweOut'

-- | Convert a Marlowe party address to a Cardano address.
marloweAddressToCardanoAddress
  :: forall era m
   . (CS.IsShelleyBasedEra era)
  => (MonadError CliError m)
  => Network
  -> Address
  -> m (AddressInEra era)
marloweAddressToCardanoAddress network address =
  do
    address' <-
      liftCli $
        toCardanoAddressInEra
          (if network == mainnet then Mainnet else Testnet (NetworkMagic 1))
          address
    address'' <-
      case address' of
        AddressInEra (CS.ShelleyAddressInEra _) address'' -> pure address''
        _ -> throwError "Byron addresses are not supported."
    let era = CS.shelleyBasedEra
    pure $ AddressInEra (CS.ShelleyAddressInEra era) address''

-- | Convert a cardano address to a Marlowe party address.
marloweAddressFromCardanoAddress
  :: (MonadError CliError m)
  => AddressInEra era
  -> m (Network, Address)
marloweAddressFromCardanoAddress address =
  do
    network' <-
      case address of
        AddressInEra _ (CS.ShelleyAddress network _ _) -> pure $ if network == LC.Mainnet then mainnet else testnet
        _ -> throwError "Byron addresses are not supported."
    pure (network', toPlutusAddress address)

-- | Withdraw funds for a specific role from the role address, without selecting inputs or outputs.
autoWithdrawFunds
  :: forall era m
   . (MonadError CliError m)
  => (MonadReader (CliEnv era) m)
  => (MonadIO m)
  => (C.IsCardanoEra era)
  => LocalNodeConnectInfo CardanoMode
  -- ^ The connection info for the local node.
  -> FilePath
  -- ^ The JSON file with the Marlowe state and contract.
  -> TokenName
  -- ^ The role name for the redemption.
  -> AddressInEra era
  -- ^ The change address.
  -> [SigningKeyFile]
  -- ^ The files for required signing keys.
  -> Maybe FilePath
  -- ^ The file containing JSON metadata, if any.
  -> TxBodyFile
  -- ^ The output file for the transaction body.
  -> Maybe Second
  -- ^ Number of seconds to wait for the transaction to be confirmed, if it is to be confirmed.
  -> PrintStats
  -- ^ Whether to print statistics about the transaction.
  -> Bool
  -- ^ Assertion that the transaction is invalid.
  -> m TxId
  -- ^ Action to build the transaction body.
autoWithdrawFunds connection marloweOutFile roleName changeAddress signingKeyFiles metadataFile bodyFile timeout printStats invalid =
  do
    era <- askEra
    -- Read the Marlowe transaction information that was used to populate the role-payout address.
    SomeMarloweTransaction _ era' marloweOut <- decodeFileStrict marloweOutFile
    let go :: forall lang. (IsPlutusScriptLanguage lang) => MarloweTransaction lang era -> m TxId
        go marloweOut' = do
          -- Read the signing keys.
          signingKeys <- mapM readSigningKey signingKeyFiles
          -- Read any metadata for the transaction.
          metadata <- readMaybeMetadata metadataFile
          let token = Token (mtRolesCurrency marloweOut') roleName
              -- Compute the validator information.
              rolePayoutValidator = mtRoleValidator marloweOut'

          -- Write the transaction file.
          (txBody :: TxBody era) <-
            autoWithdrawFundsImpl
              (mkNodeTxBuildup connection timeout)
              token
              rolePayoutValidator
              Nothing
              changeAddress
              signingKeys
              Nothing
              metadata
              printStats
              invalid

          doWithCardanoEra $ liftCliIO $ writeFileTextEnvelope (unTxBodyFile bodyFile) Nothing txBody
          pure $ getTxId txBody

    case (era, era') of
      (ScriptDataInAlonzoEra, ScriptDataInAlonzoEra) -> go marloweOut
      (ScriptDataInBabbageEra, ScriptDataInBabbageEra) -> go marloweOut
      (ScriptDataInAlonzoEra, ScriptDataInBabbageEra) -> throwError "Running in Alonzo era, read file in Babbage era"
      (ScriptDataInBabbageEra, ScriptDataInAlonzoEra) -> throwError "Running in Babbage era, read file in Alonzo era"

type FilterPayouts era = [AnUTxO era] -> [AnUTxO era]

-- | Withdraw funds for a specific role from the role address, without selecting inputs or outputs.
autoWithdrawFundsImpl
  :: forall era lang m
   . (MonadError CliError m)
  => (MonadReader (CliEnv era) m)
  => (MonadIO m)
  => (IsPlutusScriptLanguage lang)
  => TxBuildupContext era
  -- ^ The connection info for the local node.
  -> Token
  -- ^ The role token.
  -> ValidatorInfo lang era
  -- ^ The role payout validator.
  -> Maybe (SlotNo, SlotNo)
  -- ^ Transaction validity range.
  -> AddressInEra era
  -- ^ The change address.
  -> [SomePaymentSigningKey]
  -- ^ Required signing keys.
  -> Maybe (FilterPayouts era)
  -- ^ Filtering option so transactoin limits can be imposed.
  -> TxMetadataInEra era
  -- ^ The file containing JSON metadata, if any.
  -> PrintStats
  -- ^ Whether to print statistics about the transaction.
  -> Bool
  -- ^ Assertion that the transaction is invalid.
  -> m (TxBody era)
  -- ^ Action to build the transaction body.
autoWithdrawFundsImpl txBuildupCtx token validatorInfo range changeAddress signingKeys possibleFilter metadata (PrintStats printStats) invalid =
  do
    let queryCtx = toQueryContext txBuildupCtx
    -- Fetch the protocol parameters.
    protocol <- getProtocolParams queryCtx
    let Token rolesCurrency roleName = token
        filterPayoutUtxos = fromMaybe id possibleFilter
        -- Build the datum corresponding to the role name.
        roleDatum = buildRoleDatum token
    -- Compute the hash of the role name.
    roleHash <- liftCli . toCardanoScriptDataHash . diHash $ roleDatum
    let -- Fetch the role-payout validator script.
        roleScript = validatorInfoScriptOrReference validatorInfo
        -- Fetch the role address.
        roleAddress = viAddress validatorInfo
        -- Build the necessary redeemer.
        roleRedeemer = riRedeemer buildRoleRedeemer
        -- Test if a `TxOut` contains the role token datum.
        checkRole (TxOut _ _ datum _) =
          case datum of
            TxOutDatumInline _ _ -> False
            TxOutDatumNone -> False
            TxOutDatumHash _ datumHash -> datumHash == roleHash
    -- Find the role token.
    allPayouts <-
      fmap (map AnUTxO . filter (checkRole . snd) . M.toList . unUTxO)
        . queryByAddress queryCtx
        $ roleAddress
    -- Set the value of one role token.
    role <- liftCli $ toCardanoValue $ singleton rolesCurrency roleName 1
    let utxos = filterPayoutUtxos allPayouts
        -- Build the spending from the script.
        spend = buildPayFromScript roleScript (diDatum roleDatum) roleRedeemer . fst . unAnUTxO <$> utxos
        -- Find how much is being spent from the script.
        -- The output value should include the spending from the script and the role token.
        withdrawn = mconcat [txOutValueToValue value | AnUTxO (_, TxOut _ value _ _) <- utxos]
    -- Ensure that the output meets the min-Ada requirement.
    output <- ensureMinUtxo protocol (changeAddress, C.TxOutDatumNone, withdrawn <> role) >>= uncurry3 makeTxOut'

    -- Select the coins.
    (collateral, extraInputs, revisedOutputs) <-
      selectCoins
        queryCtx
        withdrawn
        [output]
        Nothing
        changeAddress
        defaultCoinSelectionStrategy
        Nothing
    -- Build the transaction body.
    body <-
      buildBody
        queryCtx
        spend
        Nothing
        []
        extraInputs
        revisedOutputs
        (Just collateral)
        changeAddress
        range
        (hashSigningKey <$> signingKeys)
        TxMintNone
        metadata
        printStats
        invalid
    void $ submitBody txBuildupCtx body signingKeys invalid
    -- Return the transaction body.
    pure body

toCardanoValue :: Value -> Either String C.Value
toCardanoValue =
  fmap C.valueFromList . traverse toSingleton . flattenValue
  where
    toSingleton (cs, tn, q) =
      toCardanoAssetId (assetClass cs tn) <&> (,C.Quantity q)

toCardanoAssetId :: AssetClass -> Either String C.AssetId
toCardanoAssetId (AssetClass (currencySymbol, tokenName))
  | currencySymbol == adaSymbol && tokenName == adaToken =
      pure C.AdaAssetId
  | otherwise =
      C.AssetId
        <$> toCardanoPolicyId (currencyMPSHash currencySymbol)
        <*> toCardanoAssetName tokenName

toCardanoAssetName :: TokenName -> Either String C.AssetName
toCardanoAssetName (TokenName bs) =
  maybe (Left "toCardanoAssetName") Right $
    deserialiseFromRawBytes C.AsAssetName (fromBuiltin bs)

toCardanoPolicyId :: MintingPolicyHash -> Either String C.PolicyId
toCardanoPolicyId (MintingPolicyHash bs) =
  maybe (Left "toCardanoPolicyId") Right $
    deserialiseFromRawBytes C.AsPolicyId (fromBuiltin bs)

toCardanoScriptDataHash :: DatumHash -> Either String (C.Hash C.ScriptData)
toCardanoScriptDataHash (DatumHash bs) =
  maybe (Left "toCardanoTxOutDatumHash") Right $
    deserialiseFromRawBytes (C.AsHash C.AsScriptData) (fromBuiltin bs)

toCardanoAddressInEra :: C.NetworkId -> Address -> Either String (C.AddressInEra C.BabbageEra)
toCardanoAddressInEra networkId (P.Address addressCredential addressStakingCredential) =
  C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraBabbage)
    <$> ( C.makeShelleyAddress networkId
            <$> toCardanoPaymentCredential addressCredential
            <*> toCardanoStakeAddressReference addressStakingCredential
        )

toCardanoPaymentCredential :: Credential -> Either String C.PaymentCredential
toCardanoPaymentCredential (PubKeyCredential pubKeyHash) = C.PaymentCredentialByKey <$> toCardanoPaymentKeyHash pubKeyHash
toCardanoPaymentCredential (ScriptCredential validatorHash) = C.PaymentCredentialByScript <$> toCardanoScriptHash validatorHash

toCardanoPaymentKeyHash :: P.PubKeyHash -> Either String (C.Hash C.PaymentKey)
toCardanoPaymentKeyHash (P.PubKeyHash bs) =
  let bsx = fromBuiltin bs
      tg = "toCardanoPaymentKeyHash (" <> show (BS.length bsx) <> " bytes)"
   in maybe (Left tg) Right $ deserialiseFromRawBytes (C.AsHash C.AsPaymentKey) bsx

toCardanoStakeAddressReference :: Maybe P.StakingCredential -> Either String C.StakeAddressReference
toCardanoStakeAddressReference Nothing = pure C.NoStakeAddress
toCardanoStakeAddressReference (Just (P.StakingHash credential)) =
  C.StakeAddressByValue <$> toCardanoStakeCredential credential
toCardanoStakeAddressReference (Just P.StakingPtr{}) = Left "StakingPointersNotSupported"

toCardanoStakeCredential :: Credential -> Either String C.StakeCredential
toCardanoStakeCredential (PubKeyCredential pubKeyHash) = CS.StakeCredentialByKey <$> toCardanoStakeKeyHash pubKeyHash
toCardanoStakeCredential (ScriptCredential validatorHash) = CS.StakeCredentialByScript <$> toCardanoScriptHash validatorHash

toCardanoScriptHash :: P.ValidatorHash -> Either String C.ScriptHash
toCardanoScriptHash (P.ValidatorHash bs) =
  maybe (Left "toCardanoScriptHash") Right $
    deserialiseFromRawBytes C.AsScriptHash $
      fromBuiltin bs

toCardanoStakeKeyHash :: P.PubKeyHash -> Either String (C.Hash C.StakeKey)
toCardanoStakeKeyHash (P.PubKeyHash bs) =
  maybe (Left "toCardanoStakeKeyHash") Right $
    deserialiseFromRawBytes (C.AsHash C.AsStakeKey) $
      fromBuiltin bs
