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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | Analyze Marlowe contracts.
module Language.Marlowe.CLI.Analyze (
  -- * Types
  ContractInstance (..),

  -- * Analysis
  analyze,
  checkExecutionCost,
  calcMarloweTxExBudgets,
) where

import Control.Monad (guard, liftM2, (<=<))
import Control.Monad.Except (MonadError (throwError), MonadIO (..))
import Data.Aeson (object, (.=))
import Data.Bifunctor (bimap)
import Data.Foldable (toList)
import Data.Function (on)
import Data.List (maximumBy, nub, (\\))
import Data.Maybe (catMaybes, isJust)
import Data.String (IsString (..))
import Language.Marlowe.Analysis.Safety.Ledger (worstValueSize)
import Language.Marlowe.Analysis.Safety.Transaction (
  LockedRoles (..),
  MarloweExBudget (..),
  UseReferenceInput (..),
  calcMarloweTxExBudget,
  executeTransaction,
  findTransactions',
  foldTransactionsM,
  inputsRequiredRoles,
  unitAnnotator,
 )
import Language.Marlowe.Analysis.Safety.Types (Transaction (..))
import Language.Marlowe.CLI.Cardano.Api.PlutusScript (IsPlutusScriptLanguage (..), toScriptLanguageInEra)
import Language.Marlowe.CLI.IO (decodeFileStrict, liftCli, liftCliIO, liftCliMaybe)
import Language.Marlowe.CLI.Run (marloweAddressFromCardanoAddress, toCardanoAddressInEra, toCardanoValue)
import Language.Marlowe.CLI.Types (
  CliError (CliError),
  MarloweTransaction (
    MarloweTransaction,
    mtContinuations,
    mtContract,
    mtInputs,
    mtOpenRoleValidator,
    mtPayments,
    mtRange,
    mtRoleValidator,
    mtRolesCurrency,
    mtSlotConfig,
    mtState,
    mtValidator
  ),
  SomeMarloweTransaction (SomeMarloweTransaction),
  ValidatorInfo (..),
  toCardanoEra,
  toCollateralSupportedInEra,
  toExtraKeyWitnessesSupportedInEra,
  toMultiAssetSupportedInEra,
  toTxFeesExplicitInEra,
  toValidityLowerBoundSupportedInEra,
  toValidityUpperBoundSupportedInEra,
  validatorInfoScriptOrReference,
  withShelleyBasedEra,
 )
import Language.Marlowe.Core.V1.Merkle (Continuations, MerkleizedContract (..))
import Language.Marlowe.Core.V1.Plate
import Language.Marlowe.Core.V1.Semantics (
  MarloweData (MarloweData, marloweContract, marloweParams, marloweState),
  MarloweParams (..),
  Payment (Payment),
  TransactionInput (..),
  TransactionOutput (Error, TransactionOutput, txOutContract, txOutPayments, txOutState, txOutWarnings),
  totalBalance,
 )
import Language.Marlowe.Core.V1.Semantics.Types (
  ChoiceId (ChoiceId),
  Contract (..),
  Input (MerkleizedInput),
  InputContent (IChoice, IDeposit),
  Party (Address, Role),
  Payee (Party),
  State (..),
  Token (..),
  getInputContent,
 )
import Language.Marlowe.Core.V1.Semantics.Types.Address (mainnet)
import Language.Marlowe.Scripts.Types (marloweTxInputsFromInputs)

import Cardano.Api (bundleProtocolParams, unsafeHashableScriptData)
import Cardano.Api qualified as Api
import Cardano.Api.Shelley qualified as Api
import Cardano.Ledger.Credential qualified as Shelley
import Control.Monad.Writer (WriterT (..))
import Data.Aeson qualified as A
import Data.Aeson.Types qualified as A (Pair)
import Data.ByteString qualified as BS (length)
import Data.ByteString.Char8 qualified as BS8 (putStr)
import Data.Foldable.Extra (foldlM)
import Data.Map.Strict qualified as M (lookup)
import Data.SatInt (SatInt (..))
import Data.Set qualified as S (filter, member)
import Data.Yaml qualified as Y (encode)
import Plutus.V1.Ledger.Ada qualified as P (lovelaceValueOf)
import Plutus.V1.Ledger.SlotConfig qualified as P (SlotConfig, posixTimeToEnclosingSlot)
import PlutusLedgerApi.V2 qualified as P hiding (evaluateScriptCounting)
import PlutusTx.AssocMap qualified as AM
import PlutusTx.Prelude qualified as P

-- | Analyze a Marlowe contract for protocol-limit or other violations.
analyze
  :: forall m
   . (MonadError CliError m)
  => (MonadIO m)
  => Api.LocalNodeConnectInfo Api.CardanoMode
  -- ^ The connection info for the local node.
  -> FilePath
  -- ^ The JSON file with the Marlowe inputs, final state, and final contract.
  -> Bool
  -- ^ Whether to check preconditions for Marlowe state.
  -> Bool
  -- ^ Whether to check lengths of role names.
  -> Bool
  -- ^ Whether to check lengths of token names.
  -> Bool
  -- ^ Whether to check the `maxValueSize` protocol limit.
  -> Bool
  -- ^ Whether to check the `utxoCostPerWord` protocol limit.
  -> Bool
  -- ^ Whether to check the `maxTxExecutionUnits` protocol limits.
  -> Bool
  -- ^ Whether to check the `maxTxSize` protocol limits.
  -> Bool
  -- ^ Whether to compute tight estimates of worst-case bounds.
  -> Bool
  -- ^ Whether to include worst-case example in output.
  -> m ()
  -- ^ Print estimates of worst-case bounds.
analyze connection marloweFile preconditions roles tokens maximumValue minimumUtxo executionCost transactionSize best verbose =
  do
    SomeMarloweTransaction _ era marlowe <- decodeFileStrict marloweFile
    case era of
      Api.ScriptDataInBabbageEra ->
        do
          protocol <-
            (liftCli <=< liftCliIO)
              . Api.queryNodeLocalState connection Nothing
              . Api.QueryInEra Api.BabbageEraInCardanoMode
              $ Api.QueryInShelleyBasedEra Api.ShelleyBasedEraBabbage Api.QueryProtocolParameters
          result <-
            analyzeImpl
              era
              protocol
              marlowe
              preconditions
              roles
              tokens
              maximumValue
              minimumUtxo
              executionCost
              transactionSize
              best
              verbose
          liftIO . BS8.putStr $ Y.encode result
      _ -> throwError . CliError $ "Analysis not supported for " <> show era <> " era."

-- | A bundle of information describing a contract instance.
data ContractInstance lang era = ContractInstance
  { ciRolesCurrency :: P.CurrencySymbol
  -- ^ The roles currency.
  , ciState :: State
  -- ^ The initial state of the contract.
  , ciContract :: Contract
  -- ^ The initial contract.
  , ciContinuations :: Continuations
  -- ^ The merkleized continuations for the contract.
  , ciSemanticsValidator :: ValidatorInfo lang era
  -- ^ The semantics validator.
  , ciPayoutValidator :: ValidatorInfo lang era
  -- ^ The payout validator.
  , ciOpenRoleValidator :: ValidatorInfo lang era
  -- ^ The open role validator.
  , ciSlotConfig :: P.SlotConfig
  -- ^ The slot configuration.
  }
  deriving (Show)

-- | Analyze a Marlowe contract for protocol-limit or other violations.
analyzeImpl
  :: forall m lang era
   . (Api.IsShelleyBasedEra era)
  => (IsPlutusScriptLanguage lang)
  => (MonadError CliError m)
  => (MonadIO m)
  => Api.ScriptDataSupportedInEra era
  -- ^ The era.
  -> Api.ProtocolParameters
  -- ^ The connection info for the local node.
  -> MarloweTransaction lang era
  -- ^ The the Marlowe inputs, final state, and final contract.
  -> Bool
  -- ^ Whether to check preconditions for Marlowe state.
  -> Bool
  -- ^ Whether to check lengths of role names.
  -> Bool
  -- ^ Whether to check lengths of token names.
  -> Bool
  -- ^ Whether to check the `maxValueSize` protocol limit.
  -> Bool
  -- ^ Whether to check the `utxoCostPerWord` protocol limit.
  -> Bool
  -- ^ Whether to check the `maxTxExecutionUnits` protocol limits.
  -> Bool
  -- ^ Whether to check the `maxTxSize` protocol limits.
  -> Bool
  -- ^ Whether to compute tight estimates of worst-case bounds.
  -> Bool
  -- ^ Whether to include worst-case example in output.
  -> m A.Value
  -- ^ Action for finding estimates of worst-case bounds.
analyzeImpl era protocol MarloweTransaction{..} preconditions roles tokens maximumValue minimumUtxo executionCost transactionSize best verbose =
  do
    let checkAll = not $ preconditions || roles || tokens || maximumValue || minimumUtxo || executionCost || transactionSize
        ci =
          ContractInstance
            mtRolesCurrency
            mtState
            mtContract
            mtContinuations
            mtValidator
            mtRoleValidator
            mtOpenRoleValidator
            mtSlotConfig
    transactions <-
      if checkAll || executionCost || transactionSize || best && (maximumValue || minimumUtxo)
        then findTransactions' unitAnnotator True $ MerkleizedContract mtContract mtContinuations
        else pure mempty
    let perhapsTransactions = if best then Right transactions else Left ci
        guardValue condition x =
          if condition
            then Just <$> x
            else pure Nothing
    A.toJSON
      . catMaybes
      <$> sequence
        [ guardValue (preconditions || checkAll)
            . pure
            $ checkPreconditions ci
        , guardValue (roles || checkAll)
            . pure
            $ checkRoles ci
        , guardValue (tokens || checkAll)
            . pure
            $ checkTokens ci
        , guardValue (maximumValue || checkAll) $
            checkMaximumValue protocol perhapsTransactions verbose
        , guardValue (minimumUtxo || checkAll)
            . withShelleyBasedEra era
            $ checkMinimumUtxo era protocol perhapsTransactions verbose
        , guardValue (executionCost || checkAll) $
            checkExecutionCost protocol ci transactions verbose
        , guardValue (transactionSize || checkAll) $
            checkTransactionSizes era protocol ci transactions verbose
        ]

-- | Report invalid properties in the Marlowe state:
--
--   * Invalid roles currency.
--   * Invalid currency symbol or token name in an account.
--   * Account balances that are not positive.
--   * Duplicate accounts, choices, or bound values.
checkPreconditions
  :: ContractInstance lang era
  -- ^ The bundle of contract information.
  -> A.Value
  -- ^ A report on preconditions.
checkPreconditions ContractInstance{ciRolesCurrency, ciState = State{..}} =
  let nonPositiveBalances = filter ((<= 0) . snd) . AM.toList
      duplicates (AM.keys -> x) = x \\ nub x
   in putJson
        "Preconditions"
        [ "Invalid roles currency" .= invalidCurrency ciRolesCurrency
        , "Invalid account tokens" .= filter invalidToken (snd <$> AM.keys accounts)
        , "Invalid account parties" .= filter invalidParty (fst <$> AM.keys accounts)
        , "Invalid choice parties" .= filter invalidChoiceParty (AM.keys choices)
        , "Non-positive account balances" .= nonPositiveBalances accounts
        , "Duplicate accounts" .= duplicates accounts
        , "Duplicate choices" .= duplicates choices
        , "Duplicate bound values" .= duplicates boundValues
        ]

-- | Detect an invalid currency symbol.
invalidCurrency :: P.CurrencySymbol -> Bool
invalidCurrency currency@P.CurrencySymbol{..} = currency /= P.adaSymbol && P.lengthOfByteString unCurrencySymbol /= 28

-- | Detect an invalid token.
invalidToken :: Token -> Bool
invalidToken (Token currency@P.CurrencySymbol{..} token@P.TokenName{..}) =
  not $
    currency == P.adaSymbol
      && token == P.adaToken
      || P.lengthOfByteString unCurrencySymbol == 28
        && P.lengthOfByteString unTokenName <= 32

-- | Detect an invalid party in a choice.
invalidChoiceParty :: ChoiceId -> Bool
invalidChoiceParty (ChoiceId _ (Role role)) = invalidRole role
invalidChoiceParty _ = False

-- | Detect an invalid party.
invalidParty :: Party -> Bool
invalidParty (Role role) = invalidRole role
invalidParty _ = False

-- | Detect an invalid role name.
invalidRole :: P.TokenName -> Bool
invalidRole P.TokenName{..} = P.lengthOfByteString unTokenName > 32

-- | Check that all tokens are valid.
checkTokens
  :: ContractInstance lang era
  -- ^ The bundle of contract information.
  -> A.Value
  -- ^ A report on token validity.
checkTokens ContractInstance{..} =
  putJson
    "Tokens"
    [ "Invalid tokens" .= invalidToken `S.filter` extractAllWithContinuations ciContract ciContinuations
    ]

-- | Check that all roles are valid.
checkRoles
  :: ContractInstance lang era
  -- ^ The bundle of contract information.
  -> A.Value
  -- ^ Action to print a report on role-name validity.
checkRoles ContractInstance{..} =
  let roles = extractAllWithContinuations ciContract ciContinuations
   in putJson
        "Role names"
        [ "Invalid role names" .= invalidRole `S.filter` roles
        , "Blank role names" .= P.adaToken `S.member` roles
        ]

-- | Check that the protocol limit on maximum value in a UTxO is not violated.
checkMaximumValue
  :: (MonadError CliError m)
  => Api.ProtocolParameters
  -- ^ The `maxValue` protocol parameter.
  -> Either (ContractInstance lang era) [Transaction ()]
  -- ^ The bundle of contract information, or the transactions to traverse.
  -> Bool
  -- ^ Whether to include worst-case example in output.
  -> m A.Value
  -- ^ Action to print a report on `maxValue` validity.
checkMaximumValue Api.ProtocolParameters{protocolParamMaxValueSize = Just maxValue} info verbose =
  let (size, worst) =
        case info of
          Right transactions ->
            let measure tx@(Transaction _ contract _ _ ()) = [(worstValueSize $ extractAll contract, Just tx)]
             in maximumBy (compare `on` fst) $ foldMap measure transactions
          Left ContractInstance{..} -> (worstValueSize $ extractAllWithContinuations ciContract ciContinuations, Nothing)
   in pure $
        putJson "Maximum value" $
          [ "Actual" .= size
          , "Maximum" .= maxValue
          , "Unit" .= ("byte" :: String)
          , "Invalid" .= (size > maxValue)
          , "Percentage" .= (100 * fromIntegral size / fromIntegral maxValue :: Double)
          ]
            <> maybe [] (pure . ("Worst case" .=)) (guard verbose >> worst)
checkMaximumValue _ _ _ = throwError $ CliError "Missing `maxValue` protocol parameter."

-- | Check that the protocol limit on minimum UTxO is not violated.
checkMinimumUtxo
  :: forall lang era m
   . (Api.IsShelleyBasedEra era)
  => (MonadError CliError m)
  => Api.ScriptDataSupportedInEra era
  -- ^ The era.
  -> Api.ProtocolParameters
  -- ^ The protocol parameters.
  -> Either (ContractInstance lang era) [Transaction ()]
  -- ^ The bundle of contract information, or the transactions to traverse.
  -> Bool
  -- ^ Whether to include worst-case example in output.
  -> m A.Value
  -- ^ Action to print a report on the `minimumUTxO` validity.
checkMinimumUtxo era protocol info verbose =
  do
    let compute tokens =
          do
            value <- liftCli (toCardanoValue $ mconcat [P.singleton cs tn 1 | Token cs tn <- toList tokens])
            let address =
                  Api.makeShelleyAddressInEra
                    Api.Mainnet
                    (Api.PaymentCredentialByScript "88888888888888888888888888888888888888888888888888888888")
                    (Api.StakeAddressByValue $ Api.StakeCredentialByKey "99999999999999999999999999999999999999999999999999999999")
                out =
                  Api.TxOut
                    address
                    (Api.TxOutValue (toMultiAssetSupportedInEra era) value)
                    (Api.TxOutDatumHash era "5555555555555555555555555555555555555555555555555555555555555555")
                    Api.ReferenceScriptNone
            liftCli $ Api.calculateMinimumUTxO Api.shelleyBasedEra out <$> bundleProtocolParams (toCardanoEra era) protocol
    (value, worst) <-
      case info of
        Right transactions ->
          let measure tx@(Transaction _ contract _ _ ()) = pure . (,Just tx) <$> compute (extractAll contract)
           in ( maximumBy (compare `on` fst)
                  :: [(Api.Lovelace, Maybe (Transaction ()))] -> (Api.Lovelace, Maybe (Transaction ()))
              )
                <$> foldTransactionsM measure transactions
        Left ContractInstance{..} -> fmap (,Nothing) . compute $ extractAllWithContinuations ciContract ciContinuations
    pure $
      putJson "Minimum UTxO" $
        [ "Requirement" .= value
        ]
          <> maybe [] (pure . ("Worst case" .=)) (guard verbose >> worst)

-- | Check that transactions satisfy the execution-cost protocol limits.
checkExecutionCost
  :: (MonadError CliError m)
  => Api.ProtocolParameters
  -- ^ The protocol parameters.
  -> ContractInstance lang era
  -- ^ The bundle of contract information.
  -> [Transaction ()]
  -- ^ The transaction-paths through the contract.
  -> Bool
  -- ^ Whether to include worst-case example in output.
  -> m A.Value
  -- ^ Action to print a report on validity of transaction execution costs.
checkExecutionCost protocol ContractInstance{..} transactions verbose =
  do
    Api.CostModel costModel <-
      liftCliMaybe "Plutus cost model not found." $
        Api.AnyPlutusScriptVersion Api.PlutusScriptV2 `M.lookup` Api.protocolParamCostModels protocol
    (evaluationContext, _) <- liftCli $ runWriterT $ P.mkEvaluationContext costModel
    let creatorAddress =
          P.Address
            (P.PubKeyCredential "88888888888888888888888888888888888888888888888888888888")
            (Just . P.StakingHash $ P.PubKeyCredential "99999999999999999999999999999999999999999999999999999999")
        referenceInput = do
          let referenceAddress =
                P.Address
                  (P.PubKeyCredential "77777777777777777777777777777777777777777777777777777777")
                  (Just . P.StakingHash $ P.PubKeyCredential "99999999999999999999999999999999999999999999999999999999")
              semanticsHash = P.ScriptHash . P.toBuiltin $ Api.serialiseToRawBytes $ viHash ciSemanticsValidator
          case viTxIn ciSemanticsValidator of
            Nothing -> mempty
            Just _ ->
              pure $
                P.TxInInfo
                  (P.TxOutRef "6666666666666666666666666666666666666666666666666666666666666666" 1)
                  (P.TxOut referenceAddress (P.lovelaceValueOf 1) P.NoOutputDatum (Just semanticsHash))
    semanticsAddress <- fmap snd . liftCli $ marloweAddressFromCardanoAddress $ viAddress ciSemanticsValidator
    payoutAddress <- fmap snd . liftCli $ marloweAddressFromCardanoAddress $ viAddress ciPayoutValidator
    let executor =
          executeTransaction
            evaluationContext
            (viBytes ciSemanticsValidator)
            semanticsAddress
            payoutAddress
            referenceInput
            creatorAddress
            (MarloweParams ciRolesCurrency)
    (steps, memories) <-
      unzip
        . fmap (\(tx, P.ExBudget{..}) -> ((tx, exBudgetCPU), (tx, exBudgetMemory)))
        <$> mapM (liftM2 (<$>) (,) executor) transactions
    let (worstSteps, P.ExCPU (unSatInt -> actualSteps)) = maximumBy (compare `on` snd) steps
        maximumSteps = maybe 0 (fromEnum . Api.executionSteps) (Api.protocolParamMaxTxExUnits protocol)
        (worstMemory, P.ExMemory (unSatInt -> actualMemory)) = maximumBy (compare `on` snd) memories
        maximumMemory = maybe 0 (fromEnum . Api.executionMemory) (Api.protocolParamMaxTxExUnits protocol)
    pure $
      putJson
        "Execution cost"
        [ "Steps"
            .= object
              ( [ "Actual" .= actualSteps
                , "Maximum" .= maximumSteps
                , "Percentage" .= (100 * fromIntegral actualSteps / fromIntegral maximumSteps :: Double)
                , "Invalid" .= (fromEnum actualSteps > maximumSteps)
                ]
                  <> maybe [] (pure . ("Worst case" .=)) (guard verbose >> Just worstSteps)
              )
        , "Memory"
            .= object
              ( [ "Actual" .= actualMemory
                , "Maximum" .= maximumMemory
                , "Percentage" .= (100 * fromIntegral actualMemory / fromIntegral maximumMemory :: Double)
                , "Invalid" .= (fromEnum actualMemory > maximumMemory)
                ]
                  <> maybe [] (pure . ("Worst case" .=)) (guard verbose >> Just worstMemory)
              )
        ]

calcMarloweTxExBudgets
  :: (MonadError CliError m)
  => Api.ProtocolParameters
  -- ^ The protocol parameters.
  -> ContractInstance lang era
  -- ^ The bundle of contract information.
  -> [Transaction ()]
  -- ^ The transaction-path through the contract. It should be a list of *consecutive* transactions.
  -> [P.TokenName]
  -> m [MarloweExBudget]
  -- ^ Action to print a report on validity of transaction execution costs.
calcMarloweTxExBudgets protocol ContractInstance{..} transactionsPath lockedRoles = do
  Api.CostModel costModel <-
    liftCliMaybe "Plutus cost model not found." $
      Api.AnyPlutusScriptVersion Api.PlutusScriptV2 `M.lookup` Api.protocolParamCostModels protocol
  (evaluationContext, _) <- liftCli $ runWriterT $ P.mkEvaluationContext costModel
  let useSemanticsReferenceInput = UseReferenceInput $ isJust $ viTxIn ciSemanticsValidator
      useOperatorReferenceInput = UseReferenceInput $ isJust $ viTxIn ciOpenRoleValidator

  semanticsAddress <- fmap snd . liftCli $ marloweAddressFromCardanoAddress $ viAddress ciSemanticsValidator
  payoutAddress <- fmap snd . liftCli $ marloweAddressFromCardanoAddress $ viAddress ciPayoutValidator
  openRoleAddress <- fmap snd . liftCli $ marloweAddressFromCardanoAddress $ viAddress ciOpenRoleValidator
  let calcTxBudget transaction stillLockedRoles =
        calcMarloweTxExBudget
          evaluationContext
          (viBytes ciSemanticsValidator, semanticsAddress, useSemanticsReferenceInput)
          (viBytes ciOpenRoleValidator, openRoleAddress, useOperatorReferenceInput, LockedRoles stillLockedRoles)
          payoutAddress
          (MarloweParams ciRolesCurrency)
          transaction

      step (budgets, stillLockedRoles) transaction = do
        let Transaction _ _ TransactionInput{txInputs = txInputs} _ () = transaction
            requiredRoles = inputsRequiredRoles txInputs
            stillLockedRoles' = filter (`notElem` requiredRoles) stillLockedRoles
        txBudgets <- calcTxBudget transaction stillLockedRoles
        -- result@(MarloweExBudget _ (_, stillLockedRoles')) <- evaluator transaction stillLockeRoles
        pure (txBudgets : budgets, stillLockedRoles')
  reverse . fst <$> foldlM step ([], lockedRoles) transactionsPath

-- | Check that transactions satisfy the transaction-size protocol limit.
checkTransactionSizes
  :: forall lang era m
   . (Api.IsShelleyBasedEra era)
  => (IsPlutusScriptLanguage lang)
  => (MonadError CliError m)
  => (MonadIO m)
  => Api.ScriptDataSupportedInEra era
  -- ^ The era.
  -> Api.ProtocolParameters
  -- ^ The protocol parameters.
  -> ContractInstance lang era
  -- ^ The bundle of contract information.
  -> [Transaction ()]
  -- ^ The transaction-paths through the contract.
  -> Bool
  -- ^ Whether to include worst-case example in output.
  -> m A.Value
  -- ^ Action to print a report on validity of transaction size.
checkTransactionSizes era protocol ci transactions verbose =
  do
    sizes <- mapM (liftM2 (<$>) (,) $ checkTransactionSize era protocol ci) transactions
    let (worst, actual) = maximumBy (compare `on` snd) sizes
        limit = Api.protocolParamMaxTxSize protocol
    pure $
      putJson "Transaction size" $
        [ "Actual" .= actual
        , "Maximum" .= limit
        , "Percentage" .= (100 * fromIntegral actual / fromIntegral limit :: Double)
        , "Invalid" .= (actual > fromEnum limit)
        ]
          <> maybe [] (pure . ("Worst case" .=)) (guard verbose >> pure worst)

-- | Check that transactions satisfy the transaction-size protocol limit.
checkTransactionSize
  :: forall lang era m
   . (Api.IsShelleyBasedEra era)
  => (IsPlutusScriptLanguage lang)
  => (MonadError CliError m)
  => (MonadIO m)
  => Api.ScriptDataSupportedInEra era
  -- ^ The era.
  -> Api.ProtocolParameters
  -- ^ The protocol parameters.
  -> ContractInstance lang era
  -- ^ The bundle of contract information.
  -> Transaction ()
  -- ^ The transaction-paths through the contract.
  -> m Int
  -- ^ Action to measure the transaction size.
checkTransactionSize era protocol ContractInstance{..} (Transaction marloweState marloweContract TransactionInput{..} output ()) =
  do
    -- We only attend to details that make affect the *size* of the transaction, so
    -- the transaction itself does not actually need to be valid.
    scriptInEra <- liftCliMaybe "Script language not supported in era" $ toScriptLanguageInEra era
    (txOutState, txOutContract, txOutPayments) <-
      case output of
        TransactionOutput{..} -> pure (txOutState, txOutContract, txOutPayments)
        Error e -> throwError . CliError $ show e
    let oneLovelace = Api.lovelaceToValue 1
        creatorAddress =
          Api.makeShelleyAddressInEra
            Api.Mainnet
            (Api.PaymentCredentialByScript "88888888888888888888888888888888888888888888888888888888")
            (Api.StakeAddressByValue $ Api.StakeCredentialByKey "99999999999999999999999999999999999999999999999999999999")
        marloweParams = MarloweParams ciRolesCurrency
        inDatum = MarloweData{..}
        redeemer = marloweTxInputsFromInputs txInputs
        inScript =
          ( Api.TxIn "0000000000000000000000000000000000000000000000000000000000000000" $ Api.TxIx 0
          , Api.BuildTxWith
              . Api.ScriptWitness Api.ScriptWitnessForSpending
              $ Api.PlutusScriptWitness
                scriptInEra
                (plutusScriptVersion @lang)
                (validatorInfoScriptOrReference ciSemanticsValidator)
                (Api.ScriptDatumForTxIn . unsafeHashableScriptData . Api.fromPlutusData $ P.toData inDatum)
                (unsafeHashableScriptData . Api.fromPlutusData $ P.toData redeemer)
                (Api.ExecutionUnits 0 0)
          )
    outValue <- liftCli . toCardanoValue $ totalBalance $ accounts txOutState
    let outScript =
          if txOutContract == Close
            then mempty
            else
              let outDatum = MarloweData marloweParams txOutState txOutContract
               in pure $
                    Api.TxOut
                      (viAddress ciSemanticsValidator)
                      (Api.TxOutValue (toMultiAssetSupportedInEra era) outValue)
                      (Api.TxOutDatumInTx era . unsafeHashableScriptData . Api.fromPlutusData $ P.toData outDatum)
                      Api.ReferenceScriptNone
        makePayment (Payment _ (Party (Address network address)) (Token currency name) amount) =
          do
            value <- liftCli . toCardanoValue $ P.singleton currency name amount
            address' <-
              liftCli $
                toCardanoAddressInEra
                  (if network == mainnet then Api.Mainnet else Api.Testnet $ Api.NetworkMagic 2)
                  address
            address'' <-
              case address' of
                Api.AddressInEra (Api.ShelleyAddressInEra _) address'' -> pure address''
                _ -> throwError $ CliError "Byron addresses are not supported."
            pure
              [ Api.TxOut
                  (Api.AddressInEra (Api.ShelleyAddressInEra Api.shelleyBasedEra) address'')
                  (Api.TxOutValue (toMultiAssetSupportedInEra era) value)
                  Api.TxOutDatumNone
                  Api.ReferenceScriptNone
              ]
        makePayment (Payment _ (Party (Role role)) (Token currency name) amount) =
          do
            value <- liftCli $ toCardanoValue $ P.singleton currency name amount
            pure
              [ Api.TxOut
                  (viAddress ciPayoutValidator)
                  (Api.TxOutValue (toMultiAssetSupportedInEra era) value)
                  (Api.TxOutDatumInTx era . unsafeHashableScriptData . Api.fromPlutusData $ P.toData (ciRolesCurrency, role))
                  Api.ReferenceScriptNone
              ]
        makePayment _ = pure []
    outPayments <- concat <$> mapM makePayment txOutPayments
    let findRole :: InputContent -> [P.TokenName]
        findRole (IDeposit _ (Role role) _ _) = pure role
        findRole (IChoice (ChoiceId _ (Role role)) _) = pure role
        findRole _ = mempty
    roles <-
      liftCli
        . mapM toCardanoValue
        $ fmap (flip (P.singleton ciRolesCurrency) 1) (foldMap (findRole . getInputContent) txInputs)
    let inRoles =
          (,Api.BuildTxWith $ Api.KeyWitness Api.KeyWitnessForSpending)
            . Api.TxIn "1111111111111111111111111111111111111111111111111111111111111111"
            . Api.TxIx
            <$> [1 .. (toEnum $ length roles)]
        outRoles =
          [ Api.TxOut
            creatorAddress
            (Api.TxOutValue (toMultiAssetSupportedInEra era) role)
            Api.TxOutDatumNone
            Api.ReferenceScriptNone
          | role <- roles
          ]
        outMerkle =
          concat
            [ case input of
              MerkleizedInput _ _ contract ->
                pure $
                  Api.TxOut
                    creatorAddress
                    (Api.TxOutValue (toMultiAssetSupportedInEra era) oneLovelace)
                    (Api.TxOutDatumInTx era . unsafeHashableScriptData . Api.fromPlutusData $ P.toData contract)
                    Api.ReferenceScriptNone
              _ -> mempty
            | input <- txInputs
            ]
        inFunds =
          ( Api.TxIn "1111111111111111111111111111111111111111111111111111111111111111" $ Api.TxIx 0
          , Api.BuildTxWith $ Api.KeyWitness Api.KeyWitnessForSpending
          )
        outChange =
          Api.TxOut
            creatorAddress
            (Api.TxOutValue (toMultiAssetSupportedInEra era) oneLovelace)
            Api.TxOutDatumNone
            Api.ReferenceScriptNone
        txIns =
          inFunds
            : inScript
            : inRoles
        txInsCollateral = Api.TxInsCollateral (toCollateralSupportedInEra era) []
        txInsReference = Api.TxInsReferenceNone
        txOuts =
          outChange
            : outScript
              <> outPayments
              <> outRoles
              <> outMerkle
        txReturnCollateral = Api.TxReturnCollateralNone
        txTotalCollateral = Api.TxTotalCollateralNone
        txFee = Api.TxFeeExplicit (toTxFeesExplicitInEra era) 1
        convertSlot = Api.SlotNo . fromIntegral . P.posixTimeToEnclosingSlot ciSlotConfig
        txValidityRange =
          bimap
            (Api.TxValidityLowerBound (toValidityLowerBoundSupportedInEra era) . convertSlot)
            (Api.TxValidityUpperBound (toValidityUpperBoundSupportedInEra era) . convertSlot)
            txInterval
        txMetadata = Api.TxMetadataNone
        txAuxScripts = Api.TxAuxScriptsNone
        txExtraKeyWits =
          Api.TxExtraKeyWitnesses
            (toExtraKeyWitnessesSupportedInEra era)
            $ concat
              [ case address of
                Api.AddressInEra _ (Api.ShelleyAddress _ (Shelley.KeyHashObj pkh) _) -> pure $ Api.PaymentKeyHash pkh
                _ -> mempty
              | Api.TxOut address _ _ _ <- outPayments
              ]
        txProtocolParams = Api.BuildTxWith $ Just protocol
        txWithdrawals = Api.TxWithdrawalsNone
        txCertificates = Api.TxCertificatesNone
        txUpdateProposal = Api.TxUpdateProposalNone
        txMintValue = Api.TxMintNone
        txScriptValidity = Api.TxScriptValidityNone
    body <- liftCli $ Api.createAndValidateTransactionBody Api.TxBodyContent{..}
    keys <-
      liftIO $
        sequence
          [ Api.WitnessPaymentKey <$> Api.generateSigningKey Api.AsPaymentKey
          | let wits = case txExtraKeyWits of
                  Api.TxExtraKeyWitnesses _ wits' -> wits'
                  _ -> mempty
          , _ <- wits
          ]
    let tx = Api.signShelleyTransaction body keys
    pure . BS.length $ Api.serialiseToCBOR tx

-- | Format results of checking as JSON.
putJson
  :: String
  -- ^ The section name.
  -> [A.Pair]
  -- ^ The results.
  -> A.Value
  -- ^ Action to print the results as YAML.
putJson section = object . pure . (fromString section .=) . object
