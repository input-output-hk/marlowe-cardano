-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Analyze Marlowe contracts.
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


module Language.Marlowe.CLI.Analyze
  ( -- * Types
    ContractInstance(..)
    -- * Analysis
  , analyze
  ) where


import Control.Monad (guard, (<=<))
import Control.Monad.Except (MonadError(throwError), MonadIO(..), foldM, unless)
import Control.Monad.Reader (ReaderT(runReaderT))
import Data.Aeson (object, (.=))
import Data.Bifunctor (bimap)
import Data.Foldable (toList)
import Data.Generics.Multiplate (Multiplate(..), foldFor, preorderFold, purePlate)
import Data.List (nub, nubBy, (\\))
import Data.Maybe (catMaybes)
import Data.String (IsString(..))
import Language.Marlowe.CLI.Cardano.Api.PlutusScript (IsPlutusScriptLanguage(..), toScriptLanguageInEra)
import Language.Marlowe.CLI.IO (decodeFileStrict, liftCli, liftCliIO, liftCliMaybe)
import Language.Marlowe.CLI.Merkle (deepDemerkleize, merkleizeInput)
import Language.Marlowe.CLI.Run (marloweAddressFromCardanoAddress)
import Language.Marlowe.CLI.Types
  ( CliError(CliError)
  , Continuations
  , MarloweTransaction(MarloweTransaction, mtContinuations, mtContract, mtInputs, mtPayments, mtRange, mtRoleValidator, mtRolesCurrency, mtSlotConfig, mtState, mtValidator)
  , SomeMarloweTransaction(SomeMarloweTransaction)
  , ValidatorInfo(..)
  , toCollateralSupportedInEra
  , toExtraKeyWitnessesSupportedInEra
  , toMultiAssetSupportedInEra
  , toTxFeesExplicitInEra
  , toValidityLowerBoundSupportedInEra
  , toValidityUpperBoundSupportedInEra
  , validatorInfoScriptOrReference
  , withShelleyBasedEra
  )
import Language.Marlowe.Core.V1.Semantics
  ( MarloweData(MarloweData, marloweContract, marloweParams, marloweState)
  , MarloweParams(..)
  , Payment(Payment)
  , TransactionInput(..)
  , TransactionOutput(Error, TransactionOutput, txOutContract, txOutPayments, txOutState, txOutWarnings)
  , computeTransaction
  , totalBalance
  )
import Language.Marlowe.Core.V1.Semantics.Types
  ( Action(..)
  , Case(..)
  , ChoiceId(ChoiceId)
  , Contract(..)
  , Input(MerkleizedInput)
  , InputContent(IChoice, IDeposit)
  , Observation(AndObs, ChoseSomething, NotObs, OrObs, ValueEQ, ValueGE, ValueGT, ValueLE, ValueLT)
  , Party(Address, Role)
  , Payee(Account, Party)
  , State(..)
  , Token(..)
  , Value(AddValue, AvailableMoney, ChoiceValue, Cond, DivValue, MulValue, NegValue, SubValue)
  , getInputContent
  )
import Language.Marlowe.Core.V1.Semantics.Types.Address (mainnet)
import Language.Marlowe.FindInputs (getAllInputs)
import Language.Marlowe.Scripts (marloweTxInputsFromInputs)
import Numeric.Natural (Natural)
import System.IO (hPutStrLn, stderr)

import qualified Cardano.Api as Api
import qualified Cardano.Api.Shelley as Api
import qualified Cardano.Ledger.Credential as Shelley
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A (Pair)
import qualified Data.ByteString as BS (length)
import qualified Data.ByteString.Char8 as BS8 (putStr)
import qualified Data.ByteString.Short as SBS (ShortByteString)
import qualified Data.Functor.Constant as F (Constant(..))
import qualified Data.Map.Strict as M (foldr, lookup, null)
import qualified Data.Set as S (Set, empty, filter, map, member, singleton, size, union)
import qualified Data.Yaml as Y (encode)
import qualified Ledger.Tx.CardanoAPI as P (toCardanoAddressInEra, toCardanoValue)
import qualified Plutus.ApiCommon as P (LedgerPlutusVersion(PlutusV2), evaluateScriptCounting)
import qualified Plutus.Script.Utils.Scripts as P (datumHash)
import qualified Plutus.V1.Ledger.Ada as P (lovelaceValueOf)
import qualified Plutus.V1.Ledger.SlotConfig as P (SlotConfig, posixTimeToEnclosingSlot)
import qualified Plutus.V2.Ledger.Api as P hiding (evaluateScriptCounting)
import qualified PlutusTx.AssocMap as AM
import qualified PlutusTx.Prelude as P


-- | Analyze a Marlowe contract for protocol-limit or other violations.
analyze :: forall m
        .  MonadError CliError m
        => MonadIO m
        => Api.LocalNodeConnectInfo Api.CardanoMode  -- ^ The connection info for the local node.
        -> FilePath                                  -- ^ The JSON file with the Marlowe inputs, final state, and final contract.
        -> Bool                                      -- ^ Whether to check preconditions for Marlowe state.
        -> Bool                                      -- ^ Whether to check lengths of role names.
        -> Bool                                      -- ^ Whether to check lengths of token names.
        -> Bool                                      -- ^ Whether to check the `maxValueSize` protocol limit.
        -> Bool                                      -- ^ Whether to check the `utxoCostPerWord` protocol limit.
        -> Bool                                      -- ^ Whether to check the `maxTxExecutionUnits` protocol limits.
        -> Bool                                      -- ^ Whether to check the `maxTxSize` protocol limits.
        -> Bool                                      -- ^ Whether to compute tight estimates of worst-case bounds.
        -> Bool                                      -- ^ Whether to include worst-case example in output.
        -> m ()                                      -- ^ Print estimates of worst-case bounds.
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
          result <- analyzeImpl era protocol marlowe preconditions roles tokens maximumValue minimumUtxo executionCost transactionSize best verbose
          liftIO . BS8.putStr $ Y.encode result
      _ -> throwError . CliError $ "Analysis not supported for " <> show era <> " era."


-- | A bundle of information describing a contract instance.
data ContractInstance lang era =
  ContractInstance
  {
    ciRolesCurrency      :: P.CurrencySymbol        -- ^ The roles currency.
  , ciState              :: State                   -- ^ The initial state of the contract.
  , ciContract           :: Contract                -- ^ The initial contract.
  , ciContinuations      :: Continuations           -- ^ The merkleized continuations for the contract.
  , ciSemanticsValidator :: ValidatorInfo lang era  -- ^ The semantics validator.
  , ciPayoutValidator    :: ValidatorInfo lang era  -- ^ The payout validator.
  , ciSlotConfig         :: P.SlotConfig            -- ^ The slot configuration.
  }
    deriving Show


-- | Analyze a Marlowe contract for protocol-limit or other violations.
analyzeImpl :: forall m lang era
            .  Api.IsShelleyBasedEra era
            => IsPlutusScriptLanguage lang
            => MonadError CliError m
            => MonadIO m
            => Api.ScriptDataSupportedInEra era          -- ^ The era.
            -> Api.ProtocolParameters                    -- ^ The connection info for the local node.
            -> MarloweTransaction lang era               -- ^ The the Marlowe inputs, final state, and final contract.
            -> Bool                                      -- ^ Whether to check preconditions for Marlowe state.
            -> Bool                                      -- ^ Whether to check lengths of role names.
            -> Bool                                      -- ^ Whether to check lengths of token names.
            -> Bool                                      -- ^ Whether to check the `maxValueSize` protocol limit.
            -> Bool                                      -- ^ Whether to check the `utxoCostPerWord` protocol limit.
            -> Bool                                      -- ^ Whether to check the `maxTxExecutionUnits` protocol limits.
            -> Bool                                      -- ^ Whether to check the `maxTxSize` protocol limits.
            -> Bool                                      -- ^ Whether to compute tight estimates of worst-case bounds.
            -> Bool                                      -- ^ Whether to include worst-case example in output.
            -> m A.Value                                 -- ^ Action for finding estimates of worst-case bounds.
analyzeImpl era protocol@Api.ProtocolParameters{protocolParamMaxValueSize} MarloweTransaction{..} preconditions roles tokens maximumValue minimumUtxo executionCost transactionSize best _verbose =
  do
    let
      checkAll = not $ preconditions || roles || tokens || maximumValue || minimumUtxo || executionCost
      ci = ContractInstance mtRolesCurrency mtState mtContract mtContinuations mtValidator mtRoleValidator mtSlotConfig
    transactions <-
      if checkAll || executionCost || transactionSize || best && (maximumValue || minimumUtxo)
        then findTransactions ci
        else pure mempty
    let
      maybeTransactions = guard best >> pure transactions
      guardValue condition x =
        if condition
          then Just <$> x
          else pure Nothing
    A.toJSON
      . catMaybes
      <$> sequence
     [
       guardValue (preconditions || checkAll)
         . pure
         $ checkPreconditions ci
     , guardValue (roles || checkAll)
         . pure
         $ checkRoles ci
     , guardValue (tokens || checkAll)
         . pure
         $ checkTokens ci
     , guardValue (maximumValue || checkAll)
         $ checkMaximumValue protocolParamMaxValueSize maybeTransactions ci
     , guardValue (minimumUtxo || checkAll)
         $ withShelleyBasedEra era
         $ checkMinimumUtxo era protocol maybeTransactions ci
     , guardValue (executionCost || checkAll)
         $ checkExecutionCost protocol ci transactions
     , guardValue (transactionSize || checkAll)
         $ checkTransactionSizes era protocol ci transactions
     ]


-- | Report invalid properties in the Marlowe state:
--
--   * Invalid roles currency.
--   * Invalid currency symbol or token name in an account.
--   * Account balances that are not positive.
--   * Duplicate accounts, choices, or bound values.
checkPreconditions :: ContractInstance lang era  -- ^ The bundle of contract information.
                   -> A.Value                    -- ^ A report on preconditions.
checkPreconditions ContractInstance{ciRolesCurrency, ciState=State{..}} =
  let
    nonPositiveBalances = filter ((<= 0) . snd) . AM.toList
    duplicates (AM.keys -> x) = x \\ nub x
  in
    putJson "Preconditions"
      [
        "Invalid roles currency"        .= invalidCurrency ciRolesCurrency
      , "Invalid account tokens"        .= filter invalidToken (snd <$> AM.keys accounts)
      , "Non-positive account balances" .= nonPositiveBalances accounts
      , "Duplicate accounts"            .= duplicates accounts
      , "Duplicate choices"             .= duplicates choices
      , "Duplicate bound values"        .= duplicates boundValues
      ]


-- | Detect an invalid currency symbol.
invalidCurrency :: P.CurrencySymbol -> Bool
invalidCurrency currency@P.CurrencySymbol{..} = currency /= P.adaSymbol && P.lengthOfByteString unCurrencySymbol /= 28


-- | Detect an invalid token.
invalidToken :: Token -> Bool
invalidToken (Token currency@P.CurrencySymbol{..} token@P.TokenName{..}) =
  not $  currency == P.adaSymbol
         && token == P.adaToken
      || P.lengthOfByteString unCurrencySymbol == 28
         && P.lengthOfByteString unTokenName <= 32


-- | Detect an invalid role name.
invalidRole :: P.TokenName -> Bool
invalidRole P.TokenName{..} = P.lengthOfByteString unTokenName > 32


-- | Check that all tokens are valid.
checkTokens :: ContractInstance lang era  -- ^ The bundle of contract information.
            -> A.Value                    -- ^ A report on token validity.
checkTokens ci =
  putJson "Tokens"
    [
      "Invalid tokens" .= invalidToken `S.filter` extractFromContract ci
    ]


-- | Check that all roles are valid.
checkRoles :: ContractInstance lang era  -- ^ The bundle of contract information.
           -> A.Value                    -- ^ Action to print a report on role-name validity.
checkRoles ci =
  let
    roles = extractFromContract ci
  in
    putJson "Role names"
      [
        "Invalid role names" .= invalidRole `S.filter` roles
      , "Blank role names"   .= P.adaToken `S.member` roles
      ]


-- | Check that the protocol limit on maximum value in a UTxO is not violated.
checkMaximumValue :: MonadError CliError m
                  => Maybe Natural              -- ^ The `maxValue` protocol parameter.
                  -> Maybe [Transaction]        -- ^ The transactions to traverse.
                  -> ContractInstance lang era  -- ^ The bundle of contract information.
                  -> m A.Value                  -- ^ Action to print a report on `maxValue` validity.
checkMaximumValue (Just maxValue) maybeTransactions ci =
  let
    size =
      case maybeTransactions of
        Just transactions -> let
                               measure (_, contract, _, _) = [computeValueSize $ extractAll contract]
                             in
                               maximum $ foldMap measure transactions
        Nothing           -> computeValueSize $ extractFromContract ci
  in
    pure
      $ putJson "Maximum value"
      [
        "Actual"     .= size
      , "Maximum"    .= maxValue
      , "Unit"       .= ("byte" :: String)
      , "Invalid"    .= (size > fromEnum maxValue)
      , "Percentage" .= (100 * fromIntegral size / fromIntegral maxValue :: Double)
      ]
checkMaximumValue _ _ _ = throwError $ CliError "Missing `maxValue` protocol parameter."


-- | Compute the size of a multi-asset value.
computeValueSize :: S.Set Token  -- ^ The tokens present.
                 -> Int          -- ^ The number of bytes on the ledger.
computeValueSize tokens =
  let
    nTokens = S.size tokens
    nPolicies = S.size $ S.map (\(Token c _) -> c) tokens
    nNames = sum . fmap P.lengthOfByteString . toList $ S.map (\(Token _ (P.TokenName n)) -> n) tokens
    padWords x = (x + 7) `div` 8
  in
    8 * (6 + padWords (12 * nTokens + 28 * nPolicies + fromInteger nNames))


-- | Check that the protocol limit on minimum UTxO is not violated.
checkMinimumUtxo :: forall lang era m
                 .  Api.IsShelleyBasedEra era
                 => MonadError CliError m
                 => Api.ScriptDataSupportedInEra era  -- ^ The era.
                 -> Api.ProtocolParameters            -- ^ The protocol parameters.
                 -> Maybe [Transaction]               -- ^ The transactions to traverse.
                 -> ContractInstance lang era         -- ^ The bundle of contract information.
                 -> m A.Value                         -- ^ Action to print a report on the `minimumUTxO` validity.
checkMinimumUtxo era protocol maybeTransactions ci =
  do
    let
      compute tokens =
        do
          value <- liftCli (P.toCardanoValue $ mconcat [P.singleton cs tn 1 | Token cs tn <- toList tokens])
          let
            address =
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
          liftCli $ Api.calculateMinimumUTxO Api.shelleyBasedEra out protocol
    value <-
      case maybeTransactions of
        Just transactions -> let
                               measure (_, contract, _, _) = (: []) <$> compute (extractAll contract)
                             in
                               Api.lovelaceToValue . maximum . fmap Api.selectLovelace <$> foldTransactionsM measure transactions
        Nothing           -> compute $ extractFromContract ci
    pure
      $ putJson "Minimum UTxO"
      [
        "Requirement" .= value
      ]


-- | Check that transactions satisfy the execution-cost protocol limits.
checkExecutionCost :: MonadError CliError m
                   => Api.ProtocolParameters     -- ^ The protocol parameters.
                   -> ContractInstance lang era  -- ^ The bundle of contract information.
                   -> [Transaction]              -- ^ The transaction-paths through the contract.
                   -> m A.Value                  -- ^ Action to print a report on validity of transaction execution costs.
checkExecutionCost protocol ContractInstance{..} transactions =
  do
    Api.CostModel costModel <-
      liftCliMaybe "Plutus cost model not found."
        $ Api.AnyPlutusScriptVersion Api.PlutusScriptV2 `M.lookup` Api.protocolParamCostModels protocol
    evaluationContext <- liftCli $ P.mkEvaluationContext costModel
    let
      creatorAddress =
        P.Address
          (P.PubKeyCredential "88888888888888888888888888888888888888888888888888888888")
          (Just . P.StakingHash $ P.PubKeyCredential "99999999999999999999999999999999999999999999999999999999")
      referenceAddress =
        P.Address
          (P.PubKeyCredential "77777777777777777777777777777777777777777777777777777777")
          (Just . P.StakingHash $ P.PubKeyCredential "99999999999999999999999999999999999999999999999999999999")
      semanticsHash = P.ScriptHash . P.toBuiltin $ Api.serialiseToRawBytes $ viHash ciSemanticsValidator
      referenceInput =
        case viTxIn ciSemanticsValidator of
          Nothing -> mempty
          Just _ -> pure
                      $ P.TxInInfo
                        (P.TxOutRef "6666666666666666666666666666666666666666666666666666666666666666" 1)
                        (P.TxOut referenceAddress (P.lovelaceValueOf 1) P.NoOutputDatum (Just semanticsHash))
    semanticsAddress <- fmap snd . liftCli $ marloweAddressFromCardanoAddress $ viAddress ciSemanticsValidator
    payoutAddress <- fmap snd . liftCli $ marloweAddressFromCardanoAddress $ viAddress ciPayoutValidator
    let
      executor =
        executeTransaction
          evaluationContext
          (viBytes ciSemanticsValidator)
          semanticsAddress
          payoutAddress
          referenceInput
          creatorAddress
          (MarloweParams ciRolesCurrency)
    (steps, memory) <-
      unzip
        . fmap (\P.ExBudget{..} -> (exBudgetCPU, exBudgetMemory))
        <$> mapM executor transactions
    let
      P.ExCPU actualSteps = maximum steps
      maximumSteps = maybe 0 fromEnum $ Api.executionSteps <$> Api.protocolParamMaxTxExUnits protocol
      P.ExMemory actualMemory = maximum memory
      maximumMemory = maybe 0 fromEnum $ Api.executionMemory <$> Api.protocolParamMaxTxExUnits protocol
    pure
      $ putJson "Execution cost"
      [
        "Steps" .= object
                   [
                     "Actual" .= actualSteps
                   , "Maximum" .= maximumSteps
                   , "Percentage" .= (100 * fromIntegral actualSteps / fromIntegral maximumSteps :: Double)
                   , "Invalid" .= (fromEnum actualSteps > maximumSteps)
                   ]
      , "Memory" .= object
                    [
                      "Actual" .= actualMemory
                    , "Maximum" .= maximumMemory
                    , "Percentage" .= (100 * fromIntegral actualMemory / fromIntegral maximumMemory :: Double)
                    , "Invalid" .= (fromEnum actualMemory > maximumMemory)
                    ]
      ]


-- | Execute a Marlowe transaction.
executeTransaction :: MonadError CliError m
                   => P.EvaluationContext  -- ^ The Plutus evaluation context.
                   -> SBS.ShortByteString  -- ^ The validator.
                   -> P.Address            -- ^ The semantics validator address.
                   -> P.Address            -- ^ The payout validator address.
                   -> [P.TxInInfo]         -- ^ The reference-script input, if any.
                   -> P.Address            -- ^ The public-key address executing the transaction.
                   -> MarloweParams        -- ^ The parameters for the Marlowe contract instance.
                   -> Transaction          -- ^ The transaction to be executed.
                   -> m P.ExBudget         -- ^ Action to execute the transaction and to return the new state and contract along with the execution cost.
executeTransaction evaluationContext semanticsValidator semanticsAddress payoutAddress referenceInputs creatorAddress marloweParams@MarloweParams{..} (marloweState, marloweContract, TransactionInput{..}, output) =
  do
    -- We only attend to details that make affect the *execution cost* of the script context,
    -- so the corresponding transaction does not actually need to be valid.
    (txOutState, txOutContract, txOutPayments) <-
      case output of
        TransactionOutput{..} -> pure (txOutState, txOutContract, txOutPayments)
        Error e               -> throwError . CliError $ show e
    let
      oneLovelace = P.lovelaceValueOf 1
      inDatum = P.Datum $ P.toBuiltinData MarloweData{..}
      inDatumHash = P.datumHash inDatum
      inValue = totalBalance $ accounts marloweState
      inScriptTxRef = P.TxOutRef "0000000000000000000000000000000000000000000000000000000000000000" 0
      inScriptTx = P.TxInInfo inScriptTxRef $ P.TxOut semanticsAddress inValue (P.OutputDatumHash inDatumHash) Nothing
      outDatum =
        if txOutContract == Close
          then mempty
          else pure . P.Datum . P.toBuiltinData $ MarloweData marloweParams txOutState txOutContract
      outDatumHash = P.datumHash <$> outDatum
      outValue = totalBalance $ accounts txOutState
      outScriptTx = flip (P.TxOut semanticsAddress outValue) Nothing . P.OutputDatumHash <$> outDatumHash
      redeemer = P.Redeemer . P.toBuiltinData $ marloweTxInputsFromInputs txInputs
      makePayment (Payment _ (Party (Address _ address)) (Token currency name) amount) =
        pure $ P.TxOut address (P.singleton currency name amount) P.NoOutputDatum Nothing
      makePayment (Payment _ (Party (Role role)) (Token currency name) amount) =
        pure
          $ P.TxOut
            payoutAddress
            (P.singleton currency name amount)
            (P.OutputDatumHash . P.datumHash . P.Datum $ P.toBuiltinData (rolesCurrency, role))
            Nothing
      makePayment _ = mempty
      makePaymentDatum (Payment _ (Party (Role role)) _ _) =
        pure . P.Datum $ P.toBuiltinData (rolesCurrency, role)
      makePaymentDatum _ = mempty
      outPayments = concatMap makePayment txOutPayments
      outPaymentDatums = concatMap makePaymentDatum txOutPayments
      findRole :: InputContent -> [P.TokenName]
      findRole (IDeposit _ (Role role) _ _) = pure role
      findRole (IChoice (ChoiceId _ (Role role)) _) = pure role
      findRole _ = mempty
      roles = fmap (flip (P.singleton rolesCurrency) 1) . foldMap findRole $ getInputContent <$> txInputs
      inRoles =
        uncurry (P.TxInInfo . P.TxOutRef "1111111111111111111111111111111111111111111111111111111111111111")
          <$> zip [1..] outRoles
      outRoles = [P.TxOut creatorAddress role P.NoOutputDatum Nothing | role <- roles]
      merkleDatums =
        concat
          [
            case input of
              MerkleizedInput _ _ contract -> pure . P.Datum $ P.toBuiltinData contract
              _                              -> mempty
          |
            input <- txInputs
          ]
      outMerkle =
        [
          P.TxOut creatorAddress oneLovelace (P.OutputDatumHash $ P.datumHash datum) Nothing
        |
          datum <- merkleDatums
        ]
      inFunds =
        P.TxInInfo
          (P.TxOutRef "1111111111111111111111111111111111111111111111111111111111111111" 0)
          (P.TxOut creatorAddress oneLovelace P.NoOutputDatum Nothing)
      outChange = P.TxOut creatorAddress oneLovelace P.NoOutputDatum Nothing
      txInfoInputs =
          inFunds
        : inScriptTx
        : inRoles
      txInfoReferenceInputs = referenceInputs
      txInfoOutputs =
           outChange
        :  outScriptTx
        <> outPayments
        <> outRoles
        <> outMerkle
      txInfoFee = oneLovelace
      txInfoMint = mempty
      txInfoDCert = mempty
      txInfoWdrl = AM.empty
      txInfoValidRange = P.Interval (P.LowerBound (P.Finite $ fst txInterval) True) (P.UpperBound (P.Finite $ snd txInterval) True)
      txInfoSignatories =
        case creatorAddress of
          P.Address (P.PubKeyCredential pkh) _ -> pure pkh
          _                                    -> mempty
      txInfoRedeemers = AM.singleton scriptContextPurpose redeemer
      txInfoData = AM.fromList $ ((,) =<< P.datumHash) <$> inDatum : outDatum <> outPaymentDatums <> merkleDatums
      txInfoId = "2222222222222222222222222222222222222222222222222222222222222222"
      scriptContextTxInfo = P.TxInfo{..}
      scriptContextPurpose = P.Spending inScriptTxRef
      scriptContext = P.ScriptContext{..}
    case evaluateSemantics evaluationContext semanticsValidator (P.toData inDatum) (P.toData redeemer) (P.toData scriptContext) of
      (_, Right budget) -> pure budget
      (msg, Left err)   -> throwError . CliError $ "Plutus execution failed: " <> show err <> " with log " <> show msg <> "."


-- | Check that transactions satisfy the transaction-size protocol limit.
checkTransactionSizes :: forall lang era m
                      .  Api.IsShelleyBasedEra era
                      => IsPlutusScriptLanguage lang
                      => MonadError CliError m
                      => MonadIO m
                      => Api.ScriptDataSupportedInEra era  -- ^ The era.
                      -> Api.ProtocolParameters            -- ^ The protocol parameters.
                      -> ContractInstance lang era         -- ^ The bundle of contract information.
                      -> [Transaction]                     -- ^ The transaction-paths through the contract.
                      -> m A.Value                         -- ^ Action to print a report on validity of transaction size.
checkTransactionSizes era protocol ci transactions =
  do
    sizes <- mapM (checkTransactionSize era protocol ci) transactions
    let
      actual = maximum sizes
      limit = Api.protocolParamMaxTxSize protocol
    pure
      $ putJson "Transaction size"
      [
          "Actual"     .= actual
        , "Maximum"    .= limit
        , "Percentage" .= (100 * fromIntegral actual / fromIntegral limit :: Double)
        , "Invalid"    .= (actual > fromEnum limit)
      ]


-- | Check that transactions satisfy the transaction-size protocol limit.
checkTransactionSize :: forall lang era m
                     .  Api.IsShelleyBasedEra era
                     => IsPlutusScriptLanguage lang
                     => MonadError CliError m
                     => MonadIO m
                     => Api.ScriptDataSupportedInEra era  -- ^ The era.
                     -> Api.ProtocolParameters            -- ^ The protocol parameters.
                     -> ContractInstance lang era         -- ^ The bundle of contract information.
                     -> Transaction                       -- ^ The transaction-paths through the contract.
                     -> m Int                             -- ^ Action to measure the transaction size.
checkTransactionSize era protocol ContractInstance{..} (marloweState, marloweContract, TransactionInput{..}, output) =
  do
    -- We only attend to details that make affect the *size* of the transaction, so
    -- the transaction itself does not actually need to be valid.
    scriptInEra <- liftCliMaybe "Script language not supported in era" $ toScriptLanguageInEra era
    (txOutState, txOutContract, txOutPayments) <-
      case output of
        TransactionOutput{..} -> pure (txOutState, txOutContract, txOutPayments)
        Error e               -> throwError . CliError $ show e
    let
      oneLovelace = Api.lovelaceToValue 1
      creatorAddress =
        Api.makeShelleyAddressInEra
            Api.Mainnet
            (Api.PaymentCredentialByScript "88888888888888888888888888888888888888888888888888888888")
            (Api.StakeAddressByValue $ Api.StakeCredentialByKey "99999999999999999999999999999999999999999999999999999999")
      marloweParams = MarloweParams ciRolesCurrency
      inDatum = MarloweData{..}
      redeemer = marloweTxInputsFromInputs txInputs
      inScript =
        (
          Api.TxIn "0000000000000000000000000000000000000000000000000000000000000000" $ Api.TxIx 0
        , Api.BuildTxWith
            . Api.ScriptWitness Api.ScriptWitnessForSpending
            $ Api.PlutusScriptWitness
              scriptInEra
              (plutusScriptVersion @lang)
              (validatorInfoScriptOrReference ciSemanticsValidator)
              (Api.ScriptDatumForTxIn . Api.fromPlutusData $ P.toData inDatum)
              (Api.fromPlutusData $ P.toData redeemer)
              (Api.ExecutionUnits 0 0)
        )
    outValue <- liftCli . P.toCardanoValue $ totalBalance $ accounts txOutState
    let
      outScript =
        if txOutContract == Close
          then mempty
          else let
                 outDatum = MarloweData marloweParams txOutState txOutContract
               in
                 pure
                   $ Api.TxOut
                     (viAddress ciSemanticsValidator)
                     (Api.TxOutValue (toMultiAssetSupportedInEra era) outValue)
                     (Api.TxOutDatumInTx era . Api.fromPlutusData $ P.toData outDatum)
                     Api.ReferenceScriptNone
      makePayment (Payment _ (Party (Address network address)) (Token currency name) amount) =
        do
          value <- liftCli . P.toCardanoValue $ P.singleton currency name amount
          address' <-
            liftCli
              $ P.toCardanoAddressInEra
                (if network == mainnet then Api.Mainnet else Api.Testnet $ Api.NetworkMagic 2)
                address
          address'' <-
            case address' of
              Api.AddressInEra (Api.ShelleyAddressInEra _) address'' -> pure address''
              _                                                      -> throwError $ CliError "Byron addresses are not supported."
          pure
            [
              Api.TxOut
                (Api.AddressInEra (Api.ShelleyAddressInEra Api.shelleyBasedEra) address'')
                (Api.TxOutValue (toMultiAssetSupportedInEra era) value)
                Api.TxOutDatumNone
                Api.ReferenceScriptNone
            ]
      makePayment (Payment _ (Party (Role role)) (Token currency name) amount) =
        do
          value <- liftCli $ P.toCardanoValue $ P.singleton currency name amount
          pure
            [
              Api.TxOut
                (viAddress ciPayoutValidator)
                (Api.TxOutValue (toMultiAssetSupportedInEra era) value)
                (Api.TxOutDatumInTx era . Api.fromPlutusData $ P.toData (ciRolesCurrency, role))
                Api.ReferenceScriptNone
            ]
      makePayment _ = pure []
    outPayments <- concat <$> mapM makePayment txOutPayments
    let
      findRole :: InputContent -> [P.TokenName]
      findRole (IDeposit _ (Role role) _ _) = pure role
      findRole (IChoice (ChoiceId _ (Role role)) _) = pure role
      findRole _ = mempty
    roles <-
      liftCli
        . mapM P.toCardanoValue
        $ fmap (flip (P.singleton ciRolesCurrency) 1)
        . foldMap findRole
        $ getInputContent
        <$> txInputs
    let
      inRoles =
          (, Api.BuildTxWith $ Api.KeyWitness Api.KeyWitnessForSpending)
            . Api.TxIn "1111111111111111111111111111111111111111111111111111111111111111" . Api.TxIx
            <$> [1..(toEnum $ length roles)]
      outRoles =
        [
          Api.TxOut
            creatorAddress
            (Api.TxOutValue (toMultiAssetSupportedInEra era) role)
            Api.TxOutDatumNone
            Api.ReferenceScriptNone
        |
          role <- roles
        ]
      outMerkle =
        concat
          [
            case input of
              MerkleizedInput _ _ contract ->
                pure
                  $ Api.TxOut
                    creatorAddress
                    (Api.TxOutValue (toMultiAssetSupportedInEra era) oneLovelace)
                    (Api.TxOutDatumInTx era . Api.fromPlutusData $ P.toData contract)
                    Api.ReferenceScriptNone
              _ -> mempty
          |
              input <- txInputs
          ]
      inFunds =
        (
          Api.TxIn "1111111111111111111111111111111111111111111111111111111111111111" $ Api.TxIx 0
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
      txTotalCollateral  = Api.TxTotalCollateralNone
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
          [
            case address of
              Api.AddressInEra _ (Api.ShelleyAddress _ (Shelley.KeyHashObj pkh) _) -> pure $ Api.PaymentKeyHash pkh
              _                                                                    -> mempty
          |
            Api.TxOut address _ _ _ <- outPayments
          ]
      txProtocolParams = Api.BuildTxWith $ Just protocol
      txWithdrawals = Api.TxWithdrawalsNone
      txCertificates = Api.TxCertificatesNone
      txUpdateProposal = Api.TxUpdateProposalNone
      txMintValue = Api.TxMintNone
      txScriptValidity = Api.TxScriptValidityNone
    body <- liftCli $ Api.makeTransactionBody Api.TxBodyContent{..}
    keys <-
      liftIO
        $ sequence
        [
          Api.WitnessPaymentKey <$> Api.generateSigningKey Api.AsPaymentKey
        |
          let wits = case txExtraKeyWits of
                      Api.TxExtraKeyWitnesses _ wits' -> wits'
                      _                               -> mempty
        , _ <- wits
        ]
    let
      tx = Api.signShelleyTransaction body keys
    pure . BS.length $ Api.serialiseToCBOR tx


-- | A mutltiplate for a Marlowe contract.
data MarlowePlate f =
  MarlowePlate
  {
    contractPlate :: Contract -> f Contract
  , casePlate :: Case Contract -> f (Case Contract)
  , actionPlate :: Action -> f Action
  , valuePlate :: Value Observation -> f (Value Observation)
  , observationPlate :: Observation -> f Observation
  }

instance Multiplate MarlowePlate where
  multiplate child =
    let
      buildContract Close = pure Close
      buildContract (Pay a p t v c) = Pay a p t <$> valuePlate child v <*> contractPlate child c
      buildContract (If o c c') = If <$> observationPlate child o <*> contractPlate child c <*> contractPlate child c'
      buildContract (When cs t c) = When <$> sequenceA (casePlate child <$> cs) <*> pure t <*> contractPlate child c
      buildContract (Let i v c) = Let i <$> valuePlate child v <*> contractPlate child c
      buildContract (Assert o c) = Assert <$> observationPlate child o <*> contractPlate child c
      buildCase (Case a c) = Case <$> actionPlate child a <*> contractPlate child c
      buildCase (MerkleizedCase a h) = MerkleizedCase <$> actionPlate child a <*> pure h
      buildAction (Deposit a p t v) = Deposit a p t <$> valuePlate child v
      buildAction (Notify o) = Notify <$> observationPlate child o
      buildAction x = pure x
      buildValue (NegValue x) = NegValue <$> valuePlate child x
      buildValue (AddValue x y) = AddValue <$> valuePlate child x <*> valuePlate child y
      buildValue (SubValue x y) = SubValue <$> valuePlate child x <*> valuePlate child y
      buildValue (MulValue x y) = MulValue <$> valuePlate child x <*> valuePlate child y
      buildValue (DivValue x y) = DivValue <$> valuePlate child x <*> valuePlate child y
      buildValue (Cond o x y) = Cond <$> observationPlate child o <*> valuePlate child x <*> valuePlate child y
      buildValue x = pure x
      buildObservation (AndObs x y) = AndObs <$> observationPlate child x <*> observationPlate child y
      buildObservation (OrObs x y) = OrObs <$> observationPlate child x <*> observationPlate child y
      buildObservation (NotObs x) = NotObs <$> observationPlate child x
      buildObservation (ValueGE x y) = ValueGE <$> valuePlate child x <*> valuePlate child y
      buildObservation (ValueGT x y) = ValueGT <$> valuePlate child x <*> valuePlate child y
      buildObservation (ValueLT x y) = ValueLT <$> valuePlate child x <*> valuePlate child y
      buildObservation (ValueLE x y) = ValueLE <$> valuePlate child x <*> valuePlate child y
      buildObservation (ValueEQ x y) = ValueEQ <$> valuePlate child x <*> valuePlate child y
      buildObservation x = pure x
    in
      MarlowePlate
        buildContract
        buildCase
        buildAction
        buildValue
        buildObservation
  mkPlate build =
    MarlowePlate
      (build contractPlate)
      (build casePlate)
      (build actionPlate)
      (build valuePlate)
      (build observationPlate)


-- | Extract something using the Marlowe multiplate.
class Extract a where
  -- | Shallow extraction.
  extractor :: MarlowePlate (F.Constant (S.Set a))
  -- | Deep extraction.
  extractAll :: Ord a => Contract -> S.Set a
  extractAll = foldFor contractPlate $ preorderFold extractor

instance Extract Token where
  extractor =
    let
      single = F.Constant . S.singleton
      contractPlate' (Pay _ _ t _ _) = single t
      contractPlate' x = pure x
      actionPlate' (Deposit _ _ t _) = single t
      actionPlate' x = pure x
      valuePlate' (AvailableMoney _ t) = single t
      valuePlate' x = pure x
    in
      purePlate
      {
        contractPlate = contractPlate'
      , actionPlate = actionPlate'
      , valuePlate = valuePlate'
      }

instance Extract P.TokenName where
  extractor =
    let
      role (Role r) = S.singleton r
      role _ = S.empty
      contractPlate' (Pay a (Account p) _ _ _) = F.Constant $ role a <> role p
      contractPlate' (Pay a (Party p) _ _ _) = F.Constant $ role a <> role p
      contractPlate' x = pure x
      actionPlate' (Deposit a p _ _) = F.Constant $ role a <> role p
      actionPlate' (Choice (ChoiceId _ p) _) = F.Constant $ role p
      actionPlate' x = pure x
      valuePlate' (AvailableMoney a _) = F.Constant $ role a
      valuePlate' (ChoiceValue (ChoiceId _ p)) = F.Constant $ role p
      valuePlate' x = pure x
      observationPlate' (ChoseSomething (ChoiceId _ p)) = F.Constant $ role p
      observationPlate' x = pure x
    in
      purePlate
      {
        contractPlate = contractPlate'
      , actionPlate = actionPlate'
      , valuePlate = valuePlate'
      , observationPlate = observationPlate'
      }



-- | Extract something from a Marlowe contract.
extractFromContract :: Extract a
                    => Ord a
                    => ContractInstance lang era  -- ^ The bundle of contract information.
                    -> S.Set a                    -- ^ The extract.
extractFromContract ContractInstance{..} =
  M.foldr (S.union . extractAll) (extractAll ciContract) ciContinuations


-- | Format results of checking as JSON.
putJson :: String    -- ^ The section name.
        -> [A.Pair]  -- ^ The results.
        -> A.Value   -- ^ Action to print the results as YAML.
putJson section = object . (: []) . (fromString section .=) . object


-- | Complete information about a Marlowe semantics transaction.
type Transaction = (State, Contract, TransactionInput, TransactionOutput)


-- | Visit transactions along all execution paths of a a contract.
foldTransactionsM :: Monad m
                  => Monoid a
                  => (Transaction -> m a)  -- ^ Function to collect results.
                  -> [Transaction]         -- ^ The transactions.
                  -> m a                   -- ^ The collected results.
foldTransactionsM f =
  (foldM $ (. f) . fmap . (<>)) mempty


-- | Find transactions along all execution paths of a contract.
findTransactions :: MonadError CliError m
                 => MonadIO m
                 => ContractInstance lang era  -- ^ The bundle of contract information.
                 -> m [Transaction]            -- ^ Action for computing the initial state, initial contract, perhaps-merkleized input, and the output for the transactions.
findTransactions ci@ContractInstance{..} =
  do
    let
      creatorAddress =
        P.Address
          (P.PubKeyCredential "88888888888888888888888888888888888888888888888888888888")
          (Just . P.StakingHash $ P.PubKeyCredential "99999999999999999999999999999999999999999999999999999999")
      prune (s, c, i, _) (s', c', i', _) = s == s' && c == c' && i == i'
    paths <- findPaths ci
    nubBy prune
      . concat
      <$> sequence
      [
         findTransactionPath ciContinuations state ciContract inputs
      |
        (minTime, inputs) <- paths
      , let state = State (AM.singleton (Address True creatorAddress, Token P.adaSymbol P.adaToken) 1) AM.empty AM.empty minTime
      ]


-- | Find the transactions corresponding to a path of demerkleized inputs.
findTransactionPath :: MonadError CliError m
                    => Continuations                                               -- ^ The continuations of the contact.
                    -> State                                                       -- ^ The initial contract.
                    -> Contract                                                    -- ^ The initial state.
                    -> [TransactionInput]                                          -- ^ The path of demerkleized inputs.
                    -> m [(State, Contract, TransactionInput, TransactionOutput)]  -- ^ Action for computing the perhaps-merkleized input and the output for the transaction.
findTransactionPath continuations state contract =
  let
    go ((state', contract'), previous) input =
      do
        (input', output) <- findTransaction continuations state' contract' input
        case output of
          TransactionOutput{..} -> pure ((txOutState, txOutContract), (state', contract', input', output) : previous)
          Error e               -> throwError . CliError $ show e
  in
    fmap snd . foldM go ((state, contract), [])


-- | Find the transaction corresponding to demerkleized input.
findTransaction :: MonadError CliError m
                => Continuations                            -- ^ The continuations of the contact.
                -> State                                    -- ^ The initial contract.
                -> Contract                                 -- ^ The initial state.
                -> TransactionInput                         -- ^ The demerkleized input to the contract.
                -> m (TransactionInput, TransactionOutput)  -- ^ Action for computing the perhaps-merkleized input and the output for the transaction.
findTransaction continuations state contract TransactionInput{..} =
  do
    input <- TransactionInput txInterval . snd <$> foldM (merkleizeInput txInterval continuations) ((state, contract), []) txInputs
    pure (input, computeTransaction input state contract)


-- | Find all of the paths through a Marlowe contract.
findPaths :: MonadError CliError m
          => MonadIO m
          => ContractInstance lang era              -- ^ The bundle of contract information.
          -> m [(P.POSIXTime, [TransactionInput])]  -- ^ The paths throught the Marlowe contract.
findPaths ContractInstance{..} =
  do
    unless (M.null ciContinuations)
      . liftIO $ hPutStrLn stderr "Demerkleizing contract . . ."
    contract <- runReaderT (deepDemerkleize ciContract) ciContinuations
    liftIO $ hPutStrLn stderr "Starting search for execution paths . . ."
    paths <- liftCliIO $ getAllInputs contract
    liftIO $ hPutStrLn stderr $ " . . . found " <> (show $ length paths) <> " execution paths."
    pure paths


-- | Run the Plutus evaluator on the Marlowe semantics validator.
evaluateSemantics :: P.EvaluationContext                                 -- ^ The evaluation context.
                  -> SBS.ShortByteString                                 -- ^ The validator script.
                  -> P.Data                                              -- ^ The datum.
                  -> P.Data                                              -- ^ The redeemer.
                  -> P.Data                                              -- ^ The script context.
                  -> (P.LogOutput, Either P.EvaluationError P.ExBudget)  -- ^ The result.
evaluateSemantics evaluationContext validator datum redeemer context =
  P.evaluateScriptCounting
    P.PlutusV2 (P.ProtocolVersion 7 0)
    P.Verbose evaluationContext
    validator [datum, redeemer, context]
