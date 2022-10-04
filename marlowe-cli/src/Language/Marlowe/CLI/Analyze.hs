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


{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module Language.Marlowe.CLI.Analyze
  ( -- * Types
    ContractInstance(..)
    -- * Analysis
  , analyze
  , measure
  ) where


import Cardano.Api hiding (Address, ScriptHash, TxOut)
import Cardano.Api.Shelley (ProtocolParameters(..), StakeCredential(..))
import Codec.Serialise (serialise)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson (object, (.=))
import Data.Aeson.Types (Pair)
import Data.Bifunctor (second)
import Data.Functor.Constant (Constant(..))
import Data.Generics.Multiplate
import Data.List
import Data.String (IsString(..))
import Data.Yaml (encode)
import Language.Marlowe.CLI.IO
import Language.Marlowe.CLI.Merkle
import Language.Marlowe.CLI.Types hiding (datumHash)
import Language.Marlowe.Core.V1.Semantics (MarloweData(..), MarloweParams(..))
import Language.Marlowe.Core.V1.Semantics.Types (Contract(Close), State(..), Token(..))
import Language.Marlowe.Core.V1.Semantics.Types.Address (deserialiseAddressBech32)
import Language.Marlowe.FindInputs (getAllInputs)
import Language.Marlowe.Scripts (MarloweInput, marloweValidator, marloweValidatorHash)
import Ledger.Tx.CardanoAPI (toCardanoValue)
import Ledger.Typed.Scripts (validatorScript)
import Numeric.Natural
import Plutus.ApiCommon
import Plutus.Script.Utils.Scripts (datumHash)
import Plutus.V1.Ledger.Ada (lovelaceValueOf)
import Plutus.V1.Ledger.Address (scriptHashAddress)
import Plutus.V2.Ledger.Api hiding (evaluateScriptCounting)
import System.IO

import qualified Cardano.Api as Api
import qualified Cardano.Api.Shelley as Api
import qualified Data.ByteString.Char8 as BS8 (putStr)
import qualified Data.ByteString.Lazy as LBS (toStrict)
import qualified Data.ByteString.Short as SBS (ShortByteString, toShort)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Language.Marlowe.Core.V1.Semantics as C
import qualified Language.Marlowe.Core.V1.Semantics.Types as C
import qualified Language.Marlowe.Scripts as C
import qualified PlutusTx.AssocMap as AM
import qualified PlutusTx.Prelude as P


-- | A bundle of information describing a contract instance.
data ContractInstance =
  ContractInstance
  {
    ciRolesCurrency :: CurrencySymbol  -- ^ The roles currency.
  , ciState         :: State           -- ^ The initial state of the contract.
  , ciContract      :: Contract        -- ^ The initial contract.
  , ciContinuations :: Continuations   -- ^ The merkleized continuations for the contract.
  }
    deriving Show


-- | Analyze a Marlowe contract for protocol-limit or other violations.
analyze :: MonadError CliError m
        => MonadIO m
        => MonadReader (CliEnv era) m
        => LocalNodeConnectInfo CardanoMode  -- ^ The connection info for the local node.
        -> FilePath                          -- ^ The JSON file with the Marlowe inputs, final state, and final contract.
        -> Bool                              -- ^ Whether to check preconditions for Marlowe state.
        -> Bool                              -- ^ Whether to check lengths of role names.
        -> Bool                              -- ^ Whether to check lengths of token names.
        -> Bool                              -- ^ Whether to check the `maxValueSize` protocol limit.
        -> Bool                              -- ^ Whether to check the `utxoCostPerWord` protocol limit.
        -> Bool                              -- ^ Whether to check the `maxTxExecutionUnits` protocol limits.
        -> Bool                              -- ^ Whether to compute tight estimates of worst-case bounds.
        -> m ()                              -- ^ Print estimates of worst-case bounds.
analyze connection marloweFile preconditions roles tokens maximumValue minimumUtxo executionCost best =
  do
    SomeMarloweTransaction _language _era MarloweTransaction{..} <- decodeFileStrict marloweFile
    era <- askEra
    protocol@ProtocolParameters{protocolParamMaxValueSize} <- queryInEra connection QueryProtocolParameters
    let
      checkAll = not $ preconditions || roles || tokens || maximumValue || minimumUtxo || executionCost
      ci = ContractInstance mtRolesCurrency mtState mtContract mtContinuations
    paths <-
      if checkAll || executionCost || best && (maximumValue || minimumUtxo)
        then findPaths ci
        else pure mempty
    when (preconditions || checkAll)
      $ checkPreconditions ci
    when (roles || checkAll)
      $ checkRoles ci
    when (tokens || checkAll)
      $ checkTokens ci
    when (maximumValue || checkAll)
      $ checkMaximumValue protocolParamMaxValueSize best ci
    when (minimumUtxo || checkAll)
      $ withShelleyBasedEra era
      $ checkMinimumUtxo era protocol best ci
    when (executionCost || checkAll)
      $ checkExecutionCost protocol ci paths


-- | Report invalid properties in the Marlowe state:
--
--   * Invalid roles currency.
--   * Invalid currency symbol or token name in an account.
--   * Account balances that are not positive.
--   * Duplicate accounts, choices, or bound values.
checkPreconditions :: MonadIO m
                   => ContractInstance
                   -> m ()
checkPreconditions ContractInstance{ciRolesCurrency, ciState=State{..}} =
  let
    nonPositiveBalances = filter ((<= 0) . snd) . AM.toList
    duplicates (AM.keys -> x) = x \\ nub x
  in
    putYaml "Preconditions"
      [
        "Invalid roles currency"        .= invalidCurrency ciRolesCurrency
      , "Invalid account tokens"        .= filter invalidToken (snd <$> AM.keys accounts)
      , "Non-positive account balances" .= nonPositiveBalances accounts
      , "Duplicate accounts"            .= duplicates accounts
      , "Duplicate choices"             .= duplicates choices
      , "Duplicate bound values"        .= duplicates boundValues
      ]


invalidCurrency :: CurrencySymbol -> Bool
invalidCurrency currency@CurrencySymbol{..} = currency /= adaSymbol && P.lengthOfByteString unCurrencySymbol /= 28


invalidToken :: C.Token -> Bool
invalidToken (Token currency@CurrencySymbol{..} token@TokenName{..}) =
  not $  currency == adaSymbol
         && token == adaToken
      || P.lengthOfByteString unCurrencySymbol == 28
         && P.lengthOfByteString unTokenName <= 32


invalidRole :: TokenName -> Bool
invalidRole TokenName{..} = P.lengthOfByteString unTokenName > 32


checkTokens :: MonadIO m
           => ContractInstance
           -> m ()
checkTokens ci =
  putYaml "Tokens"
    [
      "Invalid tokens" .= invalidToken `S.filter` extractFromContract ci
    ]


checkRoles :: MonadIO m
           => ContractInstance
           -> m ()
checkRoles ci =
  let
    roles = extractFromContract ci
  in
    putYaml "Role names"
      [
        "Invalid role names" .= invalidRole `S.filter` roles
      , "Blank role names" .= adaToken `S.member` roles
      ]


checkMaximumValue :: MonadError CliError m
                  => MonadIO m
                  => Maybe Natural
                  -> Bool
                  -> ContractInstance
                  -> m ()
checkMaximumValue (Just maxValue) _ {- TODO: Use execution paths in case where `best == True`. -} ci =
  let
    size = computeValueSize $ extractFromContract ci
  in
    putYaml "Maximum value"
      [
        "Actual" .= size
      , "Maximum" .= maxValue
      , "Unit" .= ("byte" :: String)
      , "Invalid" .= (size > fromEnum maxValue)
      , "Percentage" .= (100 * fromIntegral size / fromIntegral maxValue :: Double)
      ]
checkMaximumValue _ _ _ = throwError $ CliError "Missing `maxValue` protocol parameter."


checkMinimumUtxo :: forall era m
                 .  IsShelleyBasedEra era
                 => MonadError CliError m
                 => MonadIO m
                 => ScriptDataSupportedInEra era
                 -> ProtocolParameters
                 -> Bool
                 -> ContractInstance
                 -> m ()
checkMinimumUtxo era protocol _ {- TODO: Use execution paths in case where `best == True`. -} ci =
  do
    tokens <-
      liftCli
        . toCardanoValue
        $ mconcat
        [
          singleton cs tn 1
        |
          C.Token cs tn <- S.toList $ extractFromContract ci
        ]
    let
      address =
        makeShelleyAddressInEra
            Mainnet
            (PaymentCredentialByScript "11111111111111111111111111111111111111111111111111111111")
            (StakeAddressByValue $ StakeCredentialByKey "22222222222222222222222222222222222222222222222222222222")
      out =
        Api.TxOut
          address
          (Api.TxOutValue (toMultiAssetSupportedInEra era) tokens)
          (TxOutDatumHash era "3333333333333333333333333333333333333333333333333333333333333333")
          Api.ReferenceScriptNone
    value <- liftCli $ calculateMinimumUTxO shelleyBasedEra out protocol
    putYaml "Minimum UTxO"
      [
        "Requirement" .= value
      ]


checkExecutionCost :: MonadError CliError m
                   => MonadIO m
                   => ProtocolParameters
                   -> ContractInstance
                   -> [(POSIXTime, [C.TransactionInput])]
                   -> m ()
checkExecutionCost protocol ContractInstance{..} paths =
  do
    (_, referenceAddress) <- liftCliMaybe "Failed to parse reference script address." $ deserialiseAddressBech32 "addr1qypn5mwucvjm0pkfwemjus8n7glep8927sj76dv9a3xnq4j7e4q926apsvcd6kn33cpx95k8jsmrj7v0k62rczvz8urqfk0yl3"
    (_, creatorAddress) <- liftCliMaybe "Failed to parse reference script address." $ deserialiseAddressBech32 "addr1v90p3rn8zx3funlx4yqjx772dc7n9gwgahqm5f8myl69mugru44dx"
    CostModel costModel <- liftCliMaybe "Plutus cost model not found." $ AnyPlutusScriptVersion PlutusScriptV2 `M.lookup` protocolParamCostModels protocol
    evaluationContext <- liftCli $ mkEvaluationContext costModel
    let
      referenceInput =
        TxInInfo
          (TxOutRef "9dc19f70488336dbf3d68388fcfb2e3f7a3d7ed6358422f37b10aa6625246c71" 1)
          (TxOut referenceAddress (lovelaceValueOf 55_000_000) NoOutputDatum (Just semanticsScriptHash))
      executor = executeTransaction evaluationContext referenceInput creatorAddress $ MarloweParams ciRolesCurrency
    (steps, memory) <-
      unzip
        . fmap (\ExBudget{..} -> (exBudgetCPU, exBudgetMemory))
        . concat
        <$> sequence
        [
          executePath executor state ciContract ciContinuations inputs
        |
          (minTime, inputs) <- paths
        , let state = C.State (AM.singleton (C.Address True creatorAddress, Token adaSymbol adaToken) 30_000_000) AM.empty AM.empty minTime
        ]
    let
      ExCPU actualSteps = maximum steps
      maximumSteps = maybe 0 fromEnum $ executionSteps <$> protocolParamMaxTxExUnits protocol
      ExMemory actualMemory = maximum memory
      maximumMemory = maybe 0 fromEnum $ executionMemory <$> protocolParamMaxTxExUnits protocol
    putYaml "Execution Cost"
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


executePath :: MonadError CliError m
            => (C.State -> C.Contract -> C.TransactionInput -> m ((C.State, C.Contract), ExBudget))
            -> C.State
            -> C.Contract
            -> Continuations
            -> [C.TransactionInput]
            -> m [ExBudget]
executePath executor state contract _continuations inputs =
  snd
    <$> foldM
        (\((state', contract'), budgets) -> fmap (second (: budgets)) . executor state' contract')
        ((state, contract), [])
        inputs


executeTransaction :: MonadError CliError m
                   => EvaluationContext
                   -> TxInInfo
                   -> Address
                   -> MarloweParams
                   -> C.State
                   -> C.Contract
                   -> C.TransactionInput
                   -> m ((C.State, C.Contract), ExBudget)
executeTransaction evaluationContext referenceInput creatorAddress marloweParams@MarloweParams{..} marloweState marloweContract input@C.TransactionInput{..} =
  do
    (txOutState, txOutContract, txOutPayments) <-
      case C.computeTransaction input marloweState marloweContract of
        C.TransactionOutput{..} -> pure (txOutState, txOutContract, txOutPayments)
        C.Error e               -> throwError . CliError $ show e
    let
      inDatum = Datum $ toBuiltinData MarloweData{..}
      inDatumHash = datumHash inDatum
      inValue = C.totalBalance $ accounts marloweState
      inScriptTxRef = TxOutRef "1111111111111111111111111111111111111111111111111111111111111111" 0
      inScriptTx = TxInInfo inScriptTxRef $ TxOut semanticsAddress inValue (OutputDatumHash inDatumHash) Nothing
      outDatum =
        if txOutContract == C.Close
          then mempty
          else pure . Datum . toBuiltinData $ MarloweData marloweParams txOutState txOutContract
      outDatumHash = datumHash <$> outDatum
      outValue = C.totalBalance $ accounts txOutState
      outScriptTx = flip (TxOut semanticsAddress outValue) Nothing . OutputDatumHash <$> outDatumHash
      redeemer = Redeemer . toBuiltinData $ C.marloweTxInputsFromInputs txInputs
      makePayment (C.Payment _ (C.Party (C.Address _ address)) (C.Token currency name) amount) =
        pure $ TxOut address (singleton currency name amount) NoOutputDatum Nothing
      makePayment (C.Payment _ (C.Party (C.Role role)) (C.Token currency name) amount) =
        pure
          $ TxOut
            payoutAddress
            (singleton currency name amount)
            (OutputDatumHash . datumHash . Datum $ toBuiltinData (rolesCurrency, role))
            Nothing
      makePayment _ = mempty
      makePaymentDatum (C.Payment _ (C.Party (C.Role role)) _ _) =
        pure . Datum $ toBuiltinData (rolesCurrency, role)
      makePaymentDatum _ = mempty
      outPayments = concatMap makePayment txOutPayments
      outPaymentDatums = concatMap makePaymentDatum txOutPayments
      findRole :: C.InputContent -> [TokenName]
      findRole (C.IDeposit _ (C.Role role) _ _) = pure role
      findRole (C.IChoice (C.ChoiceId _ (C.Role role)) _) = pure role
      findRole _ = mempty
      roles = fmap (flip (singleton rolesCurrency) 1) . foldMap findRole $ C.getInputContent <$> txInputs
      inRoles =
        [
          TxInInfo (TxOutRef "1111111111111111111111111111111111111111111111111111111111111111" i) outRole
        |
          (i, outRole) <- zip [0..] outRoles
        ]
      outRoles = [TxOut creatorAddress role NoOutputDatum Nothing | role <- roles]
-- FIXME: add change, merkleization, and signatures; lengthen script addresses.
      txInfoInputs = inScriptTx : inRoles
      txInfoReferenceInputs = [referenceInput]
      txInfoOutputs =
        TxOut creatorAddress txInfoFee NoOutputDatum Nothing
          : outScriptTx
          <> outPayments
          <> outRoles
      txInfoFee = singleton adaSymbol adaToken 1
      txInfoMint = mempty
      txInfoDCert = mempty
      txInfoWdrl = AM.empty
      txInfoValidRange = Interval (LowerBound (Finite $ fst txInterval) True) (UpperBound (Finite $ snd txInterval) True)
      txInfoSignatories = []
      txInfoRedeemers = AM.singleton scriptContextPurpose redeemer
      txInfoData = AM.fromList $ ((,) =<< datumHash) <$> inDatum : outDatum <> outPaymentDatums
      txInfoId = "2222222222222222222222222222222222222222222222222222222222222222"
      scriptContextTxInfo = TxInfo{..}
      scriptContextPurpose = Spending inScriptTxRef
      scriptContext = ScriptContext{..}
    case evaluateSemantics evaluationContext (toData inDatum) (toData redeemer) (toData scriptContext) of
      (_, Right budget) -> pure ((txOutState, txOutContract), budget)
      (msg, Left err)   -> throwError . CliError $ "Plutus execution failed: " <> show err <> " with log " <> show msg <> "."


computeValueSize :: S.Set C.Token
                 -> Int
computeValueSize tokens =
  let
    nTokens = S.size tokens
    nPolicies = S.size $ S.map (\(Token c _) -> c) tokens
    nNames = sum . fmap P.lengthOfByteString . S.toList $ S.map (\(Token _ (TokenName n)) -> n) tokens
    padWords x = (x + 7) `div` 8
  in
    8 * (6 + padWords (12 * nTokens + 28 * nPolicies + fromInteger nNames))


data MarlowePlate f =
  MarlowePlate
  {
    contractPlate :: C.Contract -> f C.Contract
  , casePlate :: C.Case C.Contract -> f (C.Case C.Contract)
  , actionPlate :: C.Action -> f C.Action
  , valuePlate :: C.Value C.Observation -> f (C.Value C.Observation)
  , observationPlate :: C.Observation -> f C.Observation
  }

instance Multiplate MarlowePlate where
  multiplate child =
    let
      buildContract C.Close = pure C.Close
      buildContract (C.Pay a p t v c) = C.Pay a p t <$> valuePlate child v <*> contractPlate child c
      buildContract (C.If o c c') = C.If <$> observationPlate child o <*> contractPlate child c <*> contractPlate child c'
      buildContract (C.When cs t c) = C.When <$> sequenceA (casePlate child <$> cs) <*> pure t <*> contractPlate child c
      buildContract (C.Let i v c) = C.Let i <$> valuePlate child v <*> contractPlate child c
      buildContract (C.Assert o c) = C.Assert <$> observationPlate child o <*> contractPlate child c
      buildCase (C.Case a c) = C.Case <$> actionPlate child a <*> contractPlate child c
      buildCase (C.MerkleizedCase a h) = C.MerkleizedCase <$> actionPlate child a <*> pure h
      buildAction (C.Deposit a p t v) = C.Deposit a p t <$> valuePlate child v
      buildAction (C.Notify o) = C.Notify <$> observationPlate child o
      buildAction x = pure x
      buildValue (C.NegValue x) = C.NegValue <$> valuePlate child x
      buildValue (C.AddValue x y) = C.AddValue <$> valuePlate child x <*> valuePlate child y
      buildValue (C.SubValue x y) = C.SubValue <$> valuePlate child x <*> valuePlate child y
      buildValue (C.MulValue x y) = C.MulValue <$> valuePlate child x <*> valuePlate child y
      buildValue (C.DivValue x y) = C.DivValue <$> valuePlate child x <*> valuePlate child y
      buildValue (C.Cond o x y) = C.Cond <$> observationPlate child o <*> valuePlate child x <*> valuePlate child y
      buildValue x = pure x
      buildObservation (C.AndObs x y) = C.AndObs <$> observationPlate child x <*> observationPlate child y
      buildObservation (C.OrObs x y) = C.OrObs <$> observationPlate child x <*> observationPlate child y
      buildObservation (C.NotObs x) = C.NotObs <$> observationPlate child x
      buildObservation (C.ValueGE x y) = C.ValueGE <$> valuePlate child x <*> valuePlate child y
      buildObservation (C.ValueGT x y) = C.ValueGT <$> valuePlate child x <*> valuePlate child y
      buildObservation (C.ValueLT x y) = C.ValueLT <$> valuePlate child x <*> valuePlate child y
      buildObservation (C.ValueLE x y) = C.ValueLE <$> valuePlate child x <*> valuePlate child y
      buildObservation (C.ValueEQ x y) = C.ValueEQ <$> valuePlate child x <*> valuePlate child y
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


class Extract a where
  extractor :: MarlowePlate (Constant (S.Set a))
  extractAll :: Ord a => C.Contract -> S.Set a
  extractAll = foldFor contractPlate $ preorderFold extractor

instance Extract C.Token where
  extractor =
    let
      single = Constant . S.singleton
      contractPlate' (C.Pay _ _ t _ _) = single t
      contractPlate' x = pure x
      actionPlate' (C.Deposit _ _ t _) = single t
      actionPlate' x = pure x
      valuePlate' (C.AvailableMoney _ t) = single t
      valuePlate' x = pure x
    in
      purePlate
      {
        contractPlate = contractPlate'
      , actionPlate = actionPlate'
      , valuePlate = valuePlate'
      }

instance Extract TokenName where
  extractor =
    let
      role (C.Role r) = S.singleton r
      role _ = S.empty
      contractPlate' (C.Pay a (C.Account p) _ _ _) = Constant $ role a <> role p
      contractPlate' (C.Pay a (C.Party p) _ _ _) = Constant $ role a <> role p
      contractPlate' x = pure x
      actionPlate' (C.Deposit a p _ _) = Constant $ role a <> role p
      actionPlate' x = pure x
      valuePlate' (C.AvailableMoney a _) = Constant $ role a
      valuePlate' (C.ChoiceValue (C.ChoiceId _ p)) = Constant $ role p
      valuePlate' x = pure x
      observationPlate' (C.ChoseSomething (C.ChoiceId _ p)) = Constant $ role p
      observationPlate' x = pure x
    in
      purePlate
      {
        contractPlate = contractPlate'
      , actionPlate = actionPlate'
      , valuePlate = valuePlate'
      , observationPlate = observationPlate'
      }


extractFromContract :: Extract a
                    => Ord a
                    => ContractInstance
                    -> S.Set a
extractFromContract ContractInstance{..} =
  M.foldr (S.union . extractAll) (extractAll ciContract) ciContinuations


-- | Format results of checking as YAML.
putYaml :: MonadIO m
        => String     -- ^ The section name.
        -> [Pair]     -- ^ The results.
        -> m ()       -- ^ Action to print the results as YAML.
putYaml section = liftIO . BS8.putStr . encode . object . (: []) . (fromString section .=) . object


findPaths :: MonadError CliError m
          => MonadIO m
          => ContractInstance
          -> m [(POSIXTime, [C.TransactionInput])]
findPaths ContractInstance{..} =
  do
    unless (M.null ciContinuations)
      . liftIO $ hPutStrLn stderr "Demerkleizing contract . . ."
    contract <- runReaderT (deepDemerkleize ciContract) ciContinuations
    liftIO $ hPutStrLn stderr "Starting search for execution paths . . ."
    paths <- liftCliIO $ getAllInputs contract
    liftIO $ hPutStrLn stderr $ " . . . found " <> (show $ length paths) <> " execution paths."
    pure paths


-- | Utility for exploring cost modeling of Marlowe contracts.
measure :: forall era m
         . MonadError CliError m
        => MonadIO m
        => MonadReader (CliEnv era) m
        => LocalNodeConnectInfo CardanoMode  -- ^ The connection info for the local node.
        -> m ()                              -- ^ Explore cost of execution.
measure connection =
  do
    (_, referenceAddress) <- liftCliMaybe "Failed to parse reference script address." $ deserialiseAddressBech32 "addr1qypn5mwucvjm0pkfwemjus8n7glep8927sj76dv9a3xnq4j7e4q926apsvcd6kn33cpx95k8jsmrj7v0k62rczvz8urqfk0yl3"
    (_, creatorAddress) <- liftCliMaybe "Failed to parse reference script address." $ deserialiseAddressBech32 "addr1v90p3rn8zx3funlx4yqjx772dc7n9gwgahqm5f8myl69mugru44dx"
    let
      rolesCurrency = ""
      marloweParams = MarloweParams{..}
      marloweInLovelace = 1_000_000
      marloweInValue = lovelaceValueOf marloweInLovelace
      accounts =
        AM.fromList
          [
            ((C.Address True creatorAddress, Token adaSymbol adaToken), marloweInLovelace)
          ]
      choices = AM.empty
      boundValues =
        AM.fromList
          [
            (C.ValueId "a", 0)
          , (C.ValueId "b", 0)
          , (C.ValueId "c", 0)
          , (C.ValueId "d", 0)
          , (C.ValueId "e", 0)
          , (C.ValueId "f", 0)
          , (C.ValueId "g", 0)
          , (C.ValueId "h", 0)
          , (C.ValueId "i", 0)
          , (C.ValueId "j", 0)
          ]
      minTime = POSIXTime 0
      marloweState = State{..}
      marloweContract = Close
      datum = Datum $ toBuiltinData MarloweData{..}
      redeemer = Redeemer $ toBuiltinData ([] :: MarloweInput)
      marloweTxInRef = TxOutRef "7cf52c6dcf4e244ff017579ca2ddc0271cdf4ffb276f7dea218772887c817258" 0
      marloweTxIn = TxInInfo marloweTxInRef $ TxOut semanticsAddress marloweInValue (OutputDatum datum) Nothing
      txInfoInputs =
        [
          marloweTxIn
        ]
      txInfoReferenceInputs =
        [
          TxInInfo
            (TxOutRef "9dc19f70488336dbf3d68388fcfb2e3f7a3d7ed6358422f37b10aa6625246c71" 1)
            (TxOut referenceAddress (lovelaceValueOf 55_000_000) NoOutputDatum (Just semanticsScriptHash))
        ]
      txInfoOutputs =
        [
          TxOut creatorAddress marloweInValue NoOutputDatum Nothing
        ]
      txInfoFee = mempty
      txInfoMint = mempty
      txInfoDCert = mempty
      txInfoWdrl = AM.empty
      txInfoValidRange = Interval (LowerBound (Finite 0) True) (UpperBound (Finite 1) False)
      txInfoSignatories = []
      txInfoRedeemers = AM.empty
      txInfoData = AM.empty
      txInfoId = "52591b598b1e07551def0aecbfcb92db0c413c2d67ee337667ee30e65a3c651b"
      scriptContextTxInfo = TxInfo{..}
      scriptContextPurpose = Spending marloweTxInRef
      scriptContext = ScriptContext{..}
    ProtocolParameters{protocolParamCostModels} <- queryInEra connection QueryProtocolParameters
    CostModel costModel <- liftCliMaybe "Plutus cost model not found." $ AnyPlutusScriptVersion PlutusScriptV2 `M.lookup` protocolParamCostModels
    evaluationContext <- liftCli $ mkEvaluationContext costModel
    liftIO . print $ evaluateSemantics evaluationContext (toData datum) (toData redeemer) (toData scriptContext)


-- | Run the Plutus evaluator on the Marlowe semantics validator.
evaluateSemantics :: EvaluationContext                             -- ^ The evaluation context.
                  -> Data                                          -- ^ The datum.
                  -> Data                                          -- ^ The redeemer.
                  -> Data                                          -- ^ The script context.
                  -> (LogOutput, Either EvaluationError ExBudget)  -- ^ The result.
evaluateSemantics evaluationContext datum redeemer context =
  evaluateScriptCounting
    PlutusV2 (ProtocolVersion 7 0)
    Verbose evaluationContext
    serialiseSemanticsValidator [datum, redeemer, context]


-- | Serialize the Marlowe semantics validator.
serialiseSemanticsValidator :: SBS.ShortByteString
serialiseSemanticsValidator =
    SBS.toShort
  . LBS.toStrict
  . serialise
  . getValidator
  . validatorScript
  $ marloweValidator


-- | Compute the address of the Marlowe semantics validator.
semanticsAddress :: Address
semanticsAddress = scriptHashAddress semanticsValidatorHash


-- | Compute the hash of the Marlowe semantics validator.
semanticsScriptHash :: ScriptHash
semanticsScriptHash =
  let
    ValidatorHash bytes = marloweValidatorHash
  in
    ScriptHash bytes


-- | Compute the hash of the Marlowe semantics validator.
semanticsValidatorHash :: ValidatorHash
semanticsValidatorHash = marloweValidatorHash


-- | Compute the address of the Marlowe payout validator.
payoutAddress :: Address
payoutAddress = scriptHashAddress C.rolePayoutValidatorHash
