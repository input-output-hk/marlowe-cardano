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
import Cardano.Api.Shelley (ProtocolParameters(..))
import Codec.Serialise (serialise)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson (object, (.=))
import Data.Aeson.Types (Pair)
import Data.Functor.Constant (Constant(..))
import Data.Generics.Multiplate
import Data.List
import Data.String (IsString(..))
import Data.Yaml (encode)
import Debug.Trace
import Language.Marlowe.CLI.IO
import Language.Marlowe.CLI.Types
import Language.Marlowe.Core.V1.Semantics (MarloweData(..), MarloweParams(..))
import Language.Marlowe.Core.V1.Semantics.Types (Contract(Close), State(..), Token(..))
import Language.Marlowe.Core.V1.Semantics.Types.Address (deserialiseAddressBech32)
import Language.Marlowe.Scripts (MarloweInput, marloweValidator, marloweValidatorHash)
import Ledger.Typed.Scripts (validatorScript)
import Numeric.Natural
import Plutus.ApiCommon
import Plutus.V1.Ledger.Ada (lovelaceValueOf)
import Plutus.V1.Ledger.Address (scriptHashAddress)
import Plutus.V2.Ledger.Api hiding (evaluateScriptCounting)

import qualified Data.ByteString.Char8 as BS8 (putStr)
import qualified Data.ByteString.Lazy as LBS (toStrict)
import qualified Data.ByteString.Short as SBS (ShortByteString, toShort)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Language.Marlowe.Core.V1.Semantics.Types as C
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
    ProtocolParameters{protocolParamMaxValueSize} <- queryInEra connection QueryProtocolParameters
    let
      checkAll = not $ preconditions || roles || tokens || maximumValue || minimumUtxo || executionCost
      ci = ContractInstance mtRolesCurrency mtState mtContract mtContinuations
    when (preconditions || checkAll)
      $ checkPreconditions ci
    when (roles || checkAll)
      $ checkRoles ci
    when (tokens || checkAll)
      $ checkTokens ci
    when (maximumValue || checkAll)
      $ checkMaximumValue protocolParamMaxValueSize best ci


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


checkMaximumValue :: MonadIO m
                  => Maybe Natural
                  -> Bool
                  -> ContractInstance
                  -> m ()
checkMaximumValue (Just maxValue) False ci =
  let
    size = computeValueSize $ extractFromContract ci
  in
    putYaml "Maximum value"
      [
        "Size" .= size
      , "Maximum" .= maxValue
      , "Units" .= ("byte" :: String)
      , "Invalid" .= (size > fromEnum maxValue)
      ]
checkMaximumValue (Just _) True _ = error "Not implemented."
checkMaximumValue _ _ _ = pure ()


computeValueSize :: S.Set C.Token
                 -> Int
computeValueSize tokens =
  let
    nTokens = S.size tokens
    nPolicies = S.size $ S.map (\(Token c _) -> c) tokens
    nNames = sum . fmap P.lengthOfByteString . S.toList $ S.map (\(Token _ (TokenName n)) -> n) tokens
    padWords x = (x + 7) `div` 8
  in
    trace (show (nTokens, nPolicies, nNames)) $ 8 * (6 + padWords (12 * nTokens + 28 * nPolicies + fromInteger nNames))


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

