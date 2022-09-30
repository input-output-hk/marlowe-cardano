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
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}


module Language.Marlowe.CLI.Analyze
  ( -- * Analysis
    analyze
  , measure
  ) where


import Cardano.Api hiding (Address, ScriptHash, TxOut)
import Cardano.Api.Shelley (ProtocolParameters(..))
import Codec.Serialise (serialise)
import Control.Monad.Except
import Control.Monad.Reader
import Language.Marlowe.CLI.IO
import Language.Marlowe.CLI.Types
import Language.Marlowe.Core.V1.Semantics (MarloweData(..), MarloweParams(..))
import Language.Marlowe.Core.V1.Semantics.Types (Contract(Close), State(..), Token(..))
import Language.Marlowe.Core.V1.Semantics.Types.Address (deserialiseAddressBech32)
import Language.Marlowe.Scripts (MarloweInput, marloweValidator, marloweValidatorHash)
import Ledger.Typed.Scripts (validatorScript)
import Plutus.ApiCommon
import Plutus.V1.Ledger.Ada (lovelaceValueOf)
import Plutus.V1.Ledger.Address (scriptHashAddress)
import Plutus.V2.Ledger.Api hiding (evaluateScriptCounting)

import qualified Data.ByteString.Lazy as LBS (toStrict)
import qualified Data.ByteString.Short as SBS (ShortByteString, toShort)
import qualified Data.Map.Strict as M
import qualified Language.Marlowe.Core.V1.Semantics.Types as S
import qualified PlutusTx.AssocMap as AM


-- | Analyze a Marlowe contract for protocol-limit or other violations.
analyze :: MonadError CliError m
        => MonadIO m
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
analyze _connection marloweFile _preconditions _roles _tokens _maximumValue _minimumUtxo _executionCost _best =
  do
    SomeMarloweTransaction _language _era MarloweTransaction{} <- decodeFileStrict marloweFile
    pure ()


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
            ((S.Address True creatorAddress, Token adaSymbol adaToken), marloweInLovelace)
          ]
      choices = AM.empty
      boundValues =
        AM.fromList
          [
            (S.ValueId "a", 0)
          , (S.ValueId "b", 0)
          , (S.ValueId "c", 0)
          , (S.ValueId "d", 0)
          , (S.ValueId "e", 0)
          , (S.ValueId "f", 0)
          , (S.ValueId "g", 0)
          , (S.ValueId "h", 0)
          , (S.ValueId "i", 0)
          , (S.ValueId "j", 0)
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

