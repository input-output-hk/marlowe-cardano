{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Script execution functions for Marlowe testing.
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
module Spec.Marlowe.Plutus.Script (
  -- * Evaluation
  evaluatePayout,
  evaluateSemantics,

  -- * Addresses
  payoutAddress,
  semanticsAddress,

  -- * Hashes
  hashMarloweData,
  hashRole,
  payoutScriptHash,
  semanticsScriptHash,

  -- * Parameters
  costModel,
) where

import Codec.Serialise (serialise)
import Control.Monad.Except (runExcept)
import Data.Bifunctor (Bifunctor (first))
import Data.Maybe (fromJust)
import Data.These (These (..))
import Language.Marlowe.Core.V1.Semantics (MarloweData)
import Language.Marlowe.Scripts (
  marloweValidatorBytes,
  marloweValidatorHash,
  rolePayoutValidatorBytes,
  rolePayoutValidatorHash,
 )
import Paths_marlowe_cardano (getDataDir)
import Plutus.ApiCommon (
  EvaluationContext,
  LedgerPlutusVersion (PlutusV2),
  LogOutput,
  ProtocolVersion (ProtocolVersion),
  VerboseMode (Verbose),
  evaluateScriptCounting,
  mkEvaluationContext,
 )
import Plutus.V1.Ledger.Address (scriptHashAddress)
import Plutus.V2.Ledger.Api (
  Address,
  CostModelParams,
  Data (..),
  DatumHash (..),
  ExBudget (..),
  ExCPU (..),
  ExMemory (..),
  ScriptContext (..),
  TokenName,
  TxInfo (..),
  ValidatorHash,
  fromData,
 )
import System.FilePath ((<.>), (</>))
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.ByteString.Lazy as LBS (writeFile)
import qualified Data.Map.Strict as M (fromList)
import Language.Marlowe.Util (dataHash)

{-# NOINLINE unsafeDumpBenchmark #-}
-- Dump data files for benchmarking Plutus execution cost.
unsafeDumpBenchmark
  :: FilePath
  -- ^ Name of folder the benchmarks.
  -> Data
  -- ^ The datum.
  -> Data
  -- ^ The redeemer.
  -> Data
  -- ^ The script context.
  -> ExBudget
  -- ^ The Plutus execution cost.
  -> a
  -- ^ A value.
  -> a
  -- ^ The same value.
unsafeDumpBenchmark folder datum redeemer context ExBudget{..} x =
  unsafePerformIO $ -- ☹
    do
      let i = txInfoId . scriptContextTxInfo . fromJust $ fromData context
          ExCPU cpu = exBudgetCPU
          ExMemory memory = exBudgetMemory
          result =
            Constr
              0
              [ datum
              , redeemer
              , context
              , I $ toInteger cpu
              , I $ toInteger memory
              ]
          payload = serialise result
      folder' <- (</> folder) <$> getDataDir
      LBS.writeFile
        (folder' </> show i <.> "benchmark")
        payload
      pure x

-- | Dump benchmarking files.
dumpBenchmarks :: Bool
dumpBenchmarks = False

-- | Check the Plutus execution budget.
enforceBudget :: Bool
enforceBudget = False

-- | Run the Plutus evaluator on the Marlowe semantics validator.
evaluateSemantics
  :: Data
  -- ^ The datum.
  -> Data
  -- ^ The redeemer.
  -> Data
  -- ^ The script context.
  -> These String LogOutput
  -- ^ The result.
evaluateSemantics datum redeemer context =
  case evaluationContext of
    Left message -> This message
    Right ec -> case evaluateScriptCounting PlutusV2 (ProtocolVersion 8 0) Verbose ec marloweValidatorBytes [datum, redeemer, context] of
      (logOutput, Right ex@ExBudget{..}) ->
        ( if dumpBenchmarks
            then unsafeDumpBenchmark "semantics" datum redeemer context ex
            else id
        )
          $ if enforceBudget && (exBudgetCPU > 10_000_000_000 || exBudgetMemory > 14_000_000)
            then These ("Exceeded Plutus budget: " <> show ex) logOutput
            else That logOutput
      (logOutput, Left message) -> These (show message) logOutput

-- | Run the Plutus evaluator on the Marlowe payout validator.
evaluatePayout
  :: Data
  -- ^ The datum.
  -> Data
  -- ^ The redeemer.
  -> Data
  -- ^ The script context.
  -> These String LogOutput
  -- ^ The result.
evaluatePayout datum redeemer context =
  case evaluationContext of
    Left message -> This message
    Right ec -> case evaluateScriptCounting PlutusV2 (ProtocolVersion 8 0) Verbose ec rolePayoutValidatorBytes [datum, redeemer, context] of
      (logOutput, Right ex) ->
        ( if dumpBenchmarks
            then unsafeDumpBenchmark "rolepayout" datum redeemer context ex
            else id
        )
          $ That logOutput
      (logOutput, Left message) -> These (show message) logOutput

-- | Compute the address of the Marlowe semantics validator.
semanticsAddress :: Address
semanticsAddress = scriptHashAddress semanticsScriptHash

-- | Compute the hash of the Marlowe semantics validator.
semanticsScriptHash :: ValidatorHash
semanticsScriptHash = marloweValidatorHash

-- | Compute the address of the Marlowe payout validator.
payoutAddress :: Address
payoutAddress = scriptHashAddress payoutScriptHash

-- | Compute the hash of the Marlowe payout validator.
payoutScriptHash :: ValidatorHash
payoutScriptHash = rolePayoutValidatorHash

-- | Compute the hash of Marlowe datum.
hashMarloweData
  :: MarloweData
  -> DatumHash
hashMarloweData = DatumHash . dataHash

-- | Compute the hash of a role token.
hashRole
  :: TokenName
  -> DatumHash
hashRole = DatumHash . dataHash

-- | Build an evaluation context.
evaluationContext :: Either String EvaluationContext
evaluationContext = first show . runExcept $ mkEvaluationContext costModel

-- | A default cost model for Plutus.
costModel :: CostModelParams
costModel =
  M.fromList
    [ ("addInteger-cpu-arguments-intercept", 205665)
    , ("addInteger-cpu-arguments-slope", 812)
    , ("addInteger-memory-arguments-intercept", 1)
    , ("addInteger-memory-arguments-slope", 1)
    , ("appendByteString-cpu-arguments-intercept", 1000)
    , ("appendByteString-cpu-arguments-slope", 571)
    , ("appendByteString-memory-arguments-intercept", 0)
    , ("appendByteString-memory-arguments-slope", 1)
    , ("appendString-cpu-arguments-intercept", 1000)
    , ("appendString-cpu-arguments-slope", 24177)
    , ("appendString-memory-arguments-intercept", 4)
    , ("appendString-memory-arguments-slope", 1)
    , ("bData-cpu-arguments", 1000)
    , ("bData-memory-arguments", 32)
    , ("blake2b_256-cpu-arguments-intercept", 117366)
    , ("blake2b_256-cpu-arguments-slope", 10475)
    , ("blake2b_256-memory-arguments", 4)
    , ("cekApplyCost-exBudgetCPU", 23000)
    , ("cekApplyCost-exBudgetMemory", 100)
    , ("cekBuiltinCost-exBudgetCPU", 23000)
    , ("cekBuiltinCost-exBudgetMemory", 100)
    , ("cekConstCost-exBudgetCPU", 23000)
    , ("cekConstCost-exBudgetMemory", 100)
    , ("cekDelayCost-exBudgetCPU", 23000)
    , ("cekDelayCost-exBudgetMemory", 100)
    , ("cekForceCost-exBudgetCPU", 23000)
    , ("cekForceCost-exBudgetMemory", 100)
    , ("cekLamCost-exBudgetCPU", 23000)
    , ("cekLamCost-exBudgetMemory", 100)
    , ("cekStartupCost-exBudgetCPU", 100)
    , ("cekStartupCost-exBudgetMemory", 100)
    , ("cekVarCost-exBudgetCPU", 23000)
    , ("cekVarCost-exBudgetMemory", 100)
    , ("chooseData-cpu-arguments", 19537)
    , ("chooseData-memory-arguments", 32)
    , ("chooseList-cpu-arguments", 175354)
    , ("chooseList-memory-arguments", 32)
    , ("chooseUnit-cpu-arguments", 46417)
    , ("chooseUnit-memory-arguments", 4)
    , ("consByteString-cpu-arguments-intercept", 221973)
    , ("consByteString-cpu-arguments-slope", 511)
    , ("consByteString-memory-arguments-intercept", 0)
    , ("consByteString-memory-arguments-slope", 1)
    , ("constrData-cpu-arguments", 89141)
    , ("constrData-memory-arguments", 32)
    , ("decodeUtf8-cpu-arguments-intercept", 497525)
    , ("decodeUtf8-cpu-arguments-slope", 14068)
    , ("decodeUtf8-memory-arguments-intercept", 4)
    , ("decodeUtf8-memory-arguments-slope", 2)
    , ("divideInteger-cpu-arguments-constant", 196500)
    , ("divideInteger-cpu-arguments-model-arguments-intercept", 453240)
    , ("divideInteger-cpu-arguments-model-arguments-slope", 220)
    , ("divideInteger-memory-arguments-intercept", 0)
    , ("divideInteger-memory-arguments-minimum", 1)
    , ("divideInteger-memory-arguments-slope", 1)
    , ("encodeUtf8-cpu-arguments-intercept", 1000)
    , ("encodeUtf8-cpu-arguments-slope", 28662)
    , ("encodeUtf8-memory-arguments-intercept", 4)
    , ("encodeUtf8-memory-arguments-slope", 2)
    , ("equalsByteString-cpu-arguments-constant", 245000)
    , ("equalsByteString-cpu-arguments-intercept", 216773)
    , ("equalsByteString-cpu-arguments-slope", 62)
    , ("equalsByteString-memory-arguments", 1)
    , ("equalsData-cpu-arguments-intercept", 1060367)
    , ("equalsData-cpu-arguments-slope", 12586)
    , ("equalsData-memory-arguments", 1)
    , ("equalsInteger-cpu-arguments-intercept", 208512)
    , ("equalsInteger-cpu-arguments-slope", 421)
    , ("equalsInteger-memory-arguments", 1)
    , ("equalsString-cpu-arguments-constant", 187000)
    , ("equalsString-cpu-arguments-intercept", 1000)
    , ("equalsString-cpu-arguments-slope", 52998)
    , ("equalsString-memory-arguments", 1)
    , ("fstPair-cpu-arguments", 80436)
    , ("fstPair-memory-arguments", 32)
    , ("headList-cpu-arguments", 43249)
    , ("headList-memory-arguments", 32)
    , ("iData-cpu-arguments", 1000)
    , ("iData-memory-arguments", 32)
    , ("ifThenElse-cpu-arguments", 80556)
    , ("ifThenElse-memory-arguments", 1)
    , ("indexByteString-cpu-arguments", 57667)
    , ("indexByteString-memory-arguments", 4)
    , ("lengthOfByteString-cpu-arguments", 1000)
    , ("lengthOfByteString-memory-arguments", 10)
    , ("lessThanByteString-cpu-arguments-intercept", 197145)
    , ("lessThanByteString-cpu-arguments-slope", 156)
    , ("lessThanByteString-memory-arguments", 1)
    , ("lessThanEqualsByteString-cpu-arguments-intercept", 197145)
    , ("lessThanEqualsByteString-cpu-arguments-slope", 156)
    , ("lessThanEqualsByteString-memory-arguments", 1)
    , ("lessThanEqualsInteger-cpu-arguments-intercept", 204924)
    , ("lessThanEqualsInteger-cpu-arguments-slope", 473)
    , ("lessThanEqualsInteger-memory-arguments", 1)
    , ("lessThanInteger-cpu-arguments-intercept", 208896)
    , ("lessThanInteger-cpu-arguments-slope", 511)
    , ("lessThanInteger-memory-arguments", 1)
    , ("listData-cpu-arguments", 52467)
    , ("listData-memory-arguments", 32)
    , ("mapData-cpu-arguments", 64832)
    , ("mapData-memory-arguments", 32)
    , ("mkCons-cpu-arguments", 65493)
    , ("mkCons-memory-arguments", 32)
    , ("mkNilData-cpu-arguments", 22558)
    , ("mkNilData-memory-arguments", 32)
    , ("mkNilPairData-cpu-arguments", 16563)
    , ("mkNilPairData-memory-arguments", 32)
    , ("mkPairData-cpu-arguments", 76511)
    , ("mkPairData-memory-arguments", 32)
    , ("modInteger-cpu-arguments-constant", 196500)
    , ("modInteger-cpu-arguments-model-arguments-intercept", 453240)
    , ("modInteger-cpu-arguments-model-arguments-slope", 220)
    , ("modInteger-memory-arguments-intercept", 0)
    , ("modInteger-memory-arguments-minimum", 1)
    , ("modInteger-memory-arguments-slope", 1)
    , ("multiplyInteger-cpu-arguments-intercept", 69522)
    , ("multiplyInteger-cpu-arguments-slope", 11687)
    , ("multiplyInteger-memory-arguments-intercept", 0)
    , ("multiplyInteger-memory-arguments-slope", 1)
    , ("nullList-cpu-arguments", 60091)
    , ("nullList-memory-arguments", 32)
    , ("quotientInteger-cpu-arguments-constant", 196500)
    , ("quotientInteger-cpu-arguments-model-arguments-intercept", 453240)
    , ("quotientInteger-cpu-arguments-model-arguments-slope", 220)
    , ("quotientInteger-memory-arguments-intercept", 0)
    , ("quotientInteger-memory-arguments-minimum", 1)
    , ("quotientInteger-memory-arguments-slope", 1)
    , ("remainderInteger-cpu-arguments-constant", 196500)
    , ("remainderInteger-cpu-arguments-model-arguments-intercept", 453240)
    , ("remainderInteger-cpu-arguments-model-arguments-slope", 220)
    , ("remainderInteger-memory-arguments-intercept", 0)
    , ("remainderInteger-memory-arguments-minimum", 1)
    , ("remainderInteger-memory-arguments-slope", 1)
    , ("serialiseData-cpu-arguments-intercept", 1159724)
    , ("serialiseData-cpu-arguments-slope", 392670)
    , ("serialiseData-memory-arguments-intercept", 0)
    , ("serialiseData-memory-arguments-slope", 2)
    , ("sha2_256-cpu-arguments-intercept", 806990)
    , ("sha2_256-cpu-arguments-slope", 30482)
    , ("sha2_256-memory-arguments", 4)
    , ("sha3_256-cpu-arguments-intercept", 1927926)
    , ("sha3_256-cpu-arguments-slope", 82523)
    , ("sha3_256-memory-arguments", 4)
    , ("sliceByteString-cpu-arguments-intercept", 265318)
    , ("sliceByteString-cpu-arguments-slope", 0)
    , ("sliceByteString-memory-arguments-intercept", 4)
    , ("sliceByteString-memory-arguments-slope", 0)
    , ("sndPair-cpu-arguments", 85931)
    , ("sndPair-memory-arguments", 32)
    , ("subtractInteger-cpu-arguments-intercept", 205665)
    , ("subtractInteger-cpu-arguments-slope", 812)
    , ("subtractInteger-memory-arguments-intercept", 1)
    , ("subtractInteger-memory-arguments-slope", 1)
    , ("tailList-cpu-arguments", 41182)
    , ("tailList-memory-arguments", 32)
    , ("trace-cpu-arguments", 212342)
    , ("trace-memory-arguments", 32)
    , ("unBData-cpu-arguments", 31220)
    , ("unBData-memory-arguments", 32)
    , ("unConstrData-cpu-arguments", 32696)
    , ("unConstrData-memory-arguments", 32)
    , ("unIData-cpu-arguments", 43357)
    , ("unIData-memory-arguments", 32)
    , ("unListData-cpu-arguments", 32247)
    , ("unListData-memory-arguments", 32)
    , ("unMapData-cpu-arguments", 38314)
    , ("unMapData-memory-arguments", 32)
    , ("verifyEcdsaSecp256k1Signature-cpu-arguments", 35892428)
    , ("verifyEcdsaSecp256k1Signature-memory-arguments", 10)
    , ("verifyEd25519Signature-cpu-arguments-intercept", 57996947)
    , ("verifyEd25519Signature-cpu-arguments-slope", 18975)
    , ("verifyEd25519Signature-memory-arguments", 10)
    , ("verifySchnorrSecp256k1Signature-cpu-arguments-intercept", 38887044)
    , ("verifySchnorrSecp256k1Signature-cpu-arguments-slope", 32947)
    , ("verifySchnorrSecp256k1Signature-memory-arguments", 10)
    ]