{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# HLINT ignore "Use underscore" #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
module Spec.Analysis (
  -- * Testing
  tests,
) where

import Cardano.Api qualified as C
import Cardano.Api.Ledger qualified as Ledger
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.BaseTypes qualified as CI
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import Data.Aeson qualified as A
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy qualified as LBS
import Data.Functor ((<&>))
import Data.Map.Strict qualified as M
import Data.Ratio ((%))
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Language.Marlowe (emptyState)
import Language.Marlowe.Analysis.Safety.Transaction (CurrentState (..), findTransactions, unitAnnotator)
import Language.Marlowe.CLI.Analyze (
  ContractInstance (
    ContractInstance,
    ciContinuations,
    ciContract,
    ciOpenRoleValidator,
    ciPayoutValidator,
    ciRolesCurrency,
    ciSemanticsValidator,
    ciSlotConfig,
    ciState
  ),
  checkExecutionCost,
 )
import Language.Marlowe.CLI.Run (initializeTransactionImpl)
import Language.Marlowe.CLI.Types (
  CliEnv (CliEnv),
  MarloweTransaction (..),
 )
import Language.Marlowe.Client (marloweParams)
import Language.Marlowe.Core.V1.Merkle (MerkleizedContract (..))
import Language.Marlowe.Core.V1.Semantics.Types (
  Action (Deposit, Notify),
  Case (Case),
  Contract (Close, Pay, When),
  Observation (TrueObs),
  Party (Role),
  Payee (Party),
  State (..),
  Token (Token),
  Value (Constant),
 )
import Language.Marlowe.Extended.V1 (adaSymbol, adaToken)
import Plutus.V1.Ledger.SlotConfig (SlotConfig (SlotConfig))
import PlutusLedgerApi.V1.Value (tokenName)
import PlutusLedgerApi.V2 (MajorProtocolVersion (..), POSIXTime (..))
import PlutusTx.AssocMap qualified as AM
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)

-- | Run tests.
tests :: TestTree
tests = do
  let scenarios = scenario1 <> [scenario2]
  testGroup
    "Safety analysis"
    [ testGroup "Transaction cost" $
        scenarios <&> \scenario@Scenario{..} ->
          testCase scLabel (checkTransactionCost scenario)
    ]

checkTransactionCost :: Scenario -> Assertion
checkTransactionCost Scenario{scContract, scState, scMerkleize, scExpected} =
  fmap (either (error . show) id)
    . runExceptT
    $ do
      let era = C.BabbageEraOnwardsBabbage
      MarloweTransaction{..} <-
        flip runReaderT (CliEnv era) $
          initializeTransactionImpl
            @C.PlutusScriptV2
            (marloweParams "8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d")
            (SlotConfig 0 1000)
            (MajorProtocolVersion $ fromEnum $ fst $ C.protocolParamProtocolVersion protocolTestnet)
            ( (\(C.CostModel c) -> fromIntegral <$> c) $
                C.protocolParamCostModels protocolTestnet M.! C.AnyPlutusScriptVersion C.PlutusScriptV2
            )
            (C.Testnet $ C.NetworkMagic 1)
            C.NoStakeAddress
            scContract
            scState
            Nothing
            scMerkleize
            False
      let ciState = mtState
          ciContract = mtContract
          ciContinuations = mtContinuations
          ciRolesCurrency = mtRolesCurrency
          ciSemanticsValidator = mtValidator
          ciOpenRoleValidator = mtOpenRoleValidator
          ciPayoutValidator = mtRoleValidator
          ciSlotConfig = mtSlotConfig
          contract = MerkleizedContract ciContract ciContinuations
      transactions <- findTransactions unitAnnotator True contract (AlreadyInitialized scState)
      actual <- checkExecutionCost era protocolTestnet ContractInstance{..} transactions False
      let lsbToText :: LBS.ByteString -> Text.Text
          lsbToText = Text.decodeUtf8 . LBS.toStrict

          jsonToText :: A.Value -> Text.Text
          jsonToText = lsbToText . encodePretty

          errorMessage =
            Text.unpack . jsonToText $
              A.object
                [ "Expected costs" A..= scExpected
                , "Actual costs" A..= actual
                ]

      liftIO $ assertBool errorMessage $ actual == scExpected

data Scenario = Scenario
  { scState :: State
  , scContract :: Contract
  , scMerkleize :: Bool
  , scExpected :: A.Value
  , scLabel :: String
  }

maxMemory :: Double
maxMemory = 14_000_000

maxSteps :: Double
maxSteps = 10_000_000_000

scenario1 :: [Scenario]
scenario1 =
  [ Scenario
      { scState = state
      , scContract = contract
      , scMerkleize = False
      , scExpected = expectedNonMerkleized
      , scLabel = "Scenario 1 [Normal]"
      }
  , Scenario
      { scState = state
      , scContract = contract
      , scMerkleize = True
      , scExpected = expectedMerkeized
      , scLabel = "Scenario 1 [Merkleized]"
      }
  ]
  where
    state :: State
    state = State (AM.singleton (Role "Alice", Token "" "") 2_000_000) AM.empty AM.empty 1

    contract :: Contract
    contract =
      When
        [ Case
            (Deposit (Role "Bob") (Role "Alice") (Token "" "") (Constant 5_000_000))
            ( When
                [ Case
                    (Deposit (Role "Charlie") (Role "Alice") (Token "" "") (Constant 5_000_000))
                    ( When
                        [ Case
                            (Deposit (Role "Dave") (Role "Alice") (Token "" "") (Constant 5_000_000))
                            ( When
                                [ Case
                                    (Deposit (Role "Eve") (Role "Alice") (Token "" "") (Constant 5_000_000))
                                    ( When
                                        [ Case
                                            (Notify TrueObs)
                                            ( Pay
                                                (Role "Bob")
                                                (Party (Role "Bob"))
                                                (Token "" "")
                                                (Constant 2000000)
                                                ( Pay
                                                    (Role "Charlie")
                                                    (Party (Role "Charlie"))
                                                    (Token "" "")
                                                    (Constant 2000000)
                                                    ( Pay
                                                        (Role "Dave")
                                                        (Party (Role "Dave"))
                                                        (Token "" "")
                                                        (Constant 2000000)
                                                        ( Pay
                                                            (Role "Eve")
                                                            (Party (Role "Eve"))
                                                            (Token "" "")
                                                            (Constant 2000000)
                                                            ( When
                                                                [ Case
                                                                    (Notify TrueObs)
                                                                    Close
                                                                ]
                                                                60_000_000
                                                                Close
                                                            )
                                                        )
                                                    )
                                                )
                                            )
                                        ]
                                        50_000
                                        Close
                                    )
                                ]
                                40_000
                                Close
                            )
                        ]
                        30_000
                        Close
                    )
                ]
                20_000
                Close
            )
        ]
        10_000
        Close

    expectedNonMerkleized :: A.Value
    expectedNonMerkleized =
      A.object
        [ "Execution cost"
            A..= A.object
              [ "Memory"
                  A..= A.object
                    [ "Actual" A..= (11405174 :: Integer)
                    , "Invalid" A..= False
                    , "Maximum" A..= maxMemory
                    , "Percentage" A..= (100 * 11405174 / maxMemory)
                    ]
              , "Steps"
                  A..= A.object
                    [ "Actual" A..= (3032642845 :: Integer)
                    , "Invalid" A..= False
                    , "Maximum" A..= maxSteps
                    , "Percentage" A..= (100 * 3032642845 / maxSteps)
                    ]
              ]
        ]
    expectedMerkeized :: A.Value
    expectedMerkeized =
      A.object
        [ "Execution cost"
            A..= A.object
              [ "Memory"
                  A..= A.object
                    [ "Actual" A..= (12248998 :: Integer)
                    , "Invalid" A..= False
                    , "Maximum" A..= maxMemory
                    , "Percentage" A..= (100 * 12248998 / maxMemory)
                    ]
              , "Steps"
                  A..= A.object
                    [ "Actual" A..= (3311705924 :: Integer)
                    , "Invalid" A..= False
                    , "Maximum" A..= maxSteps
                    , "Percentage" A..= (100 * 3311705924 / maxSteps)
                    ]
              ]
        ]

scenario2 :: Scenario
scenario2 =
  Scenario
    { scState = state
    , scContract = contract
    , scMerkleize = False
    , scExpected = expectedNonMerkleized
    , scLabel = "Scenario 2 [Normal]"
    }
  where
    ada = Token adaSymbol adaToken
    accounts =
      AM.unsafeFromList $
        [ ((accountId, ada), 1)
        | i <- [1 .. 10] :: [Int]
        , let accountId = Role $ tokenName . Text.encodeUtf8 . Text.pack . show $ i
        ]
    state = (emptyState (POSIXTime 0)){accounts = accounts}
    contract =
      When
        [ Case
            (Notify TrueObs)
            Close
        ]
        30_000
        Close

    expectedNonMerkleized :: A.Value
    expectedNonMerkleized =
      A.object
        [ "Execution cost"
            A..= A.object
              [ "Memory"
                  A..= A.object
                    [ "Actual" A..= (15933293 :: Integer)
                    , "Invalid" A..= True
                    , "Maximum" A..= maxMemory
                    , "Percentage" A..= (100 * 15933293 / maxMemory)
                    ]
              , "Steps"
                  A..= A.object
                    [ "Actual" A..= (4602444699 :: Integer)
                    , "Invalid" A..= False
                    , "Maximum" A..= maxSteps
                    , "Percentage" A..= (100 * 4602444699 / maxSteps)
                    ]
              ]
        ]

protocolTestnet :: C.ProtocolParameters
protocolTestnet =
  C.ProtocolParameters
    { protocolParamProtocolVersion = (8, 0)
    , protocolParamDecentralization = Nothing
    , protocolParamExtraPraosEntropy = Nothing
    , protocolParamMaxBlockHeaderSize = 1100
    , protocolParamMaxBlockBodySize = 90112
    , protocolParamMaxTxSize = 16384
    , protocolParamTxFeeFixed = 155381
    , protocolParamTxFeePerByte = 44
    , protocolParamMinUTxOValue = Nothing
    , protocolParamStakeAddressDeposit = Ledger.Coin 2000000
    , protocolParamStakePoolDeposit = Ledger.Coin 500000000
    , protocolParamMinPoolCost = Ledger.Coin 340000000
    , protocolParamPoolRetireMaxEpoch = CI.EpochInterval 18
    , protocolParamStakePoolTargetNum = 500
    , protocolParamPoolPledgeInfluence = 3 % 10
    , protocolParamMonetaryExpansion = 3 % 1000
    , protocolParamTreasuryCut = 1 % 5
    , protocolParamCostModels =
        M.singleton
          (C.AnyPlutusScriptVersion C.PlutusScriptV2)
          . C.CostModel
          . M.elems
          $ M.fromList @String
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
    , protocolParamPrices =
        Just (C.ExecutionUnitPrices{priceExecutionSteps = 721 % 10000000, priceExecutionMemory = 577 % 10000})
    , protocolParamMaxTxExUnits = Just (C.ExecutionUnits{executionSteps = 10000000000, executionMemory = 14000000})
    , protocolParamMaxBlockExUnits = Just (C.ExecutionUnits{executionSteps = 40000000000, executionMemory = 62000000})
    , protocolParamMaxValueSize = Just 5000
    , protocolParamCollateralPercent = Just 150
    , protocolParamMaxCollateralInputs = Just 3
    , protocolParamUTxOCostPerByte = Just (Ledger.Coin 4310)
    }
