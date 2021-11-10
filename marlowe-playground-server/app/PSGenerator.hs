{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

module PSGenerator
    ( generate
    ) where

import qualified API
import qualified Auth
import qualified ContractForDifferences
import qualified ContractForDifferencesWithOracle
import Control.Applicative ((<|>))
import Control.Lens (set, (&))
import Control.Monad.Reader (MonadReader)
import qualified CouponBondGuaranteed
import Data.Aeson (encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.Monoid ()
import Data.Proxy (Proxy (Proxy))
import qualified Data.Set as Set ()
import qualified Data.Text.Encoding as T ()
import qualified Data.Text.IO as T ()
import qualified Escrow
import qualified EscrowWithCollateral
import qualified Example
import Language.Haskell.Interpreter (CompilationError, InterpreterError, InterpreterResult, SourceCode, Warning)
import qualified Language.Marlowe as S
import Language.Marlowe.ACTUS.Domain.BusinessEvents as BV (EventType)
import qualified Language.Marlowe.ACTUS.Domain.ContractTerms as CT
import Language.Marlowe.ACTUS.Domain.Schedule as SC (CashFlow)
import Language.Marlowe.Extended
import Language.Marlowe.SemanticsTypes (State (..))
import Language.PureScript.Bridge (BridgePart, Language (Haskell), PSType, SumType, TypeInfo (TypeInfo), argonaut,
                                   buildBridge, equal, genericShow, mkSumType, order, psTypeParameters, typeModule,
                                   typeName, writePSTypes, (^==))
import Language.PureScript.Bridge.Builder (BridgeData)
import Language.PureScript.Bridge.PSTypes (psString)
import Language.PureScript.Bridge.TypeParameters (A, B)
import Marlowe.Contracts (contractForDifferences, contractForDifferencesWithOracle, couponBondGuaranteed, escrow,
                          escrowWithCollateral, example, swap, zeroCouponBond)
import qualified Marlowe.Symbolic.Server as MS
import qualified Marlowe.Symbolic.Types.Request as MSReq
import qualified Marlowe.Symbolic.Types.Response as MSRes
import qualified PSGenerator.Common
import qualified PlutusTx.AssocMap as Map
import Servant ((:<|>), (:>))
import Servant.PureScript (HasBridge, Settings, apiModuleName, defaultBridge, defaultSettings, languageBridge,
                           writeAPIModuleWithSettings)
import qualified Swap
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import qualified Webghc.Server as Webghc
import qualified ZeroCouponBond


psContract :: MonadReader BridgeData m => m PSType
psContract =
    TypeInfo "marlowe-playground-client" "Marlowe.Semantics" "Contract" <$>
    psTypeParameters

contractBridge :: BridgePart
contractBridge = do
    typeName ^== "Contract"
    typeModule ^== "Language.Marlowe.SemanticsTypes"
    psContract

psState :: MonadReader BridgeData m => m PSType
psState =
    TypeInfo "marlowe-playground-client" "Marlowe.Semantics" "State" <$>
    psTypeParameters

stateBridge :: BridgePart
stateBridge = do
    typeName ^== "State"
    typeModule ^== "Language.Marlowe.SemanticsTypes"
    psState

psTransactionInput :: MonadReader BridgeData m => m PSType
psTransactionInput =
    TypeInfo "marlowe-playground-client" "Marlowe.Semantics" "TransactionInput" <$>
    psTypeParameters

transactionInputBridge :: BridgePart
transactionInputBridge = do
    typeName ^== "TransactionInput"
    typeModule ^== "Language.Marlowe.Semantics"
    psTransactionInput

psTransactionWarning :: MonadReader BridgeData m => m PSType
psTransactionWarning =
    TypeInfo "marlowe-playground-client" "Marlowe.Semantics" "TransactionWarning" <$>
    psTypeParameters

transactionWarningBridge :: BridgePart
transactionWarningBridge = do
    typeName ^== "TransactionWarning"
    typeModule ^== "Language.Marlowe.Semantics"
    psTransactionWarning

dayBridge :: BridgePart
dayBridge = typeName ^== "Day" >> return psString

timeBridge :: BridgePart
timeBridge = typeName ^== "LocalTime" >> return psString

myBridge :: BridgePart
myBridge =
    PSGenerator.Common.aesonBridge <|>
    PSGenerator.Common.containersBridge <|>
    PSGenerator.Common.languageBridge <|>
    PSGenerator.Common.ledgerBridge <|>
    PSGenerator.Common.servantBridge <|>
    PSGenerator.Common.miscBridge <|>
    dayBridge <|>
    timeBridge <|>
    contractBridge <|>
    stateBridge <|>
    transactionInputBridge <|>
    transactionWarningBridge <|>
    defaultBridge

data MyBridge

myBridgeProxy :: Proxy MyBridge
myBridgeProxy = Proxy

instance HasBridge MyBridge where
    languageBridge _ = buildBridge myBridge

myTypes :: [SumType 'Haskell]
myTypes =
    PSGenerator.Common.ledgerTypes <>
    PSGenerator.Common.walletTypes <>
    PSGenerator.Common.playgroundTypes <>
    [ argonaut $ mkSumType @SourceCode
    , argonaut $ mkSumType @CompilationError
    , argonaut $ mkSumType @InterpreterError
    , argonaut $ mkSumType @Warning
    , argonaut $ mkSumType @(InterpreterResult A)
    , genericShow . argonaut $ mkSumType @MSRes.Response
    , genericShow . argonaut $ mkSumType @MSRes.Result
    , argonaut $ mkSumType @MSReq.Request
    , argonaut $ mkSumType @(CT.ContractTermsPoly A B)
    , equal . order . genericShow . argonaut $ mkSumType @CT.PYTP
    , equal . order . genericShow . argonaut $ mkSumType @CT.PPEF
    , equal . order . genericShow . argonaut $ mkSumType @CT.SCEF
    , equal . order . genericShow . argonaut $ mkSumType @CT.OPTP
    , equal . order . genericShow . argonaut $ mkSumType @CT.OPXT
    , equal . order . genericShow . argonaut $ mkSumType @CT.DS
    , argonaut $ mkSumType @CT.Cycle
    , equal . order . genericShow . argonaut $ mkSumType @CT.Period
    , equal . order . genericShow . argonaut $ mkSumType @CT.Stub
    , argonaut $ mkSumType @CT.ScheduleConfig
    , argonaut $ mkSumType @CT.ContractStructure
    , equal . order . genericShow . argonaut $ mkSumType @CT.ReferenceType
    , equal . order . genericShow . argonaut $ mkSumType @CT.ReferenceRole
    , equal . order . genericShow . argonaut $ mkSumType @CT.DCC
    , equal . order . genericShow . argonaut $ mkSumType @CT.BDC
    , equal . order . genericShow . argonaut $ mkSumType @CT.EOMC
    , equal . order . genericShow . argonaut $ mkSumType @CT.PRF
    , equal . order . genericShow . argonaut $ mkSumType @CT.FEB
    , equal . order . genericShow . argonaut $ mkSumType @CT.IPCB
    , equal . order . genericShow . argonaut $ mkSumType @CT.CR
    , equal . order . genericShow . argonaut $ mkSumType @CT.CT
    , equal . order . genericShow . argonaut $ mkSumType @CT.Calendar
    , argonaut $ mkSumType @CT.Assertion
    , argonaut $ mkSumType @CT.Assertions
    , argonaut $ mkSumType @CT.AssertionContext
    , argonaut $ mkSumType @Webghc.CompileRequest
    , argonaut $ mkSumType @SC.CashFlow
    , equal . order . genericShow . argonaut $ mkSumType @BV.EventType
    ]

mySettings :: Settings
mySettings = defaultSettings & set apiModuleName "Marlowe"

multilineString :: BS.ByteString -> BS.ByteString -> BS.ByteString
multilineString name value =
    "\n\n" <> name <> " :: String\n" <> name <> " = \"\"\"" <> value <> "\"\"\""

psModule :: BS.ByteString -> BS.ByteString -> BS.ByteString
psModule name body = "module " <> name <> " where" <> body

writeUsecases :: FilePath -> IO ()
writeUsecases outputDir = do
    let haskellUsecases =
            multilineString "example" example
         <> multilineString "escrow" escrow
         <> multilineString "escrowWithCollateral" escrowWithCollateral
         <> multilineString "zeroCouponBond" zeroCouponBond
         <> multilineString "couponBondGuaranteed" couponBondGuaranteed
         <> multilineString "swap" swap
         <> multilineString "contractForDifferences" contractForDifferences
         <> multilineString "contractForDifferencesWithOracle" contractForDifferencesWithOracle
        haskellUsecasesModule = psModule "Examples.Haskell.Contracts" haskellUsecases
    createDirectoryIfMissing True (outputDir </> "Examples" </> "Haskell")
    BS.writeFile (outputDir </> "Examples" </> "Haskell" </> "Contracts.purs") haskellUsecasesModule
    let contractToString = BS8.pack . show . pretty
        marloweUsecases =
            multilineString "example" (contractToString Example.contract)
         <> multilineString "escrow" (contractToString Escrow.contract)
         <> multilineString "escrowWithCollateral" (contractToString EscrowWithCollateral.contract)
         <> multilineString "zeroCouponBond" (contractToString ZeroCouponBond.contract)
         <> multilineString "couponBondGuaranteed" (contractToString CouponBondGuaranteed.contract)
         <> multilineString "swap" (contractToString Swap.contract)
         <> multilineString "contractForDifferences" (contractToString ContractForDifferences.contract)
         <> multilineString "contractForDifferencesWithOracle" (contractToString ContractForDifferencesWithOracle.contract)
        marloweUsecasesModule = psModule "Examples.Marlowe.Contracts" marloweUsecases
    createDirectoryIfMissing True (outputDir </> "Examples" </> "Marlowe")
    BS.writeFile (outputDir </> "Examples" </> "Marlowe" </> "Contracts.purs") marloweUsecasesModule
    putStrLn outputDir

writePangramJson :: FilePath -> IO ()
writePangramJson outputDir = do

    let

        alicePk = S.PK "a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2"

        bobRole = S.Role "Bob"

        const100 = S.Constant 100

        choiceId = S.ChoiceId "choice" alicePk

        valueExpr = S.AddValue const100 (S.SubValue const100 (S.NegValue const100))

        token = S.Token "aa" "name"

    let pangram =
            S.Assert S.TrueObs
                (S.When
                    [ S.Case (S.Deposit alicePk alicePk ada valueExpr)
                        ( S.Let (S.ValueId "x") valueExpr
                            (S.Pay alicePk (S.Party bobRole) ada (S.Cond S.TrueObs (S.UseValue (S.ValueId "x")) (S.UseValue (S.ValueId "y"))) S.Close)
                        )
                    , S.Case (S.Choice choiceId [Bound 0 1, Bound 10 20])
                        ( S.If (S.ChoseSomething choiceId `S.OrObs` (S.ChoiceValue choiceId `S.ValueEQ` const100))
                            (S.Pay alicePk (S.Account alicePk) token (S.DivValue (S.AvailableMoney alicePk token) const100) S.Close)
                            S.Close
                        )
                    , S.Case (S.Notify (S.AndObs (S.SlotIntervalStart `S.ValueLT` S.SlotIntervalEnd) S.TrueObs)) S.Close
                    ]
                    (S.Slot 100)
                    S.Close
                )
        encodedPangram = BS8.pack . Char8.unpack $ encode pangram
        state =
            State
            { accounts = Map.singleton (alicePk, token) 12
            , choices = Map.singleton choiceId 42
            , boundValues = Map.fromList [ (ValueId "x", 1), (ValueId "y", 2) ]
            , minSlot = S.Slot 123
            }
        encodedState = BS8.pack . Char8.unpack $ encode state
    createDirectoryIfMissing True (outputDir </> "JSON")
    BS.writeFile (outputDir </> "JSON" </> "contract.json") encodedPangram
    BS.writeFile (outputDir </> "JSON" </> "state.json") encodedState

type Web = ("api" :> (API.API :<|> Auth.FrontendAPI)) :<|> MS.API :<|> Webghc.FrontendAPI

generate :: FilePath -> IO ()
generate outputDir = do
    writePSTypes outputDir (buildBridge myBridge) myTypes
    writeAPIModuleWithSettings
        mySettings
        outputDir
        myBridgeProxy
        (Proxy @Web)
    writeUsecases outputDir
    writePangramJson outputDir
