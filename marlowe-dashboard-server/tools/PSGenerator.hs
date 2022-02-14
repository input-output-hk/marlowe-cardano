{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Main
  ( main,
  )
where

import Control.Applicative ((<|>))
import Control.Lens (set, (&))
import Data.Functor (($>))
import Data.Monoid ()
import Data.Proxy (Proxy (Proxy))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as T ()
import qualified Data.Text.IO as T ()
import Language.Marlowe.Client (EndpointResponse, MarloweEndpointResult, MarloweError)
import Language.PureScript.Bridge (BridgePart, Language (Haskell, PureScript), SumType (SumType), TypeInfo (..),
                                   argonaut, buildBridge, typeModule, typeName, (^==))
import Language.PureScript.Bridge.PSTypes (psNumber, psString)
import Language.PureScript.Bridge.SumType (Instance (..), equal, genericShow, mkSumType, order)
import Language.PureScript.Bridge.TypeParameters (A, E)
import Marlowe.Run.API (HTTPAPI)
import Marlowe.Run.Wallet.V1.API (GetTotalFundsResponse)
import Marlowe.Run.Wallet.V1.CentralizedTestnet.Types (CreatePostData, CreateResponse, RestoreError, RestorePostData)
import Marlowe.Run.Wallet.V1.Types (WalletInfo)
import Marlowe.Run.WebSocket (StreamToClient, StreamToServer)
import MarloweContract (MarloweContract)
import Options.Applicative (Parser, argument, execParser, help, helper, idm, info, metavar, str)
import qualified PSGenerator.Common
import qualified Plutus.PAB.Effects.Contract as Contract
import Plutus.PAB.Effects.Contract.Builtin (Builtin)
import Plutus.PAB.Run.PSGenerator (pabTypes)
import Plutus.PAB.Webserver.API as PAB
import Servant.PureScript (HasBridge, Settings, addTypes, apiModuleName, defaultBridge, defaultSettings,
                           generateWithSettings, languageBridge)

doubleBridge :: BridgePart
doubleBridge = typeName ^== "Double" >> return psNumber

dayBridge :: BridgePart
dayBridge = typeName ^== "Day" >> return psString

psWalletId :: TypeInfo 'PureScript
psWalletId = TypeInfo "" "Data.WalletId" "WalletId" []

psWalletName :: TypeInfo 'PureScript
psWalletName = TypeInfo "" "Data.WalletNickname" "WalletNickname" []

walletIdBridge :: BridgePart
walletIdBridge = (typeName ^== "WalletId") $> psWalletId

walletNameBridge :: BridgePart
walletNameBridge = (typeName ^== "WalletName") $> psWalletName

walletV1Bridge :: BridgePart
walletV1Bridge = do
  typeModule ^== "Marlowe.Run.Wallet.V1.Types"
  walletIdBridge <|> walletNameBridge

psTransactionError :: TypeInfo 'PureScript
psTransactionError =  TypeInfo "web-common-marlowe" "Marlowe.Semantics" "TransactionError" []

transactionErrorBridge :: BridgePart
transactionErrorBridge = typeName ^== "TransactionError" >> return psTransactionError

psMarloweParams :: TypeInfo 'PureScript
psMarloweParams =  TypeInfo "web-common-marlowe" "Marlowe.Semantics" "MarloweParams" []

psPaymentPubKeyHash :: TypeInfo 'PureScript
psPaymentPubKeyHash = TypeInfo "" "Data.PaymentPubKeyHash" "PaymentPubKeyHash" []

paymentPubKeyHashBridge :: BridgePart
paymentPubKeyHashBridge = do
  typeModule ^== "Ledger.Address"
  typeName ^== "PaymentPubKeyHash"
  pure psPaymentPubKeyHash

psPubKeyHash :: TypeInfo 'PureScript
psPubKeyHash = TypeInfo "" "Data.PubKeyHash" "PubKeyHash" []

pubKeyHashBridge :: BridgePart
pubKeyHashBridge = do
  typeModule ^== "Plutus.V1.Ledger.Crypto"
  typeName ^== "PubKeyHash"
  pure psPubKeyHash

walletBridge :: BridgePart
walletBridge = do
  walletV1Bridge <|> paymentPubKeyHashBridge <|> pubKeyHashBridge

marloweParamsBridge :: BridgePart
marloweParamsBridge = typeName ^== "MarloweParams" >> return psMarloweParams

psPlutusAppId :: TypeInfo 'PureScript
psPlutusAppId = TypeInfo "" "Marlowe.PAB" "PlutusAppId" []

contractInstanceBridge :: BridgePart
contractInstanceBridge = typeName ^== "ContractInstanceId" >> return psPlutusAppId

psJson :: TypeInfo 'PureScript
psJson = TypeInfo "argonaut-core" "Data.Argonaut.Core" "Json" []

jsonBridge :: BridgePart
jsonBridge = do
    typeName ^== "Value"
    typeModule ^== "Data.Aeson.Types.Internal"
    pure psJson

myBridge :: BridgePart
myBridge =
  doubleBridge
    <|> jsonBridge
    <|> dayBridge
    <|> defaultBridge
    <|> walletBridge
    <|> transactionErrorBridge
    <|> marloweParamsBridge
    <|> contractInstanceBridge
    <|> PSGenerator.Common.aesonBridge
    <|> PSGenerator.Common.containersBridge
    <|> PSGenerator.Common.ledgerBridge
    <|> PSGenerator.Common.languageBridge
    <|> PSGenerator.Common.servantBridge
    <|> PSGenerator.Common.miscBridge

data MyBridge

myBridgeProxy :: Proxy MyBridge
myBridgeProxy = Proxy

instance HasBridge MyBridge where
  languageBridge _ = buildBridge myBridge

dto :: SumType 'Haskell -> SumType 'Haskell
dto = equal . genericShow . argonaut

myTypes :: [SumType 'Haskell]
myTypes = dto <$>
    [ mkSumType @StreamToServer,
      mkSumType @StreamToClient,
      mkSumType @RestorePostData,
      mkSumType @CreateResponse,
      mkSumType @CreatePostData,
      mkSumType @GetTotalFundsResponse,
      order $ mkSumType @RestoreError,
      mkSumType @(EndpointResponse A E),
      mkSumType @MarloweError,
      mkSumType @MarloweEndpointResult,
      mkSumType @WalletInfo
    ]

marloweRunSettings :: Settings
marloweRunSettings = defaultSettings
  & set apiModuleName "Marlowe.Run.Server"
  & addTypes myTypes

pabSettings :: Settings
pabSettings = defaultSettings
  & set apiModuleName "Plutus.PAB.Webserver"
  & addTypes ( noShow <$> filter notOverridden pabTypes)
  & addTypes [ order . dto $ mkSumType @MarloweContract ]
  where
  -- These types are overridden, and should not be manually generated.
  notOverridden (SumType TypeInfo{..} _ _) = notElem (_typeModule, _typeName) overriddenTypes
  overriddenTypes =
    [ ("Plutus.V1.Ledger.Crypto", "PubKeyHash")
    , ("Ledger.Address", "PaymentPubKeyHash")
    , ("Wallet.Types", "ContractInstanceId")
    ]
  -- We remove the GenericShow instance for the types that rely on Json under the hood
  -- as Json doesn't have a show instance.
  noShow :: SumType lang -> SumType lang
  noShow sumType@(SumType TypeInfo{..} _ _) =
    if elem (_typeModule, _typeName) typesWithNoShow
        then removeShowInstance sumType
        else sumType
  removeShowInstance (SumType typeInfo dataConstructors instances) =
      SumType typeInfo dataConstructors $ filter (/= GenericShow) instances
  typesWithNoShow =
    [ ("Wallet.Types", "Notification")
    , ("Wallet.Types", "NotificationError")
    , ("Plutus.Contract.Checkpoint", "CheckpointStore")
    , ("Plutus.Trace.Emulator.Types", "ContractInstanceLog")
    , ("Plutus.Trace.Emulator.Types", "ContractInstanceMsg")
    , ("Plutus.Trace.Emulator.Types", "EmulatorRuntimeError")
    , ("Plutus.Trace.Emulator.Types", "UserThreadMsg")
    , ("Plutus.PAB.Webserver.Types", "CombinedWSStreamToClient")
    , ("Plutus.PAB.Webserver.Types", "ContractInstanceClientState")
    , ("Plutus.PAB.Webserver.Types", "ContractReport")
    , ("Plutus.PAB.Webserver.Types", "FullReport")
    , ("Plutus.PAB.Webserver.Types", "InstanceStatusToClient")
    , ("Plutus.PAB.Events.ContractInstanceState", "PartiallyDecodedResponse")
    , ("Plutus.Contract.Effects", "ActiveEndpoint")
    , ("Plutus.Contract.Effects", "PABReq")
    , ("Plutus.Contract.Effects", "PABResp")
    ]

argParser :: Parser FilePath
argParser = argument str $ metavar "OUTPUT_DIR" <> help "Output directory to write PureScript files to."

main :: IO ()
main = do
  outputDir <- execParser (info (helper <*> argParser) idm)
  generateWithSettings marloweRunSettings outputDir myBridgeProxy (Proxy @HTTPAPI)
  -- We need to manually generate this instead of using generateAPIModule,
  -- because we override some of the pab bridge.
  generateWithSettings
    pabSettings
    outputDir
    myBridgeProxy
    (Proxy @(PAB.API (Contract.ContractDef (Builtin MarloweContract)) Text.Text))
