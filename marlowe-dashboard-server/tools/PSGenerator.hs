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

import Control.Applicative (Alternative (empty), (<|>))
import Control.Lens (ix, set, view, (&))
import Control.Monad (join)
import Data.Functor (($>))
import Data.Monoid ()
import Data.Proxy (Proxy (Proxy))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as T ()
import qualified Data.Text.IO as T ()
import Language.Marlowe.Client (ContractHistory, EndpointResponse, MarloweEndpointResult, MarloweError, UnspentPayouts)
import Language.Marlowe.Client.History (RolePayout)
import Language.PureScript.Bridge (BridgePart, HasHaskType (haskType), Language (Haskell, PureScript),
                                   SumType (SumType), TypeInfo (..), argonaut, buildBridge, typeModule, typeName, (^==))
import Language.PureScript.Bridge.PSTypes (psNumber, psString)
import Language.PureScript.Bridge.SumType (CustomInstance (..), Instance (..), InstanceImplementation (..),
                                           InstanceMember (..), equal, genericShow, mkSumType, order)
import Language.PureScript.Bridge.TypeInfo (mkTypeInfo, typeParameters)
import Language.PureScript.Bridge.TypeParameters (A, E)
import Marlowe.Run.API (HTTPAPI)
import Marlowe.Run.Wallet.V1.API (GetTotalFundsResponse)
import Marlowe.Run.Wallet.V1.CentralizedTestnet.Types (CreatePostData, CreateResponse, RestorePostData)
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

psCurrencySymbol :: TypeInfo 'PureScript
psCurrencySymbol = TypeInfo "web-common-marlowe" "Language.Marlowe.Core.V1.Semantics.Types" "CurrencySymbol" []

psTokenName :: TypeInfo 'PureScript
psTokenName = TypeInfo "web-common-marlowe" "Language.Marlowe.Core.V1.Semantics.Types" "TokenName" []

psRoleToken :: TypeInfo 'PureScript
psRoleToken = TypeInfo "" "Marlowe.Run.Contract.V1.Types" "RoleToken" []

contractsV1Bridge :: BridgePart
contractsV1Bridge = do
  typeModule ^== "Marlowe.Run.Contract.V1.API"
  typeName ^== "ApiT"
  mTypeName <- view $ haskType . typeParameters . ix 0 . typeName
  case mTypeName of
    "CurrencySymbol" -> pure psCurrencySymbol
    "TokenName"      -> pure psTokenName
    "RoleToken"      -> pure psRoleToken
    _                -> empty

walletV1Bridge :: BridgePart
walletV1Bridge = do
  typeModule ^== "Marlowe.Run.Wallet.V1.Types"
  walletIdBridge <|> walletNameBridge <|> addressBridge

psMnemonic :: TypeInfo 'PureScript
psMnemonic = TypeInfo "" "Data.MnemonicPhrase" "MnemonicPhrase" []

psPassphrase :: TypeInfo 'PureScript
psPassphrase = TypeInfo "" "Data.Passphrase" "Passphrase" []

psAddress :: TypeInfo 'PureScript
psAddress = TypeInfo "" "Data.Address" "Address" []

mnemonicBridge :: BridgePart
mnemonicBridge = (typeName ^== "CreateMnemonic" <|> typeName ^== "RestoreMnemonic") $> psMnemonic

passphraseBridge :: BridgePart
passphraseBridge = (typeName ^== "Passphrase") $> psPassphrase

addressBridge :: BridgePart
addressBridge = (typeName ^== "Address") $> psAddress

walletV1CTBridge :: BridgePart
walletV1CTBridge = do
  typeModule ^== "Marlowe.Run.Wallet.V1.CentralizedTestnet.Types"
  mnemonicBridge <|> passphraseBridge

psTransactionError :: TypeInfo 'PureScript
psTransactionError =  TypeInfo "web-common-marlowe" "Language.Marlowe.Core.V1.Semantics.Types" "TransactionError" []

transactionErrorBridge :: BridgePart
transactionErrorBridge = typeName ^== "TransactionError" >> return psTransactionError

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
  walletV1Bridge <|> walletV1CTBridge <|> paymentPubKeyHashBridge <|> pubKeyHashBridge

psMarloweParams :: TypeInfo 'PureScript
psMarloweParams =  TypeInfo "web-common-marlowe" "Language.Marlowe.Core.V1.Semantics.Types" "MarloweParams" []

marloweParamsBridge :: BridgePart
marloweParamsBridge = typeName ^== "MarloweParams" >> return psMarloweParams

psTransactionInput :: TypeInfo 'PureScript
psTransactionInput =  TypeInfo "web-common-marlowe" "Language.Marlowe.Core.V1.Semantics.Types" "TransactionInput" []

transactionInputBridge :: BridgePart
transactionInputBridge = typeName ^== "TransactionInput" >> return psTransactionInput

psMarloweData :: TypeInfo 'PureScript
psMarloweData =  TypeInfo "web-common-marlowe" "Language.Marlowe.Core.V1.Semantics.Types" "MarloweData" []

marloweDataBridge :: BridgePart
marloweDataBridge = typeName ^== "MarloweData" >> return psMarloweData


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
    <|> contractsV1Bridge
    <|> walletBridge
    <|> transactionErrorBridge
    <|> marloweParamsBridge
    <|> contractInstanceBridge
    <|> transactionInputBridge
    <|> marloweDataBridge
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

customInstance :: forall t. CustomInstance t -> SumType t -> SumType t
customInstance i (SumType ti dc is) = SumType ti dc $ Custom i : is

unspentPayouts :: SumType 'Haskell
unspentPayouts = customInstance monoid $ customInstance semigroup $ mkSumType @UnspentPayouts
  where

    appendImpl = InstanceMember
      "append"
      ["(UnspentPayouts p1)", "(UnspentPayouts p2)"]
      "UnspentPayouts $ nubEq (append p1 p2)"
      [ TypeInfo "purescript-arrays" "Data.Array" "nubEq" []
      , TypeInfo "purescript-prelude" "Data.Semigroup" "append" []
      ]
      mempty

    semigroupImpl = Explicit [ appendImpl ]
    semigroupInstanceHead = TypeInfo "purescript-prelude" "Data.Semigroup" "Semigroup" [mkTypeInfo @UnspentPayouts]
    semigroup = CustomInstance [] semigroupInstanceHead semigroupImpl

    -- We are not able to use newtype deriving here because of: https://github.com/purescript/purescript/issues/3168
    memptyImpl = InstanceMember
      "mempty"
      []
      "UnspentPayouts mempty"
      [TypeInfo "purescript-prelude" "Data.Monoid" "mempty" []]
      mempty

    monoidImpl = Explicit [ memptyImpl ]
    monoidHead = TypeInfo "purescript-prelude" "Data.Monoid" "Monoid" [mkTypeInfo @UnspentPayouts]
    monoid = CustomInstance [] monoidHead monoidImpl

myTypes :: [SumType 'Haskell]
myTypes = join
  [ dto <$>
    [ mkSumType @StreamToServer,
      mkSumType @StreamToClient,
      mkSumType @RestorePostData,
      mkSumType @CreateResponse,
      mkSumType @CreatePostData,
      mkSumType @GetTotalFundsResponse,
      mkSumType @(EndpointResponse A E),
      mkSumType @MarloweEndpointResult,
      mkSumType @WalletInfo,
      mkSumType @ContractHistory,
      mkSumType @RolePayout,
      unspentPayouts
    ]
  , equal . argonaut <$>
    [ mkSumType @MarloweError ]
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
    , ("Plutus.Contract.Error", "ContractError")
    , ("Plutus.Contract.StateMachine", "SMContractError")
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
