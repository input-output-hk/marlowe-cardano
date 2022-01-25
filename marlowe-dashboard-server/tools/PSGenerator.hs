{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Main
  ( main,
  )
where

import Control.Applicative ((<|>))
import Control.Lens (set, (&))
import Data.Monoid ()
import Data.Proxy (Proxy (Proxy))
import qualified Data.Text.Encoding as T ()
import qualified Data.Text.IO as T ()
import Language.Marlowe.Client (EndpointResponse, MarloweEndpointResult, MarloweError)
import Language.PureScript.Bridge (BridgePart, Language (Haskell, PureScript), SumType, TypeInfo (..), argonaut,
                                   buildBridge, typeName, (^==))
import Language.PureScript.Bridge.PSTypes (psNumber, psString)
import Language.PureScript.Bridge.SumType (equal, genericShow, mkSumType, order)
import Language.PureScript.Bridge.TypeParameters (A, E)
import Marlowe.Run.API (HTTPAPI)
import Marlowe.Run.Wallet.V1.API (GetTotalFundsResponse)
import Marlowe.Run.Wallet.V1.CentralizedTestnet.Types (CreatePostData, CreateResponse, RestoreError, RestorePostData)
import Marlowe.Run.WebSocket (StreamToClient, StreamToServer)
import MarloweContract (MarloweContract)
import Options.Applicative (Parser, argument, execParser, help, helper, idm, info, metavar, str)
import qualified PSGenerator.Common
import Plutus.PAB.Run.PSGenerator (generateAPIModule)
import Servant.PureScript (HasBridge, Settings, addTypes, apiModuleName, defaultBridge, defaultSettings,
                           generateWithSettings, languageBridge)

doubleBridge :: BridgePart
doubleBridge = typeName ^== "Double" >> return psNumber

dayBridge :: BridgePart
dayBridge = typeName ^== "Day" >> return psString

psWalletId :: TypeInfo 'PureScript
psWalletId = TypeInfo "marlowe-dashboard-client" "Component.Contacts.Types" "WalletId" []

walletIdBridge :: BridgePart
walletIdBridge = typeName ^== "HttpWalletId" >> return psWalletId

psTransactionError :: TypeInfo 'PureScript
psTransactionError =  TypeInfo "web-common-marlowe" "Marlowe.Semantics" "TransactionError" []

transactionErrorBridge :: BridgePart
transactionErrorBridge = typeName ^== "TransactionError" >> return psTransactionError

psMarloweParams :: TypeInfo 'PureScript
psMarloweParams =  TypeInfo "web-common-marlowe" "Marlowe.Semantics" "MarloweParams" []

marloweParamsBridge :: BridgePart
marloweParamsBridge = typeName ^== "MarloweParams" >> return psMarloweParams


myBridge :: BridgePart
myBridge =
  PSGenerator.Common.aesonBridge <|> PSGenerator.Common.containersBridge
    <|> PSGenerator.Common.ledgerBridge
    <|> PSGenerator.Common.languageBridge
    <|> PSGenerator.Common.servantBridge
    <|> PSGenerator.Common.miscBridge
    <|> doubleBridge
    <|> dayBridge
    <|> defaultBridge
    <|> walletIdBridge
    <|> transactionErrorBridge
    <|> marloweParamsBridge

data MyBridge

myBridgeProxy :: Proxy MyBridge
myBridgeProxy = Proxy

instance HasBridge MyBridge where
  languageBridge _ = buildBridge myBridge

dto :: SumType 'Haskell -> SumType 'Haskell
dto = equal . genericShow . argonaut

myTypes :: [SumType 'Haskell]
myTypes =
    PSGenerator.Common.ledgerTypes <>
    PSGenerator.Common.walletTypes <>
    PSGenerator.Common.playgroundTypes <>
    -- | `WalletInfo` has no `Eq` instance hence
    -- | `CreateResponse` should not have one.
    [ genericShow . argonaut $ mkSumType @CreateResponse ] <>
    ( dto <$>
        [ mkSumType @StreamToServer,
          mkSumType @StreamToClient,
          mkSumType @RestorePostData,
          mkSumType @CreatePostData,
          mkSumType @GetTotalFundsResponse,
          order $ mkSumType @RestoreError,
          -- We put the Client.hs types here as there is no
          -- PSGenerator for the MarlowePAB
          mkSumType @(EndpointResponse A E),
          mkSumType @MarloweError,
          mkSumType @MarloweEndpointResult
        ]
    )

mySettings :: Settings
mySettings = defaultSettings
  & set apiModuleName "Marlowe.Run.Server"
  & addTypes myTypes

argParser :: Parser FilePath
argParser = argument str $ metavar "OUTPUT_DIR" <> help "Output directory to write PureScript files to."

main :: IO ()
main = do
  outputDir <- execParser (info (helper <*> argParser) idm)
  generateWithSettings mySettings outputDir myBridgeProxy (Proxy @HTTPAPI)
  generateAPIModule (Proxy @MarloweContract) outputDir
