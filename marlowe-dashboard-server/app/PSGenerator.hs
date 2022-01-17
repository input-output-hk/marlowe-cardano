{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module PSGenerator
  ( generate,
  )
where

import Control.Applicative ((<|>))
import Control.Lens (set, (&))
import Data.Monoid ()
import Data.Proxy (Proxy (Proxy))
import qualified Data.Text.Encoding as T ()
import qualified Data.Text.IO as T ()
import Language.PureScript.Bridge (BridgePart, Language (Haskell, PureScript), SumType, TypeInfo (..), argonaut,
                                   buildBridge, typeName, writePSTypes, (^==))
import Language.PureScript.Bridge.PSTypes (psNumber, psString)
import Language.PureScript.Bridge.SumType (equal, genericShow, mkSumType, order)
import Marlowe.Run.API (HTTPAPI)
import Marlowe.Run.Wallet.V1.API (GetTotalFundsResponse)
import Marlowe.Run.Wallet.V1.CentralizedTestnet.Types (CheckPostData, RestoreError, RestorePostData)
import Marlowe.Run.WebSocket (StreamToClient, StreamToServer)
import qualified PSGenerator.Common
import Servant.PureScript (HasBridge, Settings, apiModuleName, defaultBridge, defaultSettings, languageBridge,
                           writeAPIModuleWithSettings)
doubleBridge :: BridgePart
doubleBridge = typeName ^== "Double" >> return psNumber

dayBridge :: BridgePart
dayBridge = typeName ^== "Day" >> return psString

psWalletId :: TypeInfo 'PureScript
psWalletId = TypeInfo "marlowe-dashboard-client" "Component.Contacts.Types" "WalletId" []

walletIdBridge :: BridgePart
walletIdBridge = typeName ^== "HttpWalletId" >> return psWalletId

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
    ( dto <$>
      [ mkSumType @StreamToServer,
        mkSumType @StreamToClient,
        mkSumType @RestorePostData,
        mkSumType @CheckPostData,
        mkSumType @GetTotalFundsResponse,
        order $ mkSumType @RestoreError
      ]
    )

mySettings :: Settings
mySettings = defaultSettings & set apiModuleName "Marlowe"

generate :: FilePath -> IO ()
generate outputDir = do
  writeAPIModuleWithSettings
    mySettings
    outputDir
    myBridgeProxy
    (Proxy @HTTPAPI)
  writePSTypes outputDir (buildBridge myBridge) myTypes
