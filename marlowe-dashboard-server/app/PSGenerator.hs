{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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
import Language.PureScript.Bridge (BridgePart, Language (Haskell), SumType, argonaut, buildBridge, typeName,
                                   writePSTypes, (^==))
import Language.PureScript.Bridge.PSTypes (psNumber, psString)
import Language.PureScript.Bridge.SumType (equal, genericShow, mkSumType, order)
import Marlowe.Run.Webserver.API (HTTPAPI)
import Marlowe.Run.Webserver.Wallet.CentralizedTestnet.Types (CheckPostData, RestoreError, RestorePostData)
import Marlowe.Run.Webserver.Wallet.Types (GetTotalFunds)
import Marlowe.Run.Webserver.WebSocket (StreamToClient, StreamToServer)
import qualified PSGenerator.Common
import Servant.PureScript (HasBridge, Settings, apiModuleName, defaultBridge, defaultSettings, languageBridge,
                           writeAPIModuleWithSettings)
doubleBridge :: BridgePart
doubleBridge = typeName ^== "Double" >> return psNumber

dayBridge :: BridgePart
dayBridge = typeName ^== "Day" >> return psString

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

data MyBridge

myBridgeProxy :: Proxy MyBridge
myBridgeProxy = Proxy

instance HasBridge MyBridge where
  languageBridge _ = buildBridge myBridge

myTypes :: [SumType 'Haskell]
myTypes =
    PSGenerator.Common.ledgerTypes <>
    PSGenerator.Common.walletTypes <>
    -- FIXME: this includes the EndpointDescription, probably they should be sepparated from the playground
    PSGenerator.Common.playgroundTypes <>

  [ equal . genericShow . argonaut $ mkSumType @StreamToServer,
    equal . genericShow . argonaut $ mkSumType @StreamToClient,
    equal . order . genericShow . argonaut $ mkSumType @RestoreError,
    equal . genericShow . argonaut $ mkSumType @RestorePostData,
    equal . genericShow . argonaut $ mkSumType @CheckPostData,
    equal . genericShow . argonaut $ mkSumType @GetTotalFunds
  ]

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
