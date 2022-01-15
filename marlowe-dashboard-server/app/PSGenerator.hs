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
import Language.PureScript.Bridge (BridgePart, Language (Haskell), SumType, argonaut, buildBridge, typeName,
                                   writePSTypes, (^==))
import Language.PureScript.Bridge.PSTypes (psNumber, psString)
import Language.PureScript.Bridge.SumType (equal, genericShow, mkSumType, order)
import Marlowe.Run.API (HTTPAPI)
import Marlowe.Run.Dto
import Marlowe.Run.Wallet.API (GetTotalFundsResponse)
import Marlowe.Run.Wallet.CentralizedTestnet.Types (CheckPostData, RestoreError, RestorePostData)
import Marlowe.Run.WebSocket (StreamToClient, StreamToServer)
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

dto :: SumType 'Haskell -> SumType 'Haskell
dto = equal . genericShow . argonaut

-- FIXME: remove all of this shared stuff from plutus-apps. We should only be
-- exporting API types to PureScript, and those should all be defined
-- internally in this project. With a multi-repo setup, there is far too much
-- potential for breakage with updates.
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
        mkSumType @CurrencySymbolDto,
        mkSumType @TokenNameDto,
        mkSumType @WalletIdDto,
        mkSumType @AssetsDto,
        order . mkSumType @RestoreError
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
