module Main
  ( main
  ) where

import Prologue

import AppM (runAppM)
import Capability.MarloweStorage as MarloweStorage
import Control.Logger.Effect.Console (logger) as Console
import Control.Monad.Error.Class (throwError)
import Control.Monad.Now (makeClock, now)
import Data.Argonaut
  ( class DecodeJson
  , Json
  , JsonDecodeError
  , decodeJson
  , printJsonDecodeError
  , (.:)
  )
import Data.Either (either)
import Data.Map as Map
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Effect (Effect)
import Effect.AVar as AVar
import Effect.Aff (error, forkAff, launchAff_)
import Effect.Class (liftEffect)
import Env (Env(..), WebSocketManager)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import MainFrame.State (mkMainFrame)
import MainFrame.Types (Msg(..))
import MainFrame.Types as MainFrame
import Store (mkStore)
import WebSocket.Support as WS

newtype MainArgs = MainArgs
  { pollingInterval :: Milliseconds
  , webpackBuildMode :: WebpackBuildMode
  }

data WebpackBuildMode = Production | Development

instance DecodeJson MainArgs where
  decodeJson = decodeJson >=> \obj -> ado
    pollingInterval <- Milliseconds <$> obj .: "pollingInterval"
    webpackBuildMode <- obj .: "webpackDevelMode" <#>
      if _ then Development
      else Production
    in MainArgs { pollingInterval, webpackBuildMode }

mkEnv :: WebSocketManager -> WebpackBuildMode -> Effect Env
mkEnv wsManager webpackBuildMode = do
  contractStepCarouselSubscription <- AVar.empty
  endpointSemaphores <- AVar.new Map.empty
  createListeners <- AVar.new Map.empty
  applyInputListeners <- AVar.new Map.empty
  redeemListeners <- AVar.new Map.empty
  pure $ Env
    { contractStepCarouselSubscription
    , logger: case webpackBuildMode of
        -- Add backend logging capability
        Production -> mempty
        Development -> Console.logger identity
    , endpointSemaphores
    , wsManager
    , createListeners
    , applyInputListeners
    , redeemListeners
    }

exitBadArgs :: forall a. JsonDecodeError -> Effect a
exitBadArgs e = throwError
  $ error
  $ "Failed to start: bad startup args.\n\n" <> printJsonDecodeError e

main :: Json -> Effect Unit
main args = do
  MainArgs { pollingInterval, webpackBuildMode } <- either exitBadArgs pure $
    decodeJson args
  addressBook <- MarloweStorage.getAddressBook
  contractNicknames <- MarloweStorage.getContractNicknames
  runHalogenAff do
    wsManager <- WS.mkWebSocketManager
    env <- liftEffect $ mkEnv wsManager webpackBuildMode
    currentTime <- now
    let store = mkStore currentTime addressBook contractNicknames
    body <- awaitBody
    rootComponent <- runAppM env store mkMainFrame
    pabWebsocket <- liftEffect HS.create
    clock <- liftEffect $ map void $ makeClock $ Seconds 1.0
    walletRegular <- liftEffect $ map void $ makeClock pollingInterval
    walletSync <- liftEffect $ map void $ makeClock $ Milliseconds 500.0
    let
      input =
        { sources:
            { pabWebsocket: pabWebsocket.emitter
            , clock
            , polling: { walletRegular, walletSync }
            }
        }
    driver <- runUI rootComponent input body
    void $ forkAff $ WS.runWebSocketManager
      (WS.URI "/pab/ws")
      (liftEffect <<< HS.notify pabWebsocket.listener)
      wsManager

    -- This handler allows us to call an action in the MainFrame from a child component
    -- (more info in the MainFrameLoop capability)
    void
      $ liftEffect
      $ HS.subscribe driver.messages
      $ \(MainFrameActionMsg action) -> launchAff_ $ void $ driver.query $
          MainFrame.MainFrameActionQuery action unit
