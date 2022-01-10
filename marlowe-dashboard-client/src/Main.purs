module Main
  ( main
  ) where

import Prologue

import AppM (runAppM)
import Capability.PlutusApps.MarloweApp as MarloweApp
import Effect (Effect)
import Effect.AVar as AVar
import Effect.Aff (forkAff, launchAff_)
import Effect.Class (liftEffect)
import Env (Env(..), WebSocketManager)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import MainFrame.State (mkMainFrame)
import MainFrame.Types (Msg(..), Query(..))
import WebSocket.Support as WS

mkEnv :: WebSocketManager -> Effect Env
mkEnv wsManager = do
  contractStepCarouselSubscription <- AVar.empty
  marloweAppEndpointMutex <- MarloweApp.createEndpointMutex
  pure $ Env
    { ajaxSettings: { baseURL: "/" }
    , contractStepCarouselSubscription
    , marloweAppEndpointMutex
    , wsManager
    }

main :: Effect Unit
main = do
  runHalogenAff do
    wsManager <- WS.mkWebSocketManager
    env <- liftEffect $ mkEnv wsManager
    let store = { currentSlot: zero, toast: Nothing }
    body <- awaitBody
    rootComponent <- runAppM env store mkMainFrame
    driver <- runUI rootComponent unit body
    void
      $ forkAff
      $ WS.runWebSocketManager
          (WS.URI "/pab/ws")
          ( \msg -> void $ forkAff $ driver.query $ ReceiveWebSocketMessage msg
              unit
          )
          wsManager
    -- This handler allows us to call an action in the MainFrame from a child component
    -- (more info in the MainFrameLoop capability)
    void
      $ liftEffect
      $ HS.subscribe driver.messages
      $ \(MainFrameActionMsg action) -> launchAff_ $ void $ driver.query $
          MainFrameActionQuery action unit
