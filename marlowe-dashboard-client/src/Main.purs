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
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import MainFrame.State (mkMainFrame)
import MainFrame.Types (Action(..), Msg(..), Query(..))
import WebSocket.Support as WS
import Store as Store

mkStore :: Store.WebSocketManager -> Effect Store.Store
mkStore wsManager = do
  contractStepCarouselSubscription <- AVar.empty
  marloweAppEndpointMutex <- MarloweApp.createEndpointMutex
  pure
    { ajaxSettings: { baseURL: "/" }
    , contractStepCarouselSubscription
    , marloweAppEndpointMutex
    , wsManager
    , currentSlot: zero
    }

main :: Effect Unit
main = do
  runHalogenAff do
    wsManager <- WS.mkWebSocketManager
    store <- liftEffect $ mkStore wsManager
    body <- awaitBody
    rootComponent <- runAppM store mkMainFrame
    driver <- runUI rootComponent Init body
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
