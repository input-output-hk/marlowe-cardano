module Main where

import Prologue
import AppM (runAppM)
import Capability.PlutusApps.MarloweApp as MarloweApp
import Data.BigInt.Argonaut as BigInt
import Effect (Effect)
import Effect.AVar as AVar
import Effect.Aff (forkAff, launchAff_)
import Effect.Class (liftEffect)
import Env (Env, WebSocketManager)
import Halogen (hoist)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import MainFrame.State (mkMainFrame)
import MainFrame.Types (Action(..), Msg(..), Query(..))
import WebSocket.Support as WS

mkEnvironment :: WebSocketManager -> Effect Env
mkEnvironment wsManager = do
  contractStepCarouselSubscription <- AVar.empty
  marloweAppEndpointMutex <- MarloweApp.createEndpointMutex
  pure
    { ajaxSettings: { baseURL: "/" }
    , contractStepCarouselSubscription
    , marloweAppEndpointMutex
    , wsManager
    }

main :: Effect Unit
main = do
  runHalogenAff do
    BigInt.withJsonPatch do
      wsManager <- WS.mkWebSocketManager
      environment <- liftEffect $ mkEnvironment wsManager
      body <- awaitBody
      driver <- runUI (hoist (runAppM environment) mkMainFrame) Init body
      void
        $ forkAff
        $ WS.runWebSocketManager
            (WS.URI "/ws")
            ( \msg -> void $ forkAff $ driver.query $ ReceiveWebSocketMessage
                msg
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
