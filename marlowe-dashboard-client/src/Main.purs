module Main
  ( main
  ) where

import Prologue

import AppM (runAppM)
import Capability.MarloweStorage
  ( addressBookLocalStorageKey
  , walletLocalStorageKey
  )
import Capability.PlutusApps.MarloweApp as MarloweApp
import Control.Logger.Effect.Console (logger) as Console
import Data.AddressBook as AddressBook
import Data.Argonaut.Extra (parseDecodeJson)
import Data.Either (hush)
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Effect.AVar as AVar
import Effect.Aff (forkAff, launchAff_)
import Effect.Class (liftEffect)
import Env (Env(..), WebSocketManager)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import Humanize (getTimezoneOffset)
import LocalStorage (getItem)
import MainFrame.State (mkMainFrame)
import MainFrame.Types (Msg(..), Query(..))
import WebSocket.Support as WS

mkEnv :: WebSocketManager -> Effect Env
mkEnv wsManager = do
  contractStepCarouselSubscription <- AVar.empty
  marloweAppEndpointMutex <- MarloweApp.createEndpointMutex
  pure $ Env
    { contractStepCarouselSubscription
    -- FIXME: Configure logger using bundle build
    -- context (devel vs production etc.)
    , logger: Console.logger identity
    , marloweAppEndpointMutex
    , wsManager
    }

main :: Effect Unit
main = do
  tzOffset <- getTimezoneOffset
  addressBookJson <- getItem addressBookLocalStorageKey
  -- TODO this is for dev purposes only. The need for this should go away when
  -- we have proper wallet integration with a full node or light wallet.
  walletJson <- getItem walletLocalStorageKey
  let
    addressBook =
      fromMaybe AddressBook.empty $ hush <<< parseDecodeJson =<< addressBookJson
    wallet = hush <<< parseDecodeJson =<< walletJson

  runHalogenAff do
    wsManager <- WS.mkWebSocketManager
    env <- liftEffect $ mkEnv wsManager
    let
      store =
        { addressBook
        , currentSlot: zero
        , toast: Nothing
        , wallet
        , previousCompanionAppState: Nothing
        }
    body <- awaitBody
    rootComponent <- runAppM env store mkMainFrame
    driver <- runUI rootComponent { tzOffset } body
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
