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
import Control.Monad.Error.Class (throwError)
import Data.AddressBook as AddressBook
import Data.Argonaut
  ( class DecodeJson
  , Json
  , JsonDecodeError
  , decodeJson
  , printJsonDecodeError
  , (.:)
  )
import Data.Argonaut.Extra (parseDecodeJson)
import Data.Either (either, hush)
import Data.Maybe (fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.AVar as AVar
import Effect.Aff (error, forkAff, launchAff_)
import Effect.Class (liftEffect)
import Env (Env(..), WebSocketManager)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import Humanize (getTimezoneOffset)
import LocalStorage (getItem)
import MainFrame.State (mkMainFrame)
import MainFrame.Types (Msg(..), Query(..))
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
    webpackBuildMode <- obj .: "webpackBuildMode" <#>
      if _ then Development
      else Production
    in MainArgs { pollingInterval, webpackBuildMode }

mkEnv :: Milliseconds -> WebSocketManager -> WebpackBuildMode -> Effect Env
mkEnv pollingInterval wsManager webpackBuildMode = do
  contractStepCarouselSubscription <- AVar.empty
  marloweAppEndpointMutex <- MarloweApp.createEndpointMutex
  pure $ Env
    { contractStepCarouselSubscription
    , logger: case webpackBuildMode of
        -- Add backend logging capability
        Production -> mempty
        Development -> Console.logger identity
    , marloweAppEndpointMutex
    , wsManager
    , pollingInterval
    }

exitBadArgs :: forall a. JsonDecodeError -> Effect a
exitBadArgs e = throwError
  $ error
  $ "Failed to start: bad startup args.\n\n" <> printJsonDecodeError e

main :: Json -> Effect Unit
main args = do
  MainArgs { pollingInterval, webpackBuildMode } <- either exitBadArgs pure $
    decodeJson args
  tzOffset <- getTimezoneOffset
  addressBookJson <- getItem addressBookLocalStorageKey
  -- TODO this is for dev purposes only. The need for this should go away when
  -- we have proper wallet integration with a full node or light wallet.
  -- FIXME-3208
  -- walletJson <- getItem walletLocalStorageKey
  let
    addressBook =
      fromMaybe AddressBook.empty $ hush <<< parseDecodeJson =<< addressBookJson
  -- FIXME-3208
  -- wallet = hush <<< parseDecodeJson =<< walletJson

  runHalogenAff do
    wsManager <- WS.mkWebSocketManager
    env <- liftEffect $ mkEnv pollingInterval wsManager webpackBuildMode
    let
      -- FIXME-3208
      -- store = mkStore addressBook wallet
      store = mkStore addressBook Nothing
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
