module Main
  ( main
  ) where

import Prologue

import Affjax as Affjax
import AppM (runAppM)
import Capability.MarloweStorage as MarloweStorage
import Control.Concurrent.EventBus as EventBus
import Control.Logger.Effect.Console (structuredLogger) as Console
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
import Data.DateTime.Instant (Instant)
import Data.Either (either)
import Data.Map as Map
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.AVar as AVar
import Effect.Aff (error, forkAff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Now (getTimezoneOffset)
import Env (Env(..), HandleRequest(..), MakeClock(..), Sinks, Sources)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import LocalStorage (getItem, removeItem, setItem)
import MainFrame.State (mkMainFrame)
import MainFrame.Types (Msg(..))
import MainFrame.Types as MainFrame
import Store (mkStore)
import Transcript (Transcript, TranscriptEvent(..))
import WebSocket.Support (FromSocket(..))
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

foreign import transcribe :: Tuple Instant TranscriptEvent -> Effect Unit
foreign import setShowTranscript :: (Transcript -> String) -> Effect Unit

mkEnv
  :: Milliseconds
  -> Milliseconds
  -> Sources
  -> Sinks
  -> WebpackBuildMode
  -> Effect Env
mkEnv regularPollInterval syncPollInterval sources sinks webpackBuildMode = do
  contractStepCarouselSubscription <- AVar.empty
  endpointSemaphores <- AVar.new Map.empty
  createListeners <- AVar.new Map.empty
  applyInputListeners <- AVar.new Map.empty
  redeemListeners <- AVar.new Map.empty
  followerBus <- EventBus.create
  timezoneOffset <- getTimezoneOffset
  pure $ Env
    { contractStepCarouselSubscription
    , logger: case webpackBuildMode of
        -- Add backend logging capability
        Production -> mempty
        Development -> Console.structuredLogger
    , followerBus
    , endpointSemaphores
    , createListeners
    , applyInputListeners
    , redeemListeners
    , sinks
    , sources
    , handleRequest: HandleRequest Affjax.request
    , timezoneOffset
    , makeClock: MakeClock makeClock
    , localStorage:
        { getItem
        , setItem
        , removeItem
        }
    , regularPollInterval
    , syncPollInterval
    }

exitBadArgs :: forall a. JsonDecodeError -> Effect a
exitBadArgs e = throwError
  $ error
  $ "Failed to start: bad startup args.\n\n" <> printJsonDecodeError e

main :: Json -> Effect Unit
main args = do
  MainArgs { pollingInterval, webpackBuildMode } <- either exitBadArgs pure $
    decodeJson args
  setShowTranscript show
  addressBook <- MarloweStorage.getAddressBook
  contractNicknames <- MarloweStorage.getContractNicknames
  runHalogenAff do
    wsManager <- WS.mkWebSocketManager
    pabWebsocketIn <- liftEffect HS.create
    void $ forkAff $ WS.runWebSocketManager
      (WS.URI "/pab/ws")
      ( \msg -> liftEffect do
          case msg of
            ReceiveMessage msg' -> do
              currentTime <- now
              transcribe $ Tuple currentTime $ WebSocketMsgReceived msg'
            _ -> pure unit
          HS.notify pabWebsocketIn.listener msg
      )
      wsManager
    pabWebsocketOut <- liftEffect HS.create
    void
      $ forkAff
      $ liftEffect
      $ HS.subscribe pabWebsocketOut.emitter \msg -> do
          currentTime <- now
          transcribe $ Tuple currentTime $ WebSocketMsgSent msg
          launchAff_ $ WS.managerWriteOutbound wsManager $ WS.SendMessage msg
    let
      sources =
        { pabWebsocket: pabWebsocketIn.emitter
        , currentTime: now
        }
      sinks = { pabWebsocket: pabWebsocketOut.listener }
    env <- liftEffect $ mkEnv
      pollingInterval
      (Milliseconds 500.0)
      sources
      sinks
      webpackBuildMode
    currentTime <- now
    let store = mkStore currentTime addressBook contractNicknames
    body <- awaitBody
    rootComponent <- runAppM env store mkMainFrame
    driver <- runUI rootComponent unit body

    -- This handler allows us to call an action in the MainFrame from a child component
    -- (more info in the MainFrameLoop capability)
    void
      $ liftEffect
      $ HS.subscribe driver.messages
      $ \(MainFrameActionMsg action) -> launchAff_ $ void $ driver.query $
          MainFrame.MainFrameActionQuery action unit
