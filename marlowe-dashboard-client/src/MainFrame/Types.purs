module MainFrame.Types where

import Prologue

import Analytics (class IsEvent, defaultEvent, toEvent)
import Component.Contacts.Types (AddressBook, WalletDetails)
import Component.Expand as Expand
import Component.LoadingSubmitButton.Types as LoadingSubmitButton
import Component.Tooltip.Types (ReferenceId)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Time.Duration (Minutes)
import Halogen as H
import Halogen.Extra (LifecycleEvent)
import Halogen.Form as Form
import Marlowe.PAB (PlutusAppId)
import Marlowe.Semantics (Slot)
import Page.Contract.Types (State) as Contract
import Page.Dashboard.Types (Action, State) as Dashboard
import Page.Welcome.Types (Action, State) as Welcome
import Plutus.PAB.Webserver.Types (CombinedWSStreamToClient)
import Type.Proxy (Proxy(..))
import Web.Socket.Event.CloseEvent (CloseEvent, reason) as WS
import WebSocket.Support (FromSocket) as WS

-- The app exists in one of two main subStates: the "welcome" state for when you have
-- no wallet, and all you can do is generate one or create a new one; and the "dashboard"
-- state for when you have selected a wallet, and can do all of the things.
type State =
  { webSocketStatus :: WebSocketStatus
  -- TODO: Both currentSlot and tzOffset should be stored in the global store rather than here, but in order
  --       to remove it from here we need to first change the sub-components that use this into proper components
  , currentSlot :: Slot
  , tzOffset :: Minutes
  , subState :: Either Welcome.State Dashboard.State
  }

data WebSocketStatus
  = WebSocketOpen
  | WebSocketClosed (Maybe WS.CloseEvent)

derive instance genericWebSocketStatus :: Generic WebSocketStatus _

instance showWebSocketStatus :: Show WebSocketStatus where
  show WebSocketOpen = "WebSocketOpen"
  show (WebSocketClosed Nothing) = "WebSocketClosed"
  show (WebSocketClosed (Just closeEvent)) = "WebSocketClosed " <> WS.reason
    closeEvent

------------------------------------------------------------
type ChildSlots =
  ( tooltipSlot :: forall query. H.Slot query Void ReferenceId
  , hintSlot :: forall query. H.Slot query Void String
  , submitButtonSlot :: H.Slot LoadingSubmitButton.Query Unit String
  , lifeCycleSlot :: forall query. H.Slot query LifecycleEvent String
  , expandSlot :: Expand.Slot Void String
  , restoreWalletForm :: Form.Slot Action (Tuple String String) Unit
  , toaster :: forall q m. H.Slot q m Unit
  )

_toaster :: Proxy "toaster"
_toaster = Proxy

------------------------------------------------------------
data Query a
  = ReceiveWebSocketMessage (WS.FromSocket CombinedWSStreamToClient) a
  | MainFrameActionQuery Action a

data Msg
  = MainFrameActionMsg Action

------------------------------------------------------------
data Action
  = Init
  | EnterWelcomeState
      AddressBook
      WalletDetails
      (Map PlutusAppId Contract.State)
  | EnterDashboardState AddressBook WalletDetails
  | WelcomeAction Welcome.Action
  | DashboardAction Dashboard.Action

-- | Here we decide which top-level queries to track as GA events, and
-- how to classify them.
instance actionIsEvent :: IsEvent Action where
  toEvent Init = Just $ defaultEvent "Init"
  toEvent (EnterWelcomeState _ _ _) = Just $ defaultEvent "EnterWelcomeState"
  toEvent (EnterDashboardState _ _) = Just $ defaultEvent "EnterDashboardState"
  toEvent (WelcomeAction welcomeAction) = toEvent welcomeAction
  toEvent (DashboardAction dashboardAction) = toEvent dashboardAction
