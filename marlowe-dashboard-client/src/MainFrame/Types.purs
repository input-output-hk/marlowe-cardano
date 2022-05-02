module MainFrame.Types where

import Prologue

import Analytics (class IsEvent, defaultEvent, toEvent)
import Data.AddressBook (AddressBook)
import Data.Generic.Rep (class Generic)
import Data.PABConnectedWallet (PABConnectedWallet)
import Data.Time.Duration (Minutes)
import Halogen as H
import Page.Dashboard.Types (Slot) as Dashboard
import Page.Welcome.ConfirmMnemonic.Types as ConfirmMnemonic
import Page.Welcome.CreateWallet.Types as CreateWallet
import Page.Welcome.RestoreWallet.Types as RestoreWallet
import Page.Welcome.Types (Action, State) as Welcome
import Store.Wallet (WalletStore)
import Type.Proxy (Proxy(..))
import Web.Socket.Event.CloseEvent (CloseEvent, reason) as WS

type Slice =
  { addressBook :: AddressBook
  , wallet :: WalletStore
  }

-- The app exists in one of two main subStates: the "welcome" state for when you have
-- no wallet, and all you can do is generate one or create a new one; and the "dashboard"
-- state for when you have selected a wallet, and can do all of the things.
type State =
  { webSocketStatus :: WebSocketStatus
  , tzOffset :: Minutes
  , store :: Slice
  , subState :: Either Welcome.State PABConnectedWallet
  , enteringDashboardState :: Boolean
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
  ( confirmMnemonic :: ConfirmMnemonic.Slot Unit
  , createWallet :: CreateWallet.Slot Unit
  , dashboard :: Dashboard.Slot
  , restoreWallet :: RestoreWallet.Slot Unit
  , toaster :: forall q m. H.Slot q m Unit
  )

_toaster :: Proxy "toaster"
_toaster = Proxy

------------------------------------------------------------
data Query (a :: Type)
data Msg

------------------------------------------------------------
data Action
  = WelcomeAction Welcome.Action
  | Receive Slice
  | Init
  | Tick
  | NewWebSocketStatus WebSocketStatus

-- | Here we decide which top-level queries to track as GA events, and
-- how to classify them.
instance actionIsEvent :: IsEvent Action where
  toEvent Init = Just $ defaultEvent "Init"
  toEvent (WelcomeAction welcomeAction) = toEvent welcomeAction
  toEvent _ = Nothing
