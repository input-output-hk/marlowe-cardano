module MainFrame.Lenses where

import Prologue

import Data.AddressBook (AddressBook)
import Data.Lens (Lens')
import Data.Lens.AffineTraversal (AffineTraversal')
import Data.Lens.Prism.Either (_Left, _Right)
import Data.Lens.Record (prop)
import Data.PABConnectedWallet (PABConnectedWallet)
import Data.Time.Duration (Minutes)
import MainFrame.Types (Slice, State, WebSocketStatus)
import Page.Welcome.Types (State) as Welcome
import Type.Proxy (Proxy(..))

_webSocketStatus :: Lens' State WebSocketStatus
_webSocketStatus = prop (Proxy :: _ "webSocketStatus")

_addressBook :: Lens' State AddressBook
_addressBook = _store <<< prop (Proxy :: _ "addressBook")

_tzOffset :: Lens' State Minutes
_tzOffset = prop (Proxy :: _ "tzOffset")

_subState :: Lens' State (Either Welcome.State PABConnectedWallet)
_subState = prop (Proxy :: _ "subState")

_store :: Lens' State Slice
_store = prop (Proxy :: _ "store")

_enteringDashboardState :: Lens' State Boolean
_enteringDashboardState = prop (Proxy :: _ "enteringDashboardState")

_welcomeState :: AffineTraversal' State Welcome.State
_welcomeState = _subState <<< _Left

_dashboardState :: AffineTraversal' State PABConnectedWallet
_dashboardState = _subState <<< _Right
