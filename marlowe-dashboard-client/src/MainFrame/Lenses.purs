module MainFrame.Lenses where

import Prologue

import Data.AddressBook (AddressBook)
import Data.Lens (Lens', Traversal')
import Data.Lens.Prism.Either (_Left, _Right)
import Data.Lens.Record (prop)
import Data.Time.Duration (Minutes)
import MainFrame.Types (State, WebSocketStatus)
import Marlowe.Semantics (Slot)
import Page.Dashboard.Types (State) as Dashboard
import Page.Welcome.Types (State) as Welcome
import Type.Proxy (Proxy(..))

_webSocketStatus :: Lens' State WebSocketStatus
_webSocketStatus = prop (Proxy :: _ "webSocketStatus")

_currentSlot :: Lens' State Slot
_currentSlot = prop (Proxy :: _ "currentSlot")

_addressBook :: Lens' State AddressBook
_addressBook = prop (Proxy :: _ "addressBook")

_tzOffset :: Lens' State Minutes
_tzOffset = prop (Proxy :: _ "tzOffset")

_subState :: Lens' State (Either Welcome.State Dashboard.State)
_subState = prop (Proxy :: _ "subState")

_welcomeState :: Traversal' State Welcome.State
_welcomeState = _subState <<< _Left

_dashboardState :: Traversal' State Dashboard.State
_dashboardState = _subState <<< _Right
