module Component.Toast.Lenses where

import Prologue

import Component.Toast.Types (State, ToastEntry, ToastIndex(..), ToastMessage)
import Data.Lens (Lens', _Just)
import Data.Lens.Record (prop)
import Data.Lens.Traversal (Traversal')
import Data.List (List)
import Data.Map (Map)
import Halogen (SubscriptionId)
import Type.Proxy (Proxy(..))

_toasts :: Lens' State (List ToastEntry)
_toasts = prop (Proxy :: _ "toasts")

-- _toastMessage :: Traversal' State ToastMessage
-- _toastMessage = _mToast <<< _Just <<< prop (Proxy :: _ "message")

_expanded :: Lens' State (Maybe ToastIndex)
_expanded = prop (Proxy :: _ "expanded")

_timeoutSubscriptions :: Lens' State (Map ToastIndex SubscriptionId)
_timeoutSubscriptions = prop (Proxy :: _ "timeoutSubscriptions")
