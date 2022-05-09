module Component.Toast.Lenses where

import Component.Toast.Types (State, ToastEntry, ToastIndex)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.List (List)
import Data.Map (Map)
import Halogen (SubscriptionId)
import Type.Proxy (Proxy(..))

_toasts :: Lens' State (List ToastEntry)
_toasts = prop (Proxy :: _ "toasts")

_expanded :: Lens' ToastEntry Boolean
_expanded = prop (Proxy :: _ "expanded")

_timeoutSubscriptions :: Lens' State (Map ToastIndex SubscriptionId)
_timeoutSubscriptions = prop (Proxy :: _ "timeoutSubscriptions")
