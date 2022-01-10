module Toast.Lenses
  ( _mToast
  , _toastMessage
  , _expanded
  , _timeoutSubscription
  ) where

import Prologue
import Data.Lens (Lens', _Just)
import Data.Lens.Record (prop)
import Data.Lens.Traversal (Traversal')
import Type.Proxy (Proxy(..))
import Halogen (SubscriptionId)
import Toast.Types (State, ToastMessage, ToastState)

-- TODO: when we upgrade to 0.14 change this to AffineTraversal
_mToast :: Lens' State (Maybe ToastState)
_mToast = prop (Proxy :: _ "mToast")

_toastMessage :: Traversal' State ToastMessage
_toastMessage = _mToast <<< _Just <<< prop (Proxy :: _ "message")

_expanded :: Traversal' State Boolean
_expanded = prop (Proxy :: _ "mToast") <<< _Just <<< prop
  (Proxy :: _ "expanded")

_timeoutSubscription :: Traversal' State SubscriptionId
_timeoutSubscription = prop (Proxy :: _ "mToast") <<< _Just <<< prop
  (Proxy :: _ "timeoutSubscription")
