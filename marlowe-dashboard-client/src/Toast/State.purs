module Toast.State
  ( defaultState
  , handleAction
  ) where

import Prologue
import Data.Foldable (for_)
import Data.Lens (assign)
import Data.Lens.Extra (peruse)
import Data.Time.Duration (Milliseconds(..))
import Effect.Class (liftEffect)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Halogen
  ( HalogenM
  , RefLabel(..)
  , getHTMLElementRef
  , subscribe
  , unsubscribe
  )
import Halogen.Animation (animateAndWaitUntilFinishSubscription)
import Halogen.Subscription as HS
import Toast.Lenses (_expanded, _mToast, _timeoutSubscription)
import Toast.Types (Action(..), State, ToastMessage)

defaultState :: State
defaultState =
  { mToast: Nothing
  }

toastTimeoutSubscription :: ToastMessage -> HS.Emitter Action
toastTimeoutSubscription toast =
  HS.makeEmitter \push -> do
    Aff.launchAff_ do
      Aff.delay $ Milliseconds toast.timeout
      liftEffect $ push ToastTimeout
    pure $ pure unit

handleAction
  :: forall m slots msg
   . MonadAff m
  => Action
  -> HalogenM State Action slots msg m Unit
handleAction (AddToast toast) = do
  timeoutSubscription <- subscribe $ toastTimeoutSubscription toast
  assign _mToast (Just { message: toast, expanded: false, timeoutSubscription })

handleAction ExpandToast = do
  mSubscriptionId <- peruse _timeoutSubscription
  assign _expanded true
  for_ mSubscriptionId unsubscribe

handleAction CloseToast = assign _mToast Nothing

handleAction ToastTimeout = do
  mElement <- getHTMLElementRef (RefLabel "collapsed-toast")
  for_ mElement $ subscribe <<< animateAndWaitUntilFinishSubscription
    "to-bottom"
    CloseToast
