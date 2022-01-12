module Toast.State (component) where

import Prologue

import Data.Foldable (for_)
import Data.Lens (assign)
import Data.Lens.Extra (peruse)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen
  ( HalogenM
  , RefLabel(..)
  , getHTMLElementRef
  , subscribe
  , unsubscribe
  )
import Halogen as H
import Halogen.Animation (animateAndWaitUntilFinishSubscription)
import Halogen.Store.Connect (connect)
import Halogen.Store.Monad (class MonadStore, updateStore)
import Halogen.Store.Select (selectEq)
import Halogen.Subscription as HS
import Store as Store
import Toast.Lenses (_expanded, _timeoutSubscription)
import Toast.Types (Action(..), State, ToastMessage)
import Toast.View (renderToast)

component
  :: forall query input msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => H.Component query input msg m
component =
  connect (selectEq _.toast) $ H.mkComponent
    { initialState: deriveState <<< _.context
    , render: renderToast
    , eval: H.mkEval H.defaultEval
        { handleAction = \action -> clearSubscription *> handleAction action
        , receive = Just <<< Receive <<< _.context
        }
    }

deriveState :: Maybe ToastMessage -> State
deriveState mToast =
  { mToast: mToast <#> { message: _, expanded: false }
  , timeoutSubscription: Nothing
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
  => MonadStore Store.Action Store.Store m
  => Action
  -> HalogenM State Action slots msg m Unit
handleAction (Receive mToast) = do
  H.put $ deriveState mToast
  sub <- traverse (subscribe <<< toastTimeoutSubscription) mToast
  H.modify_ _ { timeoutSubscription = sub }

handleAction ExpandToast = do
  assign _expanded true

handleAction CloseToast = updateStore Store.ClearToast

handleAction ToastTimeout = do
  mElement <- getHTMLElementRef (RefLabel "collapsed-toast")
  for_ mElement $ subscribe <<< animateAndWaitUntilFinishSubscription
    "to-bottom"
    CloseToast

clearSubscription
  :: forall m slots msg
   . HalogenM State Action slots msg m Unit
clearSubscription = do
  mSubscriptionId <- peruse _timeoutSubscription
  for_ mSubscriptionId unsubscribe
  H.modify_ _ { timeoutSubscription = Nothing }
