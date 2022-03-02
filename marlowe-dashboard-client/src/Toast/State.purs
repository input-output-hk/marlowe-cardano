module Toast.State (component) where

import Prologue

import Data.Foldable (for_)
import Data.Lens (assign)
import Data.Lens.Extra (peruse)
import Data.Maybe (fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
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
        { handleAction = handleAction
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
    cancelled <- Ref.new false
    Aff.launchAff_ do
      Aff.delay $ Milliseconds toast.timeout
      unlessM (liftEffect $ Ref.read cancelled) do
        liftEffect $ push ToastTimeout
    pure do
      Ref.write true cancelled

handleAction
  :: forall m slots msg
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Action
  -> HalogenM State Action slots msg m Unit
handleAction (Receive mToast) = do
  unlessM (fromMaybe false <$> peruse _expanded) do
    H.put $ deriveState mToast
    mSubscriptionId <- peruse _timeoutSubscription
    for_ mSubscriptionId unsubscribe
    sub <- traverse (subscribe <<< toastTimeoutSubscription) mToast
    H.modify_ _ { timeoutSubscription = sub }

handleAction ExpandToast = do
  assign _expanded true
  mSubscriptionId <- peruse _timeoutSubscription
  for_ mSubscriptionId unsubscribe

handleAction CloseToast = do
  assign _expanded false
  updateStore Store.ClearToast

handleAction ToastTimeout = do
  mElement <- getHTMLElementRef (RefLabel "collapsed-toast")
  for_ mElement $ subscribe <<< animateAndWaitUntilFinishSubscription
    "to-bottom"
    CloseToast
