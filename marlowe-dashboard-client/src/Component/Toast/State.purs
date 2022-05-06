module Component.Toast.State (component) where

import Prologue

import Component.Toast.Lenses (_expanded, _timeoutSubscriptions, _toasts)
import Component.Toast.Types
  ( Action(..)
  , State
  , ToastEntry
  , ToastMessage
  , indexRef
  )
import Component.Toast.View (render)
import Data.Foldable (for_)
import Data.Lens (assign, modifying, over, set, use)
import Data.Lens.Extra (peruse)
import Data.Lens.Index (ix)
import Data.List as List
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for, traverse)
import Data.Tuple.Nested ((/\))
import Debug (traceM)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff, liftAff)
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
import Halogen.Animation
  ( animateAndWaitUntilFinish
  , animateAndWaitUntilFinishSubscription
  )
import Halogen.Store.Connect (connect)
import Halogen.Store.Monad (class MonadStore, updateStore)
import Halogen.Store.Select (selectEq)
import Halogen.Subscription as HS
import Store as Store
import Store.Toast (ToastStore, getToasts)
import Store.Toast as Store

component
  :: forall query input msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => H.Component query input msg m
component =
  connect (selectEq _.toast) $ H.mkComponent
    { initialState: deriveState <<< _.context
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive <<< getToasts <<< _.context
        }
    }

deriveState :: ToastStore -> State
deriveState store =
  { toasts: getToasts store
  , timeoutSubscriptions: Map.empty
  }

toastTimeoutSubscription :: ToastEntry -> HS.Emitter Action
toastTimeoutSubscription { index, message: toast } =
  HS.makeEmitter \push -> do
    traceM { msg: "TOAST: Subscribed", index }

    cancelled <- Ref.new false
    Aff.launchAff_ do
      Aff.delay $ Milliseconds toast.timeout
      traceM { msg: "TOAST: Timer is up", index }

      unlessM (liftEffect $ Ref.read cancelled) do
        traceM { msg: "TOAST: Toast timeout", index }

        liftEffect $ push $ AnimateCloseToast index
    pure do
      Ref.write true cancelled

handleAction
  :: forall m slots msg
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Action
  -> HalogenM State Action slots msg m Unit
handleAction (Receive receivedToasts) = do
  traceM { msg: "TOAST: receive", receivedToasts }
  currentToasts <- use _toasts
  let
    newToasts = List.difference receivedToasts currentToasts

  -- Subscribe for a timer event for each new toast
  newSubs <- for newToasts \entry -> do
    subsId <- subscribe $ toastTimeoutSubscription entry
    pure $ entry.index /\ subsId
  traceM { msg: "TOAST: new subs", newSubs }

  H.modify_ $
    set _toasts receivedToasts
      <<< over _timeoutSubscriptions (Map.union $ Map.fromFoldable newSubs)

  -- We make the animation from the handler instead of the css directly so that if
  -- the elment Animate
  for_ newToasts \entry -> do
    mElement <- getHTMLElementRef
      (RefLabel $ indexRef "toast-message" entry.index)
    for_ mElement \elem -> liftAff $ animateAndWaitUntilFinish "from-below" elem

handleAction (ExpandToast index) = do
  -- If there is an active subscription to close the toast, cancel it
  -- and remove it from the map
  mSubscription <- peruse (_timeoutSubscriptions <<< ix index)
  for_ mSubscription unsubscribe
  H.modify_ $
    over _timeoutSubscriptions (Map.delete index)
  -- Update the global state (which will call Receive)
  updateStore $ Store.Toast $ Store.Expand index

handleAction (CloseToast index) = do
  traceM { msg: "TOAST: close toast", index }

  -- If there is an active subscription to close the toast, cancel it
  -- and remove it from the map
  mSubscription <- peruse (_timeoutSubscriptions <<< ix index)
  for_ mSubscription unsubscribe
  H.modify_ $
    over _timeoutSubscriptions (Map.delete index)

  -- Remove the toast from the global store (This will cause a Receive that updates
  -- the local state)
  updateStore $ Store.Toast $ Store.Clear index

handleAction (AnimateCloseToast index) = do
  mElement <- getHTMLElementRef (RefLabel $ indexRef "toast-message" index)
  traceM
    { msg: "TOAST: Animate close toast"
    , index
    , mElement
    , ref: (indexRef "toast-message" index)
    }
  case mElement of
    Nothing -> traceM $ "Toast not found " <> indexRef "toast-message" index
    Just elm -> void $ subscribe $ animateAndWaitUntilFinishSubscription
      "to-right"
      (CloseToast index)
      elm
