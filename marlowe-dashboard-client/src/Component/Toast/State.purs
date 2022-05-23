module Component.Toast.State (component) where

import Prologue

import Component.Toast.Lenses (_timeoutSubscriptions, _toasts)
import Component.Toast.Types
  ( Action(..)
  , State
  , ToastEntry
  , ToastIndex
  , ToastMessage
  , indexRef
  )
import Component.Toast.View (render)
import Control.Bind (bindFlipped)
import Control.Plus (empty)
import Data.Foldable (for_)
import Data.Lens (over, set, use)
import Data.Lens.Extra (peruse)
import Data.Lens.Index (ix)
import Data.List (List)
import Data.List as List
import Data.Map as Map
import Data.Maybe (maybe)
import Data.Traversable (for)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen (HalogenM, subscribe, unsubscribe)
import Halogen as H
import Halogen.Animation
  ( animateAndWaitUntilFinish
  , animateAndWaitUntilFinishSubscription
  )
import Halogen.Store.Connect (connect)
import Halogen.Store.Monad (class MonadStore, updateStore)
import Halogen.Store.Select (selectEq)
import Halogen.Subscription as HS
import Halogen.Subscription.Extra (delayEmitter)
import Record as Record
import Store (Action(..), Store) as Store
import Store.Toast (ToastAction(..)) as Store
import Store.Toast (getToasts)
import Type.Prelude (Proxy(..))
import Web.DOM (Element)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (HTMLElement, window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window (document)

component
  :: forall query input msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => H.Component query input msg m
component =
  connect (selectEq (getToasts <<< _.toast)) $ H.mkComponent
    { initialState: deriveState <<< _.context
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive <<< _.context
        }
    }

deriveState :: List ToastEntry -> State
deriveState toasts =
  { toasts
  , timeoutSubscriptions: Map.empty
  }

toastTimeoutEmitter :: ToastIndex -> ToastMessage -> HS.Emitter Action
toastTimeoutEmitter index toast = AnimateCloseToast index <$ maybe empty
  delayEmitter
  toast.timeout

handleAction
  :: forall m slots msg
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Action
  -> HalogenM State Action slots msg m Unit
handleAction (Receive receivedToasts) = do
  currentToasts <- use _toasts
  let
    -- We remove the field so that a toast expansion doesn't result in creating a new timer
    removeExpanded xs = Record.delete (Proxy :: Proxy "expanded") <$> xs
    newToasts = List.difference (removeExpanded receivedToasts)
      (removeExpanded currentToasts)

  -- Subscribe for a timer event for each new toast
  newSubs <- for newToasts \entry -> do
    subsId <- subscribe $ toastTimeoutEmitter entry.index entry.message
    pure $ entry.index /\ subsId

  H.modify_ $
    set _toasts receivedToasts
      <<< over _timeoutSubscriptions (Map.union $ Map.fromFoldable newSubs)

  -- We make the animation from the handler instead of the css directly so that if
  -- the elment Animate
  for_ newToasts \entry -> do
    mElement <- getElementById' (indexRef "toast-message" entry.index)
    for_ mElement \elem -> liftAff $ animateAndWaitUntilFinish "from-below" elem

handleAction (ToggleExpanded index) = do
  -- When we expand or collapse a toast we take a manual action that we
  -- are "reading/taking care" of the message. So:
  -- If there is an active subscription to close the toast, cancel it
  -- and remove it from the map
  mSubscription <- peruse (_timeoutSubscriptions <<< ix index)
  for_ mSubscription unsubscribe
  H.modify_ $
    over _timeoutSubscriptions (Map.delete index)
  -- Update the global state (which will call Receive)
  updateStore $ Store.Toast $ Store.ToggleExpanded index

handleAction (CloseToast index) = do

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
  mElement <- getElementById' (indexRef "toast-message" index)

  for_ mElement \elm -> void $ subscribe $ animateAndWaitUntilFinishSubscription
    "to-right"
    (CloseToast index)
    elm

getElementById' :: forall m. MonadEffect m => String -> m (Maybe HTMLElement)
getElementById' idStr = liftEffect do
  doc <- document =<< window

  let
    toHTMLElement :: Maybe Element -> Maybe HTMLElement
    toHTMLElement = bindFlipped HTMLElement.fromElement

  toHTMLElement <$> (getElementById idStr $ toNonElementParentNode doc)
