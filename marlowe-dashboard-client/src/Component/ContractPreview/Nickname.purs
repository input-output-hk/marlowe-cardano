module Component.ContractPreview.Nickname
  ( Component
  , Input
  , Msg(..)
  , Placeholder
  , Query
  , Slot
  , component
  ) where

import Prologue

import Control.Alternative (guard)
import Control.Monad.Maybe.Extra (hoistMaybe)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.ContractNickname (ContractNickname)
import Data.ContractNickname as CN
import Data.Either (either)
import Data.Foldable (for_, traverse_)
import Data.Lens (_Just, assign, to, use, (^.), (^?))
import Data.Lens.AffineTraversal (AffineTraversal')
import Data.Lens.Extra (peruse)
import Data.Lens.Record (prop)
import Data.Maybe (isJust, maybe)
import Data.These (These(..))
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.Component.Reactive (_transient)
import Halogen.Component.Reactive as Reactive
import Halogen.Css (classNames)
import Halogen.Form.FieldState (FieldState(..), _Complete)
import Halogen.Form.Input as Input
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Events.Extra (onClick_)
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA (role)
import Halogen.Query.Event as HQ
import Halogen.Subscription as HS
import Halogen.Subscription.Extra (reactimate)
import Type.Proxy (Proxy(..))
import Web.Event.Event (preventDefault)
import Web.HTML.HTMLInputElement (toEventTarget)
import Web.UIEvent.KeyboardEvent (fromEvent, key, toEvent)
import Web.UIEvent.KeyboardEvent.EventTypes (keydown)

type Placeholder = String

type Input = Maybe ContractNickname

type Msg = ContractNickname

data Query (a :: Type)

type Component m = H.Component Query Input Msg m

type Slot = H.Slot Query Msg

component :: forall m. MonadEffect m => Component m
component = Reactive.mkReactiveComponent
  { deriveState: const unit
  , initialTransient: Nothing
  , render
  , eval: Reactive.fromHandleActionAndInput
      { handleAction
      , handleInput
      }
  }

type Transient = Maybe EditingState

type EditingState =
  { field :: Input.FieldState ContractNickname
  , escKeySubId :: Maybe H.SubscriptionId
  , enterKeySubId :: Maybe H.SubscriptionId
  }

_editingState :: AffineTraversal' State EditingState
_editingState = _transient <<< _Just

_field :: AffineTraversal' State (Input.FieldState ContractNickname)
_field = _editingState <<< prop (Proxy :: _ "field")

_escKeySubId :: AffineTraversal' State (Maybe H.SubscriptionId)
_escKeySubId = _editingState <<< prop (Proxy :: _ "escKeySubId")

_enterKeySubId :: AffineTraversal' State (Maybe H.SubscriptionId)
_enterKeySubId = _editingState <<< prop (Proxy :: _ "enterKeySubId")

type State = Reactive.State Input Unit Transient

data Action
  = OnEditNickname
  | OnInputMsg (Input.Msg Action ContractNickname)
  | OnSave ContractNickname
  | OnCancel
  | OnEnter

type Slots =
  ( input :: Input.Slot Action Msg Unit
  )

_input = Proxy :: Proxy "input"

type ComponentHTML m = H.ComponentHTML Action Slots m

type ComponentM = H.HalogenM State Action Slots Msg

render :: forall m. MonadEffect m => State -> ComponentHTML m
render state = HH.div [ classNames containerClasses ]
  [ HH.div [ classNames animationClasses ]
      [ case editingState of
          Nothing -> renderNotEditing $ state ^. Reactive._input
          Just { field } -> renderEditing field
      ]
  ]
  where
  editingState = state ^? _editingState
  containerClasses = baseContainerClasses <> stateContainerClasses
  baseContainerClasses =
    [ "border"
    , "rounded-xs"
    , "py-1"
    ]
  stateContainerClasses = case editingState of
    Nothing -> [ "border-transparent" ]
    Just _ -> [ "border-gray-500" ]
  animationClasses =
    [ "transition-all"
    , "duration-150"
    , "focus-within:px-1"
    , "hover:px-1"
    ]

renderNotEditing :: forall m. Maybe ContractNickname -> ComponentHTML m
renderNotEditing mNickname =
  HH.div
    [ classNames
        [ "cursor-pointer"
        , "flex"
        , "gap-1"
        , "items-center"
        , "nickname-input"
        ]
    , HP.tabIndex 0
    , role "button"
    , onClick_ OnEditNickname
    , HE.onFocus $ const OnEditNickname
    ]
    case mNickname of
      Nothing ->
        [ HH.span [ classNames [ "text-gray-900", "italic" ] ]
            [ HH.text "Click to add nickname" ]
        ]
      Just nickname ->
        [ HH.span [] [ HH.text $ CN.toString nickname ]
        -- TODO move Icon to this repo
        , HH.span
            [ classNames
                [ "text-gray-900"
                , "text-sm"
                , "material-icons-round"
                , "transform"
                , "-translate-x-2"
                , "opacity-0"
                , "transition"
                , "duration-150"
                ]
            ]
            [ HH.text "edit" ]
        ]

renderEditing
  :: forall m
   . MonadEffect m
  => Input.FieldState ContractNickname
  -> ComponentHTML m
renderEditing fieldState =
  HH.slot _input unit Input.component inputProps OnInputMsg
  where
  inputProps =
    { fieldState
    , format: CN.toString
    , validate: either This That <<< CN.fromString
    , render: \state -> HH.input
        ( Input.setInputProps state.value
            [ classNames [ "outline-none" ]
            , HP.placeholder "Enter a contract nickname"
            ]
        )
    }

handleAction :: forall m. Action -> ComponentM m Unit
handleAction = case _ of
  OnEditNickname -> unlessM (use (_transient <<< to isJust)) do
    mNickname <- use Reactive._input
    assign _transient $ Just $
      { field: maybe Blank Complete mNickname
      , escKeySubId: Nothing
      , enterKeySubId: Nothing
      }
    H.tell _input unit Input.Focus
    mInputElement <- H.request _input unit Input.GetElement
    for_ mInputElement \inputElement -> do
      let inputEventTarget = toEventTarget inputElement
      let
        keyPressE = HQ.eventListener keydown inputEventTarget fromEvent
        filterKey whichKey keyboardEvent = whichKey == key keyboardEvent
        preventDefaultOn keyboardEvent =
          liftEffect $ preventDefault $ toEvent keyboardEvent
      let
        escPressE = HS.filter (filterKey "Escape") keyPressE
        enterPressE = HS.filter (filterKey "Enter") keyPressE
        _ = reactimate preventDefaultOn escPressE
        _ = reactimate preventDefaultOn enterPressE
      assign _escKeySubId <<< Just =<< H.subscribe (OnCancel <$ escPressE)
      assign _enterKeySubId <<< Just =<< H.subscribe (OnEnter <$ enterPressE)
  OnSave nickname -> save nickname
  OnCancel -> cancel
  OnEnter -> whenValid save
  OnInputMsg (Input.Updated fieldState) -> assign _field fieldState
  OnInputMsg Input.Blurred -> whenValid save
  OnInputMsg Input.Focused -> pure unit
  OnInputMsg (Input.Emit action) -> handleAction action

whenValid
  :: forall m. (ContractNickname -> ComponentM m Unit) -> ComponentM m Unit
whenValid f = traverse_ f =<< peruse (_field <<< _Complete)

save :: forall m. ContractNickname -> ComponentM m Unit
save nickname = do
  H.raise nickname
  cancel

cancel :: forall m. ComponentM m Unit
cancel = do
  escKeySubId <-
    peruse (_transient <<< _Just <<< prop (Proxy :: _ "escKeySubId") <<< _Just)
  traverse_ H.unsubscribe escKeySubId
  assign _transient Nothing

handleInput :: forall m. Maybe State -> ComponentM m Unit
handleInput oldState = void $ runMaybeT do
  oldFieldState <- hoistMaybe $ oldState ^? _Just <<< _field
  let oldEditingValue = Input.toString CN.toString oldFieldState
  newEditingValue <- MaybeT $ use Reactive._input
  guard $ oldEditingValue /= (CN.toString newEditingValue)
  assign _field $ Complete newEditingValue
