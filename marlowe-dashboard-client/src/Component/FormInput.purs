module Component.FormInput
  ( Action
  , ComponentHTML
  , Input
  , Msg(..)
  , Query(..)
  , Slot
  , State
  , component
  , setInputProps
  ) where

import Prologue

import Data.Foldable (traverse_)
import Effect.Class (class MonadEffect)
import Halogen (RefLabel(..))
import Halogen as H
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (Event)
import Web.HTML.HTMLElement as HTMLElement
import Web.UIEvent.FocusEvent (FocusEvent)

data Query a
  = SetPrisine Boolean a
  | Focus a
  | Blur a

type Input pa slots m =
  { value :: String
  , render :: State -> ComponentHTML pa slots m
  , format :: String -> String
  }

data Msg pa
  = Updated String
  | Focused
  | Blurred
  | Emit pa

instance Show (Msg pa) where
  show (Updated value) = "(Update " <> show value <> ")"
  show (Emit _) = "Emit"
  show Blurred = "Blurred"
  show Focused = "Focused"

data Action pa slots m
  = OnUpdate String
  | OnEmit pa
  | OnBlur
  | OnFocus
  | Init
  | Receive (Input pa slots m)

instance Show (Action pa slots m) where
  show (OnUpdate value) = "(Update " <> show value <> ")"
  show (OnEmit _) = "OnEmit"
  show OnBlur = "OnBlur"
  show OnFocus = "OnFocus"
  show Init = "Init"
  show (Receive input) = "(Receive " <> show input.value <> ")"

type ComponentHTML pa slots m = H.ComponentHTML (Action pa slots m) slots m

type Slot pa slot = H.Slot Query (Msg pa) slot

type DSL pa slots m a = H.HalogenM
  (InternalState pa slots m)
  (Action pa slots m)
  slots
  (Msg pa)
  m
  a

type State =
  { value :: String
  , pristine :: Boolean
  , focused :: Boolean
  , visited :: Boolean
  }

type InternalState pa slots m =
  { input :: Input pa slots m
  , pristine :: Boolean
  , focused :: Boolean
  , visited :: Boolean
  }

refLabel :: RefLabel
refLabel = RefLabel "Component.Input.input"

type Component pa slots m = H.Component Query (Input pa slots m) (Msg pa) m

type InputProps r =
  ( onInput :: Event
  , onFocus :: FocusEvent
  , onBlur :: FocusEvent
  , value :: String
  | r
  )

setInputProps
  :: forall pa slots m r
   . String
  -> Array (HP.IProp (InputProps r) (Action pa slots m))
  -> Array (HP.IProp (InputProps r) (Action pa slots m))
setInputProps value props = props <>
  [ HE.onValueInput OnUpdate
  , HE.onFocus $ const OnFocus
  , HE.onBlur $ const OnBlur
  , HP.value value
  , HP.ref refLabel
  ]

component :: forall pa slots m. MonadEffect m => Component pa slots m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      , initialize = Just Init
      , receive = Just <<< Receive
      }
  }

initialState :: forall pa slots m. Input pa slots m -> InternalState pa slots m
initialState input =
  { input: input { value = input.format input.value }
  , pristine: false
  , focused: false
  , visited: false
  }

render
  :: forall pa slots m. InternalState pa slots m -> ComponentHTML pa slots m
render { input, pristine, focused, visited } =
  input.render { value: input.value, pristine, focused, visited }

handleAction :: forall pa slots m. Action pa slots m -> DSL pa slots m Unit
handleAction = case _ of
  OnUpdate value ->
    handleInputChange true <<< _ { value = value } =<< H.gets _.input
  OnEmit action -> H.raise $ Emit action
  OnFocus -> do
    H.modify_ _ { focused = true }
    handleInputChange true =<< H.gets _.input
    H.raise Blurred
  OnBlur -> do
    H.modify_ _ { focused = false, visited = true }
    handleInputChange true =<< H.gets _.input
    H.raise Focused
  Init -> handleInputChange false =<< H.gets _.input
  Receive input -> handleInputChange false input

handleInputChange
  :: forall pa slots m. Boolean -> Input pa slots m -> DSL pa slots m Unit
handleInputChange raise input = do
  { input: { value }, focused } <- H.get
  let newValue = if focused then input.value else input.format input.value
  H.modify_ _ { input = input }
  when (raise && value /= newValue) do
    H.raise $ Updated newValue

handleQuery
  :: forall pa slots m a. MonadEffect m => Query a -> DSL pa slots m (Maybe a)
handleQuery = case _ of
  SetPrisine pristine a -> do
    H.modify_ _ { pristine = pristine }
    pure $ Just a
  Focus a -> do
    element <- H.getHTMLElementRef refLabel
    H.liftEffect $ traverse_ HTMLElement.focus element
    pure $ Just a
  Blur a -> do
    element <- H.getHTMLElementRef refLabel
    H.liftEffect $ traverse_ HTMLElement.blur element
    pure $ Just a
