module Halogen.Form.Input
  ( Action
  , ComponentHTML
  , FieldState
  , InitializeField
  , Input
  , Msg(..)
  , Query(..)
  , Render
  , Slot
  , State
  , component
  , defaultHandlers
  , setInputProps
  ) where

import Prologue

import Control.Alternative (guard)
import Data.Bifunctor (class Bifunctor)
import Data.Either (either, hush)
import Data.Foldable (traverse_)
import Data.Maybe (maybe)
import Effect.Class (class MonadEffect)
import Halogen (RefLabel(..))
import Halogen as H
import Halogen.Form.Types as HF
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (Event)
import Web.HTML.HTMLElement as HTMLElement
import Web.UIEvent.FocusEvent (FocusEvent)

type FieldState = HF.FieldState String
type InitializeField = HF.InitializeField String

data Query output a
  = Initialize (InitializeField output) a
  | Focus a
  | Blur a

derive instance Functor (Query output)

instance Bifunctor Query where
  bimap f g = case _ of
    Initialize initialize a -> Initialize (f <$> initialize) $ g a
    Focus a -> Focus $ g a
    Blur a -> Blur $ g a

type Render pa error output slots m =
  State error output -> ComponentHTML pa error output slots m

type Input pa error output slots m =
  { initialize :: InitializeField output
  , format :: output -> String
  , validate :: String -> Either error output
  , render :: Render pa error output slots m
  }

data Msg pa output
  = Updated (FieldState output)
  | Blurred
  | Focused
  | Emit pa

data Action pa error output slots m
  = OnUpdate String
  | OnEmit pa
  | OnBlur
  | OnFocus
  | OnInit
  | OnReceive (Input pa error output slots m)

type ComponentHTML pa error output slots m =
  H.ComponentHTML (Action pa error output slots m) slots m

type Slot pa output = H.Slot (Query output) (Msg pa output)

type DSL pa error output slots m a = H.HalogenM
  (InternalState pa error output slots m)
  (Action pa error output slots m)
  slots
  (Msg pa output)
  m
  a

type State error output =
  { error :: Maybe error
  , focused :: Boolean
  , result :: Maybe output
  , value :: String
  }

type InternalState pa error output slots m =
  { focused :: Boolean
  , format :: output -> String
  , render :: Render pa error output slots m
  , result :: Either error output
  , validate :: String -> Either error output
  , value :: String
  , visited :: Boolean
  }

refLabel :: RefLabel
refLabel = RefLabel "Component.Input.input"

type Component pa error output slots m =
  H.Component (Query output) (Input pa error output slots m) (Msg pa output) m

type InputProps r =
  ( onInput :: Event
  , onFocus :: FocusEvent
  , onBlur :: FocusEvent
  , value :: String
  | r
  )

setInputProps
  :: forall pa slots m error output r
   . String
  -> Array (HP.IProp (InputProps r) (Action pa error output slots m))
  -> Array (HP.IProp (InputProps r) (Action pa error output slots m))
setInputProps value props = props <>
  [ HE.onValueInput OnUpdate
  , HE.onFocus $ const OnFocus
  , HE.onBlur $ const OnBlur
  , HP.value value
  , HP.ref refLabel
  ]

component
  :: forall pa error output slots m
   . MonadEffect m
  => Component pa error output slots m
component = H.mkComponent
  { initialState: initialState { focused: false, visited: false }
  , render: render'
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      , receive = Just <<< OnReceive
      , initialize = Just OnInit
      }
  }

type InputOptions :: forall k1 k2 k3. k1 -> Type -> Type -> k2 -> k3 -> Type
type InputOptions pa error output slots m =
  { initialize :: InitializeField output
  , format :: output -> String
  , validate :: String -> Either error output
  }

type Handlers action =
  { blurred :: Maybe action
  , focused :: Maybe action
  , valueUpdated :: Maybe (String -> action)
  }

defaultHandlers :: forall action. Handlers action
defaultHandlers =
  { blurred: Nothing
  , focused: Nothing
  , valueUpdated: Nothing
  }

initialState
  :: forall pa error output slots m r
   . { focused :: Boolean
     , visited :: Boolean
     | r
     }
  -> Input pa error output slots m
  -> InternalState pa error output slots m
initialState
  { focused, visited }
  { initialize, format, validate, render } =
  { format
  , validate
  , render
  , value: newValue
  , result: newResult
  , focused
  , visited
  }
  where
  initialize' = case _ of
    HF.FromBlank -> initialize' $ HF.FromInput ""
    HF.FromInput input -> { newValue: input, newResult: validate input }
    HF.FromOutput output -> { newValue: format output, newResult: Right output }
  { newResult, newValue } = initialize' initialize

modifyWithFormat
  :: forall pa error output slots m
   . ( InternalState pa error output slots m
       -> InternalState pa error output slots m
     )
  -> InternalState pa error output slots m
  -> InternalState pa error output slots m
modifyWithFormat f = formatIfNotFocuesd <<< f
  where
  formatIfNotFocuesd state
    | state.focused = state
    | otherwise = case state.result of
        Right result -> state { value = state.format result }
        _ -> state

render'
  :: forall pa error output slots m
   . InternalState pa error output slots m
  -> ComponentHTML pa error output slots m
render' { result, format, render, value, focused, visited } =
  render
    { error: guard visited *> either Just (const Nothing) result
    , focused
    , result: mOutput
    , value: maybe value format $ guard (not focused) *> mOutput
    }
  where
  mOutput = hush result

handleAction
  :: forall pa error output slots m
   . Action pa error output slots m
  -> DSL pa error output slots m Unit
handleAction = case _ of
  OnInit -> do
    { value, result } <- H.get
    H.raise $ Updated $ HF.FieldState value $ hush result
  OnUpdate newValue -> do
    oldValue <- H.gets _.value
    when (newValue /= oldValue) do
      { value, result } <- H.modify $ modifyWithFormat \s -> s
        { result = s.validate newValue, value = newValue }
      H.raise $ Updated $ HF.FieldState value $ hush result
  OnEmit action -> H.raise $ Emit action
  OnFocus -> setFocus true
  OnBlur -> setFocus false
  OnReceive { render, format, validate } -> H.modify_ $ modifyWithFormat _
    { render = render
    , format = format
    , validate = validate
    }
  where
  setFocus focused = do
    currentFocused <- H.gets _.focused
    when (focused /= currentFocused) do
      H.modify_ $ modifyWithFormat \s ->
        s
          { focused = focused
          , visited = (s.focused && not focused) || s.visited
          }
      H.raise if focused then Focused else Blurred

handleQuery
  :: forall pa error output slots m a
   . MonadEffect m
  => Query output a
  -> DSL pa error output slots m (Maybe a)
handleQuery = case _ of
  Initialize initialize a -> do
    H.modify_ \s@{ format, validate, render } ->
      initialState s { initialize, format, validate, render }
    pure $ Just a
  Focus a -> do
    element <- H.getHTMLElementRef refLabel
    H.liftEffect $ traverse_ HTMLElement.focus element
    pure $ Just a
  Blur a -> do
    element <- H.getHTMLElementRef refLabel
    H.liftEffect $ traverse_ HTMLElement.blur element
    pure $ Just a
