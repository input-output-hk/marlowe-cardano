module Halogen.Form.Input
  ( Action
  , ComponentHTML
  , FieldState
  , Input
  , Msg(..)
  , Query(..)
  , Render
  , Slot
  , State
  , component
  , setInputProps
  ) where

import Prologue

import Control.Alternative (guard)
import Data.Either (either, hush, isRight)
import Data.Foldable (traverse_)
import Data.Generic.Rep (class Generic)
import Data.Maybe (maybe)
import Data.Show.Generic (genericShow)
import Effect.Class (class MonadEffect)
import Halogen (RefLabel(..))
import Halogen as H
import Halogen.Form.FieldState as FS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (Event)
import Web.HTML.HTMLElement as HTMLElement
import Web.UIEvent.FocusEvent (FocusEvent)

type FieldState = FS.FieldState String

data Query a
  = Focus a
  | Blur a

derive instance Functor Query

type Render pa error output slots m =
  State error output -> ComponentHTML pa error output slots m

type Input pa error output slots m =
  { fieldState :: FieldState output
  , format :: output -> String
  , validate :: String -> Either error output
  , render :: Render pa error output slots m
  }

data Msg pa output
  = Updated (FieldState output)
  | Blurred
  | Focused
  | Emit pa

derive instance Generic (Msg pa output) _
derive instance Functor (Msg pa)
instance (Show output, Show pa) => Show (Msg pa output) where
  show = genericShow

data Action pa error output slots m
  = OnUpdate String
  | OnEmit pa
  | OnBlur
  | OnFocus
  | OnInit
  | OnReceive (Input pa error output slots m)

type ComponentHTML pa error output slots m =
  H.ComponentHTML (Action pa error output slots m) slots m

type Slot pa output = H.Slot Query (Msg pa output)

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
  H.Component Query (Input pa error output slots m) (Msg pa output) m

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
   . Eq output
  => MonadEffect m
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

initialState
  :: forall pa error output slots m r
   . { focused :: Boolean
     , visited :: Boolean
     | r
     }
  -> Input pa error output slots m
  -> InternalState pa error output slots m
initialState { focused, visited } { fieldState, format, validate, render } =
  { format
  , validate
  , render
  , value
  , result
  , focused
  , visited
  }
  where
  { result, value } = case fieldState of
    FS.Blank -> { value: "", result: validate "" }
    FS.Incomplete input -> { value: input, result: validate input }
    FS.Complete output -> { value: format output, result: Right output }

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

setValue
  :: forall pa error output slots m
   . Eq output
  => (String -> String)
  -> DSL pa error output slots m Unit
setValue f = do
  { value: oldValue, result: oldResult } <- H.get
  let newValue = f oldValue
  { value, result } <- H.modify \s ->
    let
      newResult = s.validate newValue
    in
      s
        { result = newResult
        , value =
            if s.focused then newValue
            else case newResult of
              Right output -> s.format output
              _ -> newValue
        , visited = s.visited || isRight newResult
        }
  let
    toFieldState v r = case r of
      Left _ -> FS.Incomplete v
      Right output -> FS.Complete output
    oldState = toFieldState oldValue oldResult
    newState = toFieldState value result

  when (newState /= oldState || oldValue /= value) do
    H.raise $ Updated newState

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
   . Eq output
  => Action pa error output slots m
  -> DSL pa error output slots m Unit
handleAction = case _ of
  OnInit -> setValue identity
  OnReceive { fieldState, render, format, validate } -> do
    H.modify_ $ \s -> s
      { render = render
      , format = format
      , validate = validate
      }
    setValue $ \oldValue -> case fieldState of
      FS.Blank -> ""
      FS.Incomplete v -> v
      _ -> oldValue
  OnUpdate newValue -> setValue $ const newValue
  OnEmit action -> H.raise $ Emit action
  OnFocus -> setFocus true
  OnBlur -> setFocus false
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
      setValue identity

handleQuery
  :: forall pa error output slots m a
   . MonadEffect m
  => Query a
  -> DSL pa error output slots m (Maybe a)
handleQuery = case _ of
  Focus a -> do
    element <- H.getHTMLElementRef refLabel
    H.liftEffect $ traverse_ HTMLElement.focus element
    pure $ Just a
  Blur a -> do
    element <- H.getHTMLElementRef refLabel
    H.liftEffect $ traverse_ HTMLElement.blur element
    pure $ Just a
