module Halogen.Form.Input
  ( Action
  , ComponentHTML
  , Input
  , Msg(..)
  , Query(..)
  , Render
  , Slot
  , State
  , component
  , defaultHandlers
  , renderInSlot
  , setInputProps
  ) where

import Prologue

import Component.Form as HF
import Control.Alt ((<|>))
import Control.Alternative (guard)
import Data.Either (either, hush)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe, maybe, maybe')
import Data.Symbol (class IsSymbol)
import Effect.Class (class MonadEffect)
import Halogen (RefLabel(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prim.Row as Row
import Type.Proxy (Proxy)
import Web.Event.Event (Event)
import Web.HTML.HTMLElement as HTMLElement
import Web.UIEvent.FocusEvent (FocusEvent)

data Query a
  = SetValue String a
  | Focus a
  | Blur a

derive instance Functor Query

type Render pa error output slots m =
  State error output -> ComponentHTML pa error output slots m

type Input pa error output slots m =
  { load :: Maybe output
  , format :: output -> String
  , render :: Render pa error output slots m
  , validate :: String -> Either error output
  }

data Msg pa output
  = OutputUpdated (Maybe output)
  | ValueUpdated String
  | Blurred
  | Focused
  | Emit pa

data Action pa error output slots m
  = OnUpdate String
  | OnEmit pa
  | OnBlur
  | OnFocus
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
  , input :: Input pa error output slots m
  , result :: Either error output
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
   . MonadEffect m
  => Component pa error output slots m
component = H.mkComponent
  { initialState: computeState Nothing
      { focused: false, value: "", visited: false }
  , render: render'
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      , receive = Just <<< OnReceive
      }
  }

type InputOptions :: forall k1 k2 k3. k1 -> Type -> Type -> k2 -> k3 -> Type
type InputOptions pa error output slots m =
  { load :: Maybe output
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

renderInSlot
  :: forall label slot slots' slots pa error output childSlots m
   . MonadEffect m
  => Row.Cons label (Slot pa output slot) slots' slots
  => IsSymbol label
  => Ord slot
  => Proxy label
  -> slot
  -> { options :: InputOptions pa error output childSlots m
     , handlers :: Handlers (HF.Action pa output slots m)
     , render :: Render pa error output childSlots m
     }
  -> HF.ComponentHTML pa output slots m
renderInSlot label slot { options, handlers, render } =
  HH.slot label slot component { format, load, render, validate } case _ of
    OutputUpdated output -> HF.update output
    Emit pa -> HF.emit pa
    ValueUpdated value ->
      fromMaybe HF.void $ handlers.valueUpdated <*> pure value
    Blurred -> fromMaybe HF.void handlers.blurred
    Focused -> fromMaybe HF.void handlers.focused
  where
  { format, load, validate } = options

computeState
  :: forall pa error output slots m r
   . Maybe output
  -> { focused :: Boolean
     , value :: String
     , visited :: Boolean
     | r
     }
  -> Input pa error output slots m
  -> InternalState pa error output slots m
computeState result { focused, value, visited } input =
  { input
  , value: newValue
  , result: newResult
  , focused
  , visited
  }
  where
  newResult = maybe' (\_ -> input.validate value) Right input.load
  newValue = maybe value input.format
    $ guard (not focused) *> (result <|> hush newResult)

render'
  :: forall pa error output slots m
   . InternalState pa error output slots m
  -> ComponentHTML pa error output slots m
render' { result, input: { format, render }, value, focused, visited } =
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
  OnUpdate newValue -> setValue newValue
  OnEmit action -> H.raise $ Emit action
  OnFocus -> setFocus true
  OnBlur -> setFocus false
  OnReceive input -> H.modify_ \s -> computeState (hush s.result) s input
  where
  setFocus focused = do
    H.modify_ \s -> computeState (hush s.result)
      s
        { focused = focused
        , visited = (s.focused && not focused) || s.visited
        }
      s.input
    H.raise if focused then Focused else Blurred

setValue
  :: forall pa error output slots m. String -> DSL pa error output slots m Unit
setValue newValue = do
  oldValue <- H.gets _.value
  when (newValue /= oldValue) do
    { result } <- H.modify \s -> s
      { result = s.input.validate newValue, value = newValue }
    H.raise $ OutputUpdated $ hush result

handleQuery
  :: forall pa error output slots m a
   . MonadEffect m
  => Query a
  -> DSL pa error output slots m (Maybe a)
handleQuery = case _ of
  SetValue value a -> do
    setValue value
    pure $ Just a
  Focus a -> do
    element <- H.getHTMLElementRef refLabel
    H.liftEffect $ traverse_ HTMLElement.focus element
    pure $ Just a
  Blur a -> do
    element <- H.getHTMLElementRef refLabel
    H.liftEffect $ traverse_ HTMLElement.blur element
    pure $ Just a
