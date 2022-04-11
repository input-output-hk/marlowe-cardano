module Component.CurrentStepActions.Types where

import Prologue

import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.UserNamedActions (UserNamedActions)
import Halogen as H
import Halogen.Component.Reactive (_input, _transient)
import Halogen.Component.Reactive as Reactive
import Halogen.Store.Connect (Connected)
import Marlowe.Execution.Types (NamedAction)
import Marlowe.Execution.Types as Execution
import Marlowe.Semantics (ChoiceId, ChosenNum, CurrencySymbol)
import Marlowe.Semantics as Semantics
import Store.RoleTokens (RoleTokenStore)
import Type.Proxy (Proxy(..))

data Msg = ActionSelected NamedAction (Maybe ChosenNum)

data Action
  = SelectAction NamedAction (Maybe ChosenNum)
  | ChangeChoice ChoiceId (Maybe ChosenNum)

type State = Reactive.State Input Unit Transient

type Transient =
  { choiceValues :: Map ChoiceId ChosenNum
  }

type Input' =
  { executionState :: Execution.State
  , namedActions :: UserNamedActions
  }

type Input = Connected RoleTokenStore Input'

_choiceValues :: Lens' State (Map ChoiceId ChosenNum)
_choiceValues = _transient <<< prop (Proxy :: _ "choiceValues")

_executionState :: Lens' State Execution.State
_executionState = _input
  <<< prop (Proxy :: _ "input")
  <<< prop (Proxy :: _ "executionState")

_roleTokens :: Lens' State RoleTokenStore
_roleTokens = _input <<< prop (Proxy :: _ "context")

_rolesCurrency :: Lens' State CurrencySymbol
_rolesCurrency = _executionState
  <<< prop (Proxy :: _ "marloweParams")
  <<< Semantics._rolesCurrency

_namedActions :: Lens' State UserNamedActions
_namedActions = _input
  <<< prop (Proxy :: _ "input")
  <<< prop (Proxy :: _ "namedActions")

type ComponentHTML m =
  H.ComponentHTML Action () m

type DSL m a =
  H.HalogenM State Action () Msg m a

data Query (a :: Type)

type Slot m = H.Slot Query Msg m

_currentStepActions = Proxy :: Proxy "currentStepActions"
