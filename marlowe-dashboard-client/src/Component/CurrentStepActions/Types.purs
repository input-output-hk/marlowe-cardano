module Component.CurrentStepActions.Types
  ( Action(..)
  , ComponentHTML
  , DSL
  , Input
  , Msg(..)
  , Query
  , Slot
  , State
  , _currentStepActions
  ) where

import Prologue

import Data.ContractUserParties (ContractUserParties)
import Data.UserNamedActions (UserNamedActions)
import Halogen as H
import Marlowe.Execution.Types (NamedAction)
import Marlowe.Execution.Types as Execution
import Marlowe.Semantics (ChoiceId, ChosenNum)
import Type.Proxy (Proxy(..))

data Msg = ActionSelected NamedAction

data Action
  = SelectAction NamedAction
  | ChangeChoice ChoiceId (Maybe ChosenNum)

type State =
  { executionState :: Execution.State
  , contractUserParties :: ContractUserParties
  , namedActions :: UserNamedActions
  }

type Input = State

type ComponentHTML m =
  H.ComponentHTML Action () m

type DSL m a =
  H.HalogenM State Action () Msg m a

data Query (a :: Type)

type Slot m = H.Slot Query Msg m

_currentStepActions = Proxy :: Proxy "currentStepActions"
