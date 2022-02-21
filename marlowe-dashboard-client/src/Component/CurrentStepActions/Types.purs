module Component.CurrentStepActions.Types
  ( Action(..)
  , ComponentHTML
  , DSL
  , Input
  , State
  , _currentStepActions
  ) where

import Prologue

import Data.ContractUserParties (ContractUserParties)
import Halogen as H
import Marlowe.Execution.Types (NamedAction)
import Marlowe.Execution.Types as Execution
import Marlowe.Semantics (ChoiceId, ChosenNum, Party)
import Type.Proxy (Proxy(..))

-- type ChildSlots =
--   ( submitButtonSlot :: H.Slot LoadingSubmitButton.Query Unit String
--   , tooltipSlot :: forall query. H.Slot query Void ReferenceId
--   , hintSlot :: forall query. H.Slot query Void String
--   )

type ComponentHTML m =
  H.ComponentHTML Action () m

type DSL m a =
  H.HalogenM State Action () Void m a

data Action
  = AskConfirmation NamedAction
  | ChangeChoice ChoiceId (Maybe ChosenNum)

type State =
  { executionState :: Execution.State
  , contractUserParties :: ContractUserParties
  -- Same as Page.Contract.Types Named actions. TODO: Create a custom data type
  , namedActions :: Array (Tuple Party (Array NamedAction))
  }

type Input = State

_currentStepActions = Proxy :: Proxy "currentStepActions"
