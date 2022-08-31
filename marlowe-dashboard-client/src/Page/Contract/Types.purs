module Page.Contract.Types where

import Prologue

import Analytics (class IsEvent, defaultEvent)
import Component.CurrentStepActions.Types as CurrentStepActions
import Component.LoadingSubmitButton.Types as LoadingSubmitButton
import Component.Tooltip.Types (ReferenceId)
import Data.Array (length)
import Data.ContractStatus (ContractStatus, ContractStatusId)
import Data.DateTime.Instant (Instant)
import Data.Map (Map)
import Data.NewContract (NewContract)
import Data.Set (Set)
import Data.Time.Duration (Minutes)
import Data.UserNamedActions (UserNamedActions)
import Halogen (RefLabel(..))
import Halogen as H
import Halogen.Store.Connect (Connected)
import Language.Marlowe.Core.V1.Semantics.Types
  ( Accounts
  , ChosenNum
  , MarloweParams
  , Party
  , Payment
  , TransactionInput
  )
import Marlowe.Execution.Types (NamedAction)
import Marlowe.Execution.Types (State) as Execution
import Store.Contracts (ContractStore)
import Store.RoleTokens (RoleTokenStore)
import Type.Proxy (Proxy(..))

type ContractState = ContractStatus NewContract StartedState

type State =
  { contract :: ContractState
  , currentTime :: Instant
  , tzOffset :: Minutes
  , roleTokens :: RoleTokenStore
  }

type StartedState =
  { executionState :: Execution.State
  , previousSteps :: Array PreviousStep
  -- Which step is selected. This index is 0 based and should be between [0, previousSteps.length]
  -- (both sides inclusive). This is because the array represent the past steps and the
  -- executionState has the current state and visually we can select any one of them.
  -- TODO: fix primitive obsession - maybe a zipper is a better representation
  -- than an index + the execution state?
  , selectedStep :: Int
  -- These are the possible actions a user can make in the current step (grouped by party).
  , namedActions :: UserNamedActions
  , tabs :: Map Int Tab
  , expandPayments :: Map Int Boolean
  , participants :: Set Party
  }

type StepBalance =
  { atStart :: Accounts
  , atEnd :: Maybe Accounts
  }

-- Represents a historical step in a contract's life.
type PreviousStep =
  { resultingPayments :: Array Payment
  , balances :: StepBalance
  , state :: PreviousStepState
  }

type TimeoutInfo =
  { time :: Instant
  , missedActions :: UserNamedActions
  }

data PreviousStepState
  = TransactionStep TransactionInput
  | TimeoutStep TimeoutInfo

derive instance Eq PreviousStepState

data Tab
  = Tasks
  | Balances

derive instance eqTab :: Eq Tab

type Slice =
  { contracts :: ContractStore
  , currentTime :: Instant
  , roleTokens :: RoleTokenStore
  }

type Input =
  { contractIndex :: ContractStatusId
  }

data Msg = AskConfirmation NamedAction (Maybe ChosenNum)

data Action
  = Init
  | Finalize
  | Receive (Connected Slice Input)
  | SelectTab Int Tab
  | ToggleExpandPayment Int
  | OnActionSelected NamedAction (Maybe ChosenNum)
  -- The SelectStep action is what changes the model and causes the card to seem bigger.
  | SelectStep Int
  -- The MoveToStep action scrolls the step carousel so that the indicated step is at the center (without changing the model).
  | MoveToStep Int

type ChildSlots =
  ( submitButtonSlot :: H.Slot LoadingSubmitButton.Query Unit String
  , tooltipSlot :: forall query. H.Slot query Void ReferenceId
  , hintSlot :: forall query. H.Slot query Void String
  , currentStepActions :: CurrentStepActions.Slot MarloweParams
  )

type ComponentHTML m =
  H.ComponentHTML Action ChildSlots m

type DSL m a =
  H.HalogenM State Action ChildSlots Msg m a

data Query (a :: Type)

type Slot m = H.Slot Query Msg m

instance actionIsEvent :: IsEvent Action where
  toEvent Init = Nothing
  toEvent Finalize = Nothing
  toEvent (Receive _) = Nothing
  toEvent (SelectTab _ _) = Just $ defaultEvent "SelectTab"
  toEvent (ToggleExpandPayment _) = Just $ defaultEvent "ToggleExpandPayment"
  toEvent (OnActionSelected _ _) = Nothing
  toEvent (SelectStep _) = Just $ defaultEvent "SelectStep"
  toEvent (MoveToStep _) = Nothing

scrollContainerRef :: RefLabel
scrollContainerRef = RefLabel "scroll-container"

_contractPage = Proxy :: Proxy "contractPage"

currentStep :: StartedState -> Int
currentStep { previousSteps } = length previousSteps
