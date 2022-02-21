module Page.Contract.Types
  ( Action(..)
  , Input
  , PreviousStep
  , PreviousStepState(..)
  , Context
  , StartedState
  , StartingState
  , State
  , ContractState(..)
  , StepBalance
  , Tab(..)
  , TimeoutInfo
  , _contractPage
  , scrollContainerRef
  ) where

import Prologue

import Analytics (class IsEvent, defaultEvent)
import Data.ContractNickname (ContractNickname)
import Data.ContractUserParties (ContractUserParties)
import Data.PABConnectedWallet (PABConnectedWallet)
import Data.Time.Duration (Minutes)
import Halogen (RefLabel(..))
import Halogen.Store.Connect (Connected)
import Marlowe.Execution.Types (NamedAction)
import Marlowe.Execution.Types (State) as Execution
import Marlowe.Extended.Metadata (MetaData)
import Marlowe.Semantics
  ( Accounts
  , ChoiceId
  , ChosenNum
  , MarloweParams
  , Party
  , Payment
  , Slot
  , TransactionInput
  )
import Store.Contracts (ContractStore)
import Type.Proxy (Proxy(..))

data ContractState
  = Starting StartingState
  | Started StartedState

derive instance Eq ContractState

type State =
  { contract :: ContractState
  , currentSlot :: Slot
  , tzOffset :: Minutes
  , wallet :: PABConnectedWallet
  }

type StartingState =
  { nickname :: ContractNickname
  , metadata :: MetaData
  }

type StartedState =
  { tab :: Tab -- this is the tab of the current (latest) step - previous steps have their own tabs
  , executionState :: Execution.State
  , previousSteps :: Array PreviousStep
  -- Which step is selected. This index is 0 based and should be between [0, previousSteps.length]
  -- (both sides inclusive). This is because the array represent the past steps and the
  -- executionState has the current state and visually we can select any one of them.
  -- TODO: fix primitive obsession - maybe a zipper is a better representation
  -- than an index + the execution state?
  , selectedStep :: Int
  -- How the "logged-in" user sees the different Parties of the contract
  , contractUserParties :: ContractUserParties
  -- These are the possible actions a user can make in the current step (grouped by part). We store this
  -- mainly because extractNamedActions and expandAndGroupByRole could potentially be unperformant to compute
  -- for every render.
  , namedActions :: Array (Tuple Party (Array NamedAction))
  }

type StepBalance =
  { atStart :: Accounts
  , atEnd :: Maybe Accounts
  }

-- Represents a historical step in a contract's life.
type PreviousStep =
  { tab :: Tab
  , expandPayments :: Boolean
  , resultingPayments :: Array Payment
  , balances :: StepBalance
  , state :: PreviousStepState
  }

type TimeoutInfo =
  { slot :: Slot
  , missedActions :: Array (Tuple Party (Array NamedAction))
  }

data PreviousStepState
  = TransactionStep TransactionInput
  | TimeoutStep TimeoutInfo

derive instance Eq PreviousStepState

data Tab
  = Tasks
  | Balances

derive instance eqTab :: Eq Tab

type Context =
  { currentSlot :: Slot
  , contracts :: ContractStore
  }

type Input =
  { tzOffset :: Minutes
  , wallet :: PABConnectedWallet
  -- FIXME-3208 Instead of just MarloweParms this should be a custom data type or a
  --            Either UUID MarloweParams to be able to work with Starting and Started contracts.
  , marloweParams :: MarloweParams
  }

data Action
  = Receive (Connected Context Input)
  | SetNickname ContractNickname
  | ConfirmAction NamedAction
  | ChangeChoice ChoiceId (Maybe ChosenNum)
  | SelectTab Int Tab
  | ToggleExpandPayment Int
  | AskConfirmation NamedAction
  | CancelConfirmation
  -- The SelectStep action is what changes the model and causes the card to seem bigger.
  -- TODO: refactor this stuff - why are there two actions?
  | SelectStep Int
  -- The MoveToStep action scrolls the step carousel so that the indicated step is at the center (without changing the model).
  | MoveToStep Int
  -- TODO: seems like all the carousel stuff shoule be moved to a component
  | CarouselOpened
  | CarouselClosed

instance actionIsEvent :: IsEvent Action where
  toEvent (Receive _) = Nothing
  toEvent (ConfirmAction _) = Just $ defaultEvent "ConfirmAction"
  toEvent (SetNickname _) = Just $ defaultEvent "SetNickname"
  toEvent (ChangeChoice _ _) = Just $ defaultEvent "ChangeChoice"
  toEvent (SelectTab _ _) = Just $ defaultEvent "SelectTab"
  toEvent (ToggleExpandPayment _) = Just $ defaultEvent "ToggleExpandPayment"
  toEvent (AskConfirmation _) = Just $ defaultEvent "AskConfirmation"
  toEvent CancelConfirmation = Just $ defaultEvent "CancelConfirmation"
  toEvent (SelectStep _) = Just $ defaultEvent "SelectStep"
  toEvent (MoveToStep _) = Nothing
  toEvent CarouselOpened = Just $ defaultEvent "CarouselOpened"
  toEvent CarouselClosed = Just $ defaultEvent "CarouselClosed"

scrollContainerRef :: RefLabel
scrollContainerRef = RefLabel "scroll-container"

_contractPage = Proxy :: Proxy "contractPage"
