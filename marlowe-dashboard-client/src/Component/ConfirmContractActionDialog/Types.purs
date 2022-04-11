module Component.ConfirmContractActionDialog.Types where

import Prologue

import Component.Expand as Expand
import Component.LoadingSubmitButton.Types as LoadingSubmitButton
import Data.BigInt.Argonaut (BigInt)
import Data.ContractUserParties (ContractUserParties)
import Data.DateTime.Instant (Instant)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.PABConnectedWallet (PABConnectedWallet)
import Effect.Aff (Fiber)
import Halogen as H
import Halogen.Component.Reactive (_derived, _input, _transient)
import Halogen.Component.Reactive as HR
import Halogen.Store.Connect (Connected)
import Marlowe.Execution.Types (NamedAction)
import Marlowe.Execution.Types as Execution
import Marlowe.Semantics (ChosenNum, TransactionInput)
import Type.Proxy (Proxy(..))

data Msg = DialogClosed

data Action
  = ConfirmAction
  | CancelConfirmation

type Input' = (Connected Instant Input)

type State = HR.State Input' Derived Transient

type Derived =
  { contractUserParties :: ContractUserParties
  , transactionFeeQuote :: BigInt
  , txInput :: TransactionInput
  }

_contractUserParties :: Lens' State ContractUserParties
_contractUserParties = _derived <<< prop (Proxy :: _ "contractUserParties")

_transactionFeeQuote :: Lens' State BigInt
_transactionFeeQuote = _derived <<< prop (Proxy :: _ "transactionFeeQuote")

_txInput :: Lens' State TransactionInput
_txInput = _derived <<< prop (Proxy :: _ "txInput")

type Transient =
  { pendingFiber :: Maybe (Fiber Unit)
  }

_pendingFiber :: Lens' State (Maybe (Fiber Unit))
_pendingFiber = _transient <<< prop (Proxy :: _ "pendingFiber")

type Input =
  { action :: NamedAction
  , executionState :: Execution.State
  , wallet :: PABConnectedWallet
  , chosenNum :: Maybe ChosenNum
  }

_action :: Lens' State NamedAction
_action = _input <<< prop (Proxy :: _ "input") <<< prop (Proxy :: _ "action")

_executionState :: Lens' State Execution.State
_executionState = _input <<< prop (Proxy :: _ "input") <<< prop
  (Proxy :: _ "executionState")

_wallet :: Lens' State PABConnectedWallet
_wallet = _input <<< prop (Proxy :: _ "input") <<< prop (Proxy :: _ "wallet")

_chosenNum :: Lens' State (Maybe ChosenNum)
_chosenNum = _input <<< prop (Proxy :: _ "input") <<< prop
  (Proxy :: _ "chosenNum")

_currentTime :: Lens' State Instant
_currentTime = _input <<< prop (Proxy :: _ "context")

type ChildSlots =
  ( expandSlot :: Expand.Slot Void String
  , submitButtonSlot :: H.Slot LoadingSubmitButton.Query Unit String
  )

type ComponentHTML m =
  H.ComponentHTML Action ChildSlots m

type DSL m a =
  HR.HalogenM Input' Derived Transient Action ChildSlots Msg m a

data Query (a :: Type)

type Slot m = H.Slot Query Msg m

_confirmActionDialog = Proxy :: Proxy "confirmActionDialog"
