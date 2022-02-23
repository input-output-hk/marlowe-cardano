-- TODO: Rename to ConfirmActionDialog
module Component.ConfirmInput.Types
  ( Action(..)
  , ChildSlots
  , ComponentHTML
  , DSL
  , Input
  , Msg(..)
  , Query(..)
  , Slot
  , State
  , _confirmActionDialog
  ) where

import Prologue

import Component.Expand as Expand
import Component.LoadingSubmitButton.Types as LoadingSubmitButton
import Data.BigInt.Argonaut (BigInt)
import Data.ContractUserParties (ContractUserParties)
import Data.PABConnectedWallet (PABConnectedWallet)
import Halogen as H
import Marlowe.Execution.Types (NamedAction)
import Marlowe.Execution.Types as Execution
import Marlowe.Semantics (Slot) as Semantics
import Marlowe.Semantics (TransactionInput)
import Type.Proxy (Proxy(..))

data Msg
  = DialogClosed

data Action
  = ConfirmAction NamedAction
  | CancelConfirmation

type State =
  { action :: NamedAction
  , executionState :: Execution.State
  , contractUserParties :: ContractUserParties
  , currentSlot :: Semantics.Slot
  , transactionFeeQuote :: BigInt
  , txInput :: TransactionInput
  , wallet :: PABConnectedWallet
  }

type Input =
  { action :: NamedAction
  , executionState :: Execution.State
  , currentSlot :: Semantics.Slot
  , wallet :: PABConnectedWallet
  }

type ChildSlots =
  ( expandSlot :: Expand.Slot Void String
  , submitButtonSlot :: H.Slot LoadingSubmitButton.Query Unit String
  )

type ComponentHTML m =
  H.ComponentHTML Action ChildSlots m

type DSL m a =
  H.HalogenM State Action ChildSlots Msg m a

data Query (a :: Type)

type Slot m = H.Slot Query Msg m

_confirmActionDialog = Proxy :: Proxy "confirmActionDialog"
