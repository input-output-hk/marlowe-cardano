module Page.Dashboard.Types
  ( Action(..)
  , State
  , Card(..)
  , ContractFilter(..)
  , Input
  , WalletCompanionStatus(..)
  ) where

import Prologue

import Analytics (class IsEvent, defaultEvent, toEvent)
import Clipboard (Action) as Clipboard
import Component.ConfirmContractActionDialog.Types as ConfirmContractActionDialog
import Component.Contacts.Types (Action, State) as Contacts
import Component.Template.Types (Action, State) as Template
import Data.AddressBook (AddressBook)
import Data.Map (Map)
import Data.PABConnectedWallet (PABConnectedWallet)
import Data.Time.Duration (Minutes)
import Data.WalletNickname (WalletNickname)
import Marlowe.Execution.Types (NamedAction)
import Marlowe.Semantics (MarloweData, MarloweParams, Slot)
import Store.Contracts (ContractStore)

type State =
  { contactsState :: Contacts.State
  , walletCompanionStatus :: WalletCompanionStatus
  , menuOpen :: Boolean
  , card :: Maybe Card
  -- TODO use HalogenStore for modals. It would sure be nice to have portals...
  , cardOpen :: Boolean -- see note [CardOpen] in Welcome.State (the same applies here)
  , contractStore :: ContractStore
  , contractFilter :: ContractFilter
  , selectedContractMarloweParams :: Maybe MarloweParams
  , templateState :: Template.State
  }

-- This represents the status of the wallet companion. When we start the application
-- we are waiting for the wallet companion to tell us of every Marlowe contract that
-- the user has. While we wait, we show a loading indicator in the dashboard.
-- After we have the initial status update, we show the state of every contract that
-- we were following, and we start to follow the new contracts that the user has.
data WalletCompanionStatus
  = WaitingToSync
  | WalletCompanionSynced

derive instance eqWalletCompanionStatus :: Eq WalletCompanionStatus

data Card
  = TutorialsCard
  | CurrentWalletCard
  | ContactsCard
  | ContractTemplateCard
  | ContractActionConfirmationCard ConfirmContractActionDialog.Input

data ContractFilter
  = Running
  | Completed

derive instance eqContractFilter :: Eq ContractFilter

type Input =
  { wallet :: PABConnectedWallet
  , contracts :: ContractStore
  , addressBook :: AddressBook
  , currentSlot :: Slot
  , tzOffset :: Minutes
  }

data Action
  = DisconnectWallet
  | ContactsAction Contacts.Action
  | ToggleMenu
  | OpenCard Card
  | CloseCard
  | SetContractFilter ContractFilter
  | SelectContract (Maybe MarloweParams)
  | UpdateFollowerApps (Map MarloweParams MarloweData)
  | RedeemPayments MarloweParams
  | OnAskContractActionConfirmation MarloweParams NamedAction
  | TemplateAction Template.Action
  | SetContactForRole String WalletNickname
  | ClipboardAction Clipboard.Action

-- | Here we decide which top-level queries to track as GA events, and how to classify them.
instance actionIsEvent :: IsEvent Action where
  toEvent DisconnectWallet = Just $ defaultEvent "DisconnectWallet"
  toEvent (ContactsAction contactsAction) = toEvent contactsAction
  toEvent ToggleMenu = Just $ defaultEvent "ToggleMenu"
  toEvent (OpenCard _) = Nothing
  toEvent (ClipboardAction _) = Just $ defaultEvent "ClipboardAction"
  toEvent CloseCard = Nothing
  toEvent (SetContractFilter _) = Just $ defaultEvent "FilterContracts"
  toEvent (SelectContract _) = Just $ defaultEvent "OpenContract"
  toEvent (UpdateFollowerApps _) = Nothing
  toEvent (RedeemPayments _) = Nothing
  toEvent (OnAskContractActionConfirmation _ _) = Nothing
  toEvent (TemplateAction _) = Nothing
  toEvent (SetContactForRole _ _) = Nothing
