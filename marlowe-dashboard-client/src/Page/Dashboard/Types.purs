module Page.Dashboard.Types where

import Prologue

import Analytics (class IsEvent, defaultEvent)
import Clipboard (Action) as Clipboard
import Component.ConfirmContractActionDialog.Types as ConfirmContractActionDialog
import Component.Contacts.Types as Contacts
import Component.ContractPreview.Nickname as Nickname
import Component.CurrentStepActions.Types as CurrentStepActions
import Component.Expand as Expand
import Component.LoadingSubmitButton.Types as LoadingSubmitButton
import Component.Template.Types as Template
import Component.Tooltip.Types (ReferenceId)
import Data.Argonaut (Json, JsonDecodeError)
import Data.ContractNickname (ContractNickname)
import Data.ContractStatus (ContractStatusId)
import Data.DateTime.Instant (Instant)
import Data.Map (Map)
import Data.NewContract (NewContract)
import Data.PABConnectedWallet (PABConnectedWallet)
import Data.Slot as Slot
import Data.Time.Duration (Minutes)
import Data.UUID.Argonaut (UUID)
import Data.UserNamedActions (UserNamedActions)
import Data.WalletNickname (WalletNickname)
import Env (WalletFunds)
import Errors.Debuggable (class Debuggable)
import Errors.Explain (class Explain)
import Halogen as H
import Halogen.Component.Reactive as Reactive
import Halogen.Store.Connect (Connected)
import Language.Marlowe.Client (ContractHistory, MarloweError)
import Marlowe.Execution.Types (NamedAction)
import Marlowe.Execution.Types as Execution
import Marlowe.PAB (PlutusAppId)
import Marlowe.Semantics (ChosenNum, MarloweData, MarloweParams)
import Page.Contract.Types as ContractPage
import Plutus.Contract.Effects (ActiveEndpoint)
import Store.Contracts (ContractStore)
import Store.RoleTokens (RoleTokenStore)
import Text.Pretty (text)
import Type.Proxy (Proxy(..))
import Types (JsonAjaxError)

type ContractState =
  { executionState :: Execution.State
  , namedActions :: UserNamedActions
  , isClosed :: Boolean
  , nickname :: Maybe ContractNickname
  }

type State = Reactive.State
  (Connected Slice PABConnectedWallet)
  DerivedState
  TransientState

type TransientState =
  { walletCompanionStatus :: WalletCompanionStatus
  , menuOpen :: Boolean
  , card :: Maybe Card
  -- TODO use HalogenStore for modals. It would sure be nice to have portals...
  , cardOpen :: Boolean -- see note [CardOpen] in Welcome.State (the same applies here)
  , contractFilter :: ContractFilter
  , selectedContractIndex :: Maybe ContractStatusId
  , tzOffset :: Minutes
  }

type DerivedState =
  { newContracts :: Map UUID NewContract
  , contracts :: Map MarloweParams ContractState
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

data Query (a :: Type)

type Slice =
  { contracts :: ContractStore
  , currentTime :: Instant
  , roleTokens :: RoleTokenStore
  , tipSlot :: Slot.Slot
  }

data Msg

type Component = H.Component Query PABConnectedWallet Msg

type Slot = H.Slot Query Msg Unit

type ChildSlots =
  ( contacts :: Contacts.Slot Unit
  , contractNickname :: Nickname.Slot ContractStatusId
  , tooltipSlot :: forall query. H.Slot query Void ReferenceId
  , hintSlot :: forall query. H.Slot query Void String
  , submitButtonSlot :: H.Slot LoadingSubmitButton.Query Unit String
  , expandSlot :: Expand.Slot Void String
  , template :: Template.Slot Unit
  , contractPage :: ContractPage.Slot Unit
  , confirmActionDialog :: ConfirmContractActionDialog.Slot Unit
  , currentStepActions :: CurrentStepActions.Slot MarloweParams
  )

_dashboard = Proxy :: Proxy "dashboard"

_contractNickname = Proxy :: Proxy "contractNickname"

_template = Proxy :: Proxy "template"

data Action
  = DisconnectWallet (Maybe JsonAjaxError)
  | ToggleMenu
  | OpenCard Card
  | CloseCard
  | SetContractFilter ContractFilter
  | SelectContract (Maybe ContractStatusId)
  | RedeemPayments MarloweParams
  | OnAskContractActionConfirmation MarloweParams NamedAction (Maybe ChosenNum)
  | OnTemplateMsg Template.Msg
  | SetContactForRole String WalletNickname
  | ClipboardAction Clipboard.Action
  | OnContactsMsg Contacts.Msg
  | UpdateWalletFunds WalletFunds
  | NotificationParseFailed NotificationParseFailedError
  | CompanionAppStateUpdated (Map MarloweParams MarloweData)
  | MarloweContractCreated UUID MarloweParams
  | InputsApplied UUID
  | PaymentRedeemed UUID
  | CreateFailed UUID MarloweError
  | ApplyInputsFailed UUID MarloweError
  | RedeemFailed UUID MarloweError
  | ContractHistoryUpdated PlutusAppId ContractHistory
  | NewActiveEndpoints PlutusAppId (Array ActiveEndpoint)
  | MarloweAppClosed (Maybe Json)
  | FollowerAppClosed (Maybe Json) MarloweParams
  | WalletCompanionAppClosed (Maybe Json)
  | SlotChanged Slot.Slot
  | NicknameUpdated ContractStatusId ContractNickname
  | RestartFollower PlutusAppId MarloweParams

-- | Here we decide which top-level queries to track as GA events, and how to classify them.
instance actionIsEvent :: IsEvent Action where
  toEvent (DisconnectWallet _) = Just $ defaultEvent "DisconnectWallet"
  toEvent ToggleMenu = Just $ defaultEvent "ToggleMenu"
  toEvent (ClipboardAction _) = Just $ defaultEvent "ClipboardAction"
  toEvent (SetContractFilter _) = Just $ defaultEvent "FilterContracts"
  toEvent (SelectContract _) = Just $ defaultEvent "OpenContract"
  toEvent _ = Nothing

newtype NotificationParseFailedError = NotificationParseFailedError
  { whatFailed :: String
  , originalValue :: Json
  , parsingError :: JsonDecodeError
  }

instance Explain NotificationParseFailedError where
  explain _ = text
    "We received a message from the server that we can't understand."

derive newtype instance Debuggable NotificationParseFailedError
