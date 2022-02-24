module MainFrame.Types where

import Prologue

import Analytics (class IsEvent, defaultEvent, toEvent)
import Component.AddContact.Types as AddContact
import Component.ConfirmContractActionDialog.Types as ConfirmContractActionDialog
import Component.ContractSetup.Types as ContractSetup
import Component.Expand as Expand
import Component.LoadingSubmitButton.Types as LoadingSubmitButton
import Component.Tooltip.Types (ReferenceId)
import Data.AddressBook (AddressBook)
import Data.Argonaut (Json, JsonDecodeError)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.PABConnectedWallet (PABConnectedWallet)
import Data.Time.Duration (Minutes)
import Data.UUID.Argonaut (UUID)
import Data.Wallet (SyncStatus)
import Data.WalletId (WalletId)
import Halogen as H
import Halogen.Extra (LifecycleEvent)
import Halogen.Store.Connect (Connected)
import Language.Marlowe.Client (MarloweError)
import Marlowe.Client (ContractHistory)
import Marlowe.PAB (PlutusAppId)
import Marlowe.Semantics (MarloweData, MarloweParams, Slot)
import Page.Contract.Types as ContractPage
import Page.Dashboard.Types (Action, State) as Dashboard
import Page.Welcome.ConfirmMnemonic.Types as ConfirmMnemonic
import Page.Welcome.CreateWallet.Types as CreateWallet
import Page.Welcome.RestoreWallet.Types as RestoreWallet
import Page.Welcome.Types (Action, State) as Welcome
import Plutus.Contract.Effects (ActiveEndpoint)
import Store.Contracts (ContractStore)
import Store.Wallet (WalletStore)
import Type.Proxy (Proxy(..))
import Web.Socket.Event.CloseEvent (CloseEvent, reason) as WS

type Slice =
  { addressBook :: AddressBook
  , wallet :: WalletStore
  , contracts :: ContractStore
  }

type Input =
  { tzOffset :: Minutes
  }

-- The app exists in one of two main subStates: the "welcome" state for when you have
-- no wallet, and all you can do is generate one or create a new one; and the "dashboard"
-- state for when you have selected a wallet, and can do all of the things.
type State =
  { webSocketStatus :: WebSocketStatus
  -- TODO: currentSlot, tzOffset, and addressBook should be stored in the global store rather than here, but in order
  --       to remove it from here we need to first change the sub-components that use this into proper components
  , currentSlot :: Slot
  , tzOffset :: Minutes
  , store :: Slice
  , subState :: Either Welcome.State Dashboard.State
  }

data WebSocketStatus
  = WebSocketOpen
  | WebSocketClosed (Maybe WS.CloseEvent)

derive instance genericWebSocketStatus :: Generic WebSocketStatus _

instance showWebSocketStatus :: Show WebSocketStatus where
  show WebSocketOpen = "WebSocketOpen"
  show (WebSocketClosed Nothing) = "WebSocketClosed"
  show (WebSocketClosed (Just closeEvent)) = "WebSocketClosed " <> WS.reason
    closeEvent

------------------------------------------------------------
type ChildSlots =
  ( addContact :: AddContact.Slot Unit
  , tooltipSlot :: forall query. H.Slot query Void ReferenceId
  , hintSlot :: forall query. H.Slot query Void String
  , submitButtonSlot :: H.Slot LoadingSubmitButton.Query Unit String
  , lifeCycleSlot :: forall query. H.Slot query LifecycleEvent String
  , expandSlot :: Expand.Slot Void String
  , confirmMnemonic :: ConfirmMnemonic.Slot Unit
  , createWallet :: CreateWallet.Slot Unit
  , restoreWallet :: RestoreWallet.Slot Unit
  , toaster :: forall q m. H.Slot q m Unit
  , contractSetup :: ContractSetup.Slot Unit
  , contractPage :: ContractPage.Slot Unit
  , confirmActionDialog :: ConfirmContractActionDialog.Slot Unit
  )

_toaster :: Proxy "toaster"
_toaster = Proxy

------------------------------------------------------------
data Query a
  = MainFrameActionQuery Action a
  | GetWallet (PABConnectedWallet -> a)
  | NewWebSocketStatus WebSocketStatus a
  | NotificationParseFailed String JsonDecodeError a
  | SlotChange Slot a
  | CompanionAppStateUpdated (Map MarloweParams MarloweData) a
  | MarloweContractCreated UUID MarloweParams a
  | InputsApplied UUID a
  | PaymentRedeemed UUID a
  | CreateFailed UUID MarloweError a
  | ApplyInputsFailed UUID MarloweError a
  | RedeemFailed UUID MarloweError a
  | ContractHistoryUpdated PlutusAppId ContractHistory a
  | NewActiveEndpoints PlutusAppId (Array ActiveEndpoint) a
  | MarloweAppClosed (Maybe Json) a
  | WalletCompanionAppClosed (Maybe Json) a

data Msg
  = MainFrameActionMsg Action

------------------------------------------------------------
data Action
  = WelcomeAction Welcome.Action
  | DashboardAction Dashboard.Action
  | Receive (Connected Slice Input)
  | Init
  | OnPoll SyncStatus WalletId

-- | Here we decide which top-level queries to track as GA events, and
-- how to classify them.
instance actionIsEvent :: IsEvent Action where
  toEvent (Receive _) = Just $ defaultEvent "Receive"
  toEvent Init = Just $ defaultEvent "Init"
  toEvent (OnPoll _ _) = Nothing
  toEvent (WelcomeAction welcomeAction) = toEvent welcomeAction
  toEvent (DashboardAction dashboardAction) = toEvent dashboardAction
