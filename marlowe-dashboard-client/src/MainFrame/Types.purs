module MainFrame.Types where

import Prologue

import Analytics (class IsEvent, defaultEvent, toEvent)
import Component.AddContact.Types as AddContact
import Component.ConfirmContractActionDialog.Types as ConfirmContractActionDialog
import Component.ContractSetup.Types as ContractSetup
import Component.CurrentStepActions.Types as CurrentStepActions
import Component.Expand as Expand
import Component.LoadingSubmitButton.Types as LoadingSubmitButton
import Component.Tooltip.Types (ReferenceId)
import Data.AddressBook (AddressBook)
import Data.Argonaut (Json, JsonDecodeError)
import Data.DateTime.Instant (Instant)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Time.Duration (Minutes)
import Data.UUID.Argonaut (UUID)
import Data.Wallet (SyncStatus)
import Data.WalletId (WalletId)
import Halogen as H
import Halogen.Extra (LifecycleEvent)
import Halogen.Store.Connect (Connected)
import Halogen.Subscription (Emitter)
import Language.Marlowe.Client (ContractHistory, MarloweError)
import Marlowe.PAB (PlutusAppId)
import Marlowe.Semantics (MarloweData, MarloweParams)
import Page.Contract.Types as ContractPage
import Page.Dashboard.Types (Action, State) as Dashboard
import Page.Welcome.ConfirmMnemonic.Types as ConfirmMnemonic
import Page.Welcome.CreateWallet.Types as CreateWallet
import Page.Welcome.RestoreWallet.Types as RestoreWallet
import Page.Welcome.Types (Action, State) as Welcome
import Plutus.Contract.Effects (ActiveEndpoint)
import Plutus.PAB.Webserver.Types (CombinedWSStreamToClient)
import Store.Contracts (ContractStore)
import Store.Wallet (WalletStore)
import Type.Proxy (Proxy(..))
import Web.Socket.Event.CloseEvent (CloseEvent, reason) as WS
import WebSocket.Support (FromSocket)

type Slice =
  { addressBook :: AddressBook
  , wallet :: WalletStore
  , contracts :: ContractStore
  , currentTime :: Instant
  }

type PollingSources =
  { walletRegular :: Emitter Unit
  , walletSync :: Emitter Unit
  }

type Sources =
  { pabWebsocket :: Emitter (FromSocket CombinedWSStreamToClient)
  , clock :: Emitter Unit
  , polling :: PollingSources
  }

type Input =
  { sources :: Sources
  }

-- The app exists in one of two main subStates: the "welcome" state for when you have
-- no wallet, and all you can do is generate one or create a new one; and the "dashboard"
-- state for when you have selected a wallet, and can do all of the things.
type State =
  { webSocketStatus :: WebSocketStatus
  , tzOffset :: Minutes
  , store :: Slice
  , subState :: Either Welcome.State Dashboard.State
  , sources :: Sources
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
  , currentStepActions :: CurrentStepActions.Slot MarloweParams
  )

_toaster :: Proxy "toaster"
_toaster = Proxy

------------------------------------------------------------
data Query a
  = MainFrameActionQuery Action a

data Msg
  = MainFrameActionMsg Action

------------------------------------------------------------
data Action
  = WelcomeAction Welcome.Action
  | DashboardAction Dashboard.Action
  | Receive (Connected Slice Input)
  | Init
  | Tick
  | OnPoll SyncStatus WalletId
  | NewWebSocketStatus WebSocketStatus
  | NotificationParseFailed String Json JsonDecodeError
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
  | WalletCompanionAppClosed (Maybe Json)

-- | Here we decide which top-level queries to track as GA events, and
-- how to classify them.
instance actionIsEvent :: IsEvent Action where
  toEvent (Receive _) = Nothing
  toEvent (Tick) = Nothing
  toEvent Init = Just $ defaultEvent "Init"
  toEvent (OnPoll _ _) = Nothing
  toEvent (WelcomeAction welcomeAction) = toEvent welcomeAction
  toEvent (DashboardAction dashboardAction) = toEvent dashboardAction
  toEvent _ = Nothing
