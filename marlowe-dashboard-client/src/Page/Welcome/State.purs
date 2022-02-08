module Page.Welcome.State
  ( initialState
  , handleAction
  ) where

import Prologue

import Capability.MainFrameLoop (class MainFrameLoop, callMainFrameAction)
import Capability.Marlowe (class ManageMarlowe)
import Capability.MarloweStorage
  ( class ManageMarloweStorage
  , modifyAddressBook_
  )
import Capability.Toast (class Toast)
import Clipboard (class MonadClipboard)
import Control.Monad.Reader (class MonadAsk)
import Data.Address as A
import Data.AddressBook as AddressBook
import Data.Lens (assign, view)
import Data.PaymentPubKeyHash (_PaymentPubKeyHash)
import Data.Wallet (_pubKeyHash, _walletNickname)
import Effect.Aff.Class (class MonadAff)
import Env (Env)
import Halogen (HalogenM, modify_)
import MainFrame.Types (Action(..)) as MainFrame
import MainFrame.Types (ChildSlots, Msg)
import Page.Welcome.ConfirmMnemonic.Types as ConfirmMnemonic
import Page.Welcome.CreateWallet.Types as CreateWallet
import Page.Welcome.Lenses (_enteringDashboardState)
import Page.Welcome.RestoreWallet.Types as RestoreWallet
import Page.Welcome.Types (Action(..), Card(..), CreateWalletStep(..), State)

initialState :: State
initialState =
  { card: Nothing
  , cardOpen: false
  , enteringDashboardState: false
  }

-- Some actions are handled in `MainFrame.State` because they involve
-- modifications of that state. See Note [State] in MainFrame.State.
handleAction
  :: forall m
   . MonadAff m
  => MonadAsk Env m
  => MainFrameLoop m
  => ManageMarlowe m
  => ManageMarloweStorage m
  => Toast m
  => MonadClipboard m
  => Action
  -> HalogenM State Action ChildSlots Msg m Unit
handleAction OnCreateWalletHelp = openCard CreateWalletHelpCard

handleAction OnGetStartedHelp = openCard GetStartedHelpCard

handleAction OnCreateWallet =
  openCard $ CreateWalletCard $ CreateWalletSetWalletName

handleAction OnRestoreWallet = openCard RestoreWalletCard

handleAction (OnAcknowledgeMnemonic details) =
  openCard $ CreateWalletCard $ CreateWalletConfirmMnemonic details

handleAction CloseCard =
  modify_ _ { enteringDashboardState = false, cardOpen = false }

{- [UC-WALLET-TESTNET-1][0] Create a new testnet wallet
   [UC-WALLET-TESTNET-2][4a] Restore a testnet wallet
This action is triggered either after the restore wallet form or create wallet form submit.
In both cases we receive a fully populated wallet details here so we can add them to the address
book. Then we enter the dashboard.

Both `createWallet` and `restoreWallet` (performed during respective form submition)
functions don't just create or restore a wallet.  They also create and attach or just attach to the
two PAB apps for that wallet: a `WalletCompanion` and a `MarloweApp`.
- The `WalletCompanion` will watch for any new role tokens paid to this wallet, and then update its
  internal state to include the `MarloweParams` and initial `MarloweData` for the corresponding
  contract.
- The `MarloweApp` is a control app, used to create Marlowe contracts, apply inputs, and redeem
  payments to this wallet.
-}
handleAction (ConnectWallet walletDetails) = do
  assign _enteringDashboardState true
  let
    walletNickname = view _walletNickname walletDetails
    pubKeyHash = view
      (_pubKeyHash <<< _PaymentPubKeyHash)
      walletDetails

  modifyAddressBook_
    (AddressBook.insert walletNickname $ A.fromPubKeyHash pubKeyHash)
  callMainFrameAction $ MainFrame.EnterDashboardState walletDetails

handleAction (OnRestoreWalletMsg msg) = case msg of
  RestoreWallet.CancelClicked -> handleAction CloseCard
  RestoreWallet.WalletRestored walletDetails ->
    handleAction $ ConnectWallet walletDetails

handleAction (OnCreateWalletMsg msg) = case msg of
  CreateWallet.CancelClicked -> handleAction CloseCard
  CreateWallet.WalletCreated details ->
    openCard $ CreateWalletCard $ CreateWalletPresentMnemonic details

handleAction (OnConfirmMnemonicMsg msg) = case msg of
  ConfirmMnemonic.BackClicked details ->
    openCard
      $ CreateWalletCard
      $ CreateWalletPresentMnemonic details
  ConfirmMnemonic.MnemonicConfirmed details ->
    handleAction $ ConnectWallet details.walletDetails

openCard :: forall m. Card -> HalogenM State Action ChildSlots Msg m Unit
openCard card =
  modify_ _ { card = Just card, cardOpen = true }
