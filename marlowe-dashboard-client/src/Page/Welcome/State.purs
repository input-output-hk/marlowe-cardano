module Page.Welcome.State
  ( dummyState
  , mkInitialState
  , handleAction
  ) where

import Prologue
import API.Marlowe.Run.TestnetWallet (RestoreError(..))
import Capability.MainFrameLoop (class MainFrameLoop, callMainFrameAction)
import Capability.Marlowe (class ManageMarlowe, lookupWalletDetails, restoreWallet)
import Capability.MarloweStorage (class ManageMarloweStorage, clearAllLocalStorage, insertIntoWalletLibrary)
import Capability.Toast (class Toast, addToast)
import Clipboard (class MonadClipboard)
import Clipboard (handleAction) as Clipboard
import Component.Contacts.Lenses (_companionAppId, _walletNickname)
import Component.Contacts.State (walletNicknameError)
import Component.Contacts.Types (WalletLibrary, WalletNicknameError)
import Component.InputField.Lenses (_value)
import Component.InputField.State (handleAction, mkInitialState) as InputField
import Component.InputField.Types (Action(..), State) as InputField
import Component.LoadingSubmitButton.Types (Query(..), _submitButtonSlot)
import Control.Monad.Reader (class MonadAsk)
import Data.Array as Array
import Data.Lens (assign, modifying, set, use, view, (^.))
import Data.Map (insert)
import Data.Map as Map
import Data.String (Pattern(..), split)
import Data.UUID.Argonaut (emptyUUID) as UUID
import Debug (traceM)
import Effect.Aff.Class (class MonadAff)
import Env (Env)
import Halogen (HalogenM, liftEffect, modify_, tell)
import Halogen.Extra (mapSubmodule)
import Halogen.Query.HalogenM (mapAction)
import MainFrame.Types (Action(..)) as MainFrame
import MainFrame.Types (ChildSlots, Msg)
import Marlowe.PAB (PlutusAppId(..))
import Network.RemoteData (RemoteData(..), fromEither)
import Page.Welcome.Lenses (_card, _cardOpen, _enteringDashboardState, _remoteWalletDetails, _walletId, _walletLibrary, _walletMnemonicInput, _walletNicknameInput)
import Page.Welcome.Types (Action(..), Card(..), State, WalletMnemonicError(..))
import Toast.Types (errorToast, successToast)
import Web.HTML (window)
import Web.HTML.Location (reload)
import Web.HTML.Window (location)
import Data.Time.Duration (Milliseconds(..))

-- see note [dummyState] in MainFrame.State
dummyState :: State
dummyState = mkInitialState Map.empty

mkInitialState :: WalletLibrary -> State
mkInitialState walletLibrary =
  { walletLibrary
  , card: Nothing
  , cardOpen: false
  , walletNicknameInput: InputField.mkInitialState Nothing
  , walletMnemonicInput: InputField.mkInitialState Nothing
  , walletId: PlutusAppId UUID.emptyUUID
  , remoteWalletDetails: NotAsked
  , enteringDashboardState: false
  }

words :: String -> Array String
words = Array.filter (notEq "") <<< split (Pattern " ")

walletMnemonicError :: Maybe String -> String -> Maybe WalletMnemonicError
walletMnemonicError invalidFromServer phrase =
  if Just phrase == invalidFromServer then
    Just InvalidMnemonicFromServer
  else if 24 /= (Array.length $ words phrase) then
    Just MnemonicAmountOfWords
  else
    Nothing

-- Some actions are handled in `MainFrame.State` because they involve
-- modifications of that state. See Note [State] in MainFrame.State.
handleAction ::
  forall m.
  MonadAff m =>
  MonadAsk Env m =>
  MainFrameLoop m =>
  ManageMarlowe m =>
  ManageMarloweStorage m =>
  Toast m =>
  MonadClipboard m =>
  Action -> HalogenM State Action ChildSlots Msg m Unit
handleAction (OpenCard card) = do
  -- TODO: make cards real halogen components to encapsulate initialization logic
  case card of
    RestoreTestnetWalletCard -> do
      walletLibrary <- use _walletLibrary
      handleAction $ WalletNicknameInputAction $ InputField.Reset
      handleAction $ WalletNicknameInputAction $ InputField.SetValidator $ walletNicknameError walletLibrary
      handleAction $ WalletMnemonicInputAction $ InputField.Reset
      handleAction $ WalletMnemonicInputAction $ InputField.SetValidator $ walletMnemonicError Nothing
    _ -> pure unit
  modify_
    $ set _card (Just card)
    <<< set _cardOpen true

handleAction CloseCard = do
  modify_
    $ set _remoteWalletDetails NotAsked
    <<< set _enteringDashboardState false
    <<< set _cardOpen false
    <<< set _walletId dummyState.walletId
  handleAction $ WalletNicknameInputAction $ InputField.Reset

{- [UC-WALLET-TESTNET-1][0] Create a new testnet wallet
Here we attempt to create a new demo wallet (with everything that entails), and - if successful -
open up the UseNewWalletCard for connecting the wallet just created.
Note the `createWallet` function doesn't just create a wallet. It also creates two PAB apps for
that wallet: a `WalletCompanion` and a `MarloweApp`.
- The `WalletCompanion` will watch for any new role tokens paid to this wallet, and then update its
  internal state to include the `MarloweParams` and initial `MarloweData` for the corresponding
  contract.
- The `MarloweApp` is a control app, used to create Marlowe contracts, apply inputs, and redeem
  payments to this wallet.
-}
-- TODO: Button disable, re-enable it as part of SCP-3170.
handleAction GenerateWallet = pure unit

-- walletLibrary <- use _walletLibrary
-- assign _remoteWalletDetails Loading
-- ajaxWalletDetails <- createWallet
-- assign _remoteWalletDetails $ fromEither $ lmap Just $ ajaxWalletDetails
-- case ajaxWalletDetails of
--   Left ajaxError -> addToast $ ajaxErrorToast "Failed to generate wallet." ajaxError
--   Right walletDetails -> do
--     handleAction $ WalletNicknameInputAction $ InputField.Reset
--     handleAction $ WalletNicknameInputAction $ InputField.SetValidator $ walletNicknameError walletLibrary
--     assign _walletId $ walletDetails ^. _companionAppId
--     handleAction $ OpenCard UseNewWalletCard
{- [UC-WALLET-TESTNET-2][0] Restore a testnet wallet
-}
handleAction RestoreTestnetWallet = do
  walletName <- use (_walletNicknameInput <<< _value)
  mnemonicPhraseStr <- use (_walletMnemonicInput <<< _value)
  let
    mnemonicPhrase = words mnemonicPhraseStr
  result <- restoreWallet { walletName, mnemonicPhrase, passphrase: "" }
  case result of
    Left InvalidMnemonic -> do
      tell _submitButtonSlot "restore-wallet" $ SubmitResult (Milliseconds 1200.0) (Left "Invalid mnemonic")
      handleAction $ WalletMnemonicInputAction $ InputField.SetValidator $ walletMnemonicError (Just mnemonicPhraseStr)
    Left _ -> do
      tell _submitButtonSlot "restore-wallet" $ SubmitResult (Milliseconds 1200.0) (Left "Error with server")
      handleAction $ CloseCard
    Right walletDetails -> do
      tell _submitButtonSlot "restore-wallet" $ SubmitResult (Milliseconds 1200.0) (Right "Wallet restored")
      traceM { msg: "oh yeas2", walletDetails }
      assign _remoteWalletDetails $ pure walletDetails
      -- TODO: SCP-3132 Fire logic to sync total funds
      handleAction $ ConnectWallet walletName

{- [Workflow 2][1] Connect a wallet
If we are connecting a wallet that was selected by the user inputting a wallet nickname, then we
will have a cache of it's `WalletDetails` in LocalStorage. But those details may well be out of
date. This intermediate step makes sure we have the current details before proceeding.
This is also factored out into a separate handler so that it can be called directly when the user
selects a wallet nickname from the dropdown menu (as well as indirectly via the previous handler).
-}
handleAction (OpenUseWalletCardWithDetails walletDetails) = do
  assign _remoteWalletDetails Loading
  ajaxWalletDetails <- lookupWalletDetails $ view _companionAppId walletDetails
  assign _remoteWalletDetails $ fromEither ajaxWalletDetails
  case ajaxWalletDetails of
    Left _ -> handleAction $ OpenCard LocalWalletMissingCard
    Right _ -> do
      handleAction $ WalletNicknameInputAction $ InputField.SetValue $ view _walletNickname walletDetails
      assign _walletId $ walletDetails ^. _companionAppId
      handleAction $ OpenCard UseWalletCard

handleAction (WalletNicknameInputAction inputFieldAction) = toWalletNicknameInput $ InputField.handleAction inputFieldAction

handleAction (WalletMnemonicInputAction inputFieldAction) = toWalletMnemonicInput $ InputField.handleAction inputFieldAction

{- [Workflow 2][2] Connect a wallet
This action is triggered by clicking the confirmation button on the UseWalletCard or
UseNewWalletCard. It saves the wallet nickname to LocalStorage, and then calls the
`MainFrame.EnterDashboardState` action.
-}
-- FIXME: Probably remove
handleAction (ConnectWallet walletNickname) = do
  assign _enteringDashboardState true
  remoteWalletDetails <- use _remoteWalletDetails
  case remoteWalletDetails of
    Success walletDetails -> do
      let
        walletDetailsWithNickname = set _walletNickname walletNickname walletDetails
      modifying _walletLibrary (insert walletNickname walletDetailsWithNickname)
      insertIntoWalletLibrary walletDetailsWithNickname
      walletLibrary <- use _walletLibrary
      callMainFrameAction $ MainFrame.EnterDashboardState walletLibrary walletDetailsWithNickname
    _ -> do
      -- this should never happen (the button to use a wallet should be disabled unless
      -- remoteWalletDetails is Success), but let's add some sensible behaviour anyway just in case
      handleAction CloseCard
      addToast $ errorToast "Unable to use this wallet." $ Just "Details for this wallet could not be loaded."

handleAction ClearLocalStorage = do
  clearAllLocalStorage
  liftEffect do
    location_ <- location =<< window
    reload location_

handleAction (ClipboardAction clipboardAction) = do
  mapAction ClipboardAction $ Clipboard.handleAction clipboardAction
  addToast $ successToast "Copied to clipboard"

------------------------------------------------------------
toWalletNicknameInput ::
  forall m msg slots.
  Functor m =>
  HalogenM (InputField.State WalletNicknameError) (InputField.Action WalletNicknameError) slots msg m Unit ->
  HalogenM State Action slots msg m Unit
toWalletNicknameInput = mapSubmodule _walletNicknameInput WalletNicknameInputAction

toWalletMnemonicInput ::
  forall m msg slots.
  Functor m =>
  HalogenM (InputField.State WalletMnemonicError) (InputField.Action WalletMnemonicError) slots msg m Unit ->
  HalogenM State Action slots msg m Unit
toWalletMnemonicInput = mapSubmodule _walletMnemonicInput WalletMnemonicInputAction
