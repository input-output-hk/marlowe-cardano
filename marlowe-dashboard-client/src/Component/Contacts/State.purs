module Component.Contacts.State
  ( initialState
  , handleAction
  , adaToken
  , getAda
  ) where

import Prologue

import Capability.MainFrameLoop (callMainFrameAction)
import Capability.Marlowe (class ManageMarlowe)
import Capability.MarloweStorage
  ( class ManageMarloweStorage
  , modifyAddressBook_
  )
import Capability.Toast (class Toast, addToast)
import Clipboard (class MonadClipboard)
import Clipboard (handleAction) as Clipboard
import Component.Contacts.Lenses
  ( _addressInput
  , _cardSection
  , _walletNicknameInput
  )
import Component.Contacts.Types
  ( Action(..)
  , AddressError(..)
  , CardSection(..)
  , State
  , WalletNicknameError(..)
  )
import Component.InputField.Lenses (_pristine, _value)
import Component.InputField.State (handleAction, mkInitialState) as InputField
import Component.InputField.Types (Action(..), State) as InputField
import Control.Monad.Reader (class MonadAsk)
import Data.Address as A
import Data.AddressBook (AddressBook)
import Data.AddressBook as AddressBook
import Data.Bifunctor (lmap)
import Data.BigInt.Argonaut (BigInt)
import Data.Either (either, hush)
import Data.Lens (assign, set, use)
import Data.Map (lookup)
import Data.Maybe (fromMaybe)
import Data.Newtype (unwrap)
import Data.Set as Set
import Data.WalletNickname as WN
import Effect.Aff.Class (class MonadAff)
import Env (Env)
import Halogen (HalogenM, modify_)
import Halogen.Extra (mapSubmodule)
import Halogen.Query.HalogenM (mapAction)
import MainFrame.Types (Action(..)) as MainFrame
import MainFrame.Types (ChildSlots, Msg)
import Marlowe.Semantics (Assets, CurrencySymbol, Token(..), TokenName)
import Page.Dashboard.Types as Dashboard
import Toast.Types (successToast)

initialState :: State
initialState =
  { cardSection: Home
  , walletNicknameInput: InputField.mkInitialState Nothing
  , addressInput: InputField.mkInitialState Nothing
  }

handleAction
  :: forall m
   . MonadAff m
  => MonadAsk Env m
  => ManageMarlowe m
  => ManageMarloweStorage m
  => Toast m
  => MonadClipboard m
  => AddressBook
  -> Action
  -> HalogenM State Action ChildSlots Msg m Unit
handleAction _ CloseContactsCard = callMainFrameAction
  $ MainFrame.DashboardAction
  $ Dashboard.CloseCard

handleAction addressBook (SetCardSection cardSection) = do
  case cardSection of
    NewWallet _ -> do
      handleAction addressBook $ WalletNicknameInputAction InputField.Reset
      handleAction addressBook
        $ WalletNicknameInputAction
        $ InputField.SetValidator
        $ either Just (const Nothing)
            <<< lmap walletNicknameErrorToLegacyError
            <<< WN.fromString (AddressBook.nicknames addressBook)
      handleAction addressBook $ AddressInputAction InputField.Reset
      handleAction addressBook
        $ AddressInputAction
        $ InputField.SetValidator
        $ either Just (const Nothing)
            <<< lmap addressErrorToLegacyError
            <<< A.fromString (AddressBook.addresses addressBook)
    _ -> pure unit
  assign _cardSection cardSection

handleAction _ (SaveWallet mTokenName) = do
  walletNicknameString <- use (_walletNicknameInput <<< _value)
  addressString <- use (_addressInput <<< _value)
  let
    result = Tuple
      <$> hush (WN.fromString Set.empty walletNicknameString)
      <*> hush (A.fromString Set.empty addressString)
  case result of
    Just (Tuple walletNickname address) -> do
      modifyAddressBook_ (AddressBook.insert walletNickname address)
      addToast $ successToast "Contact added"
      case mTokenName of
        -- if a tokenName was also passed, we are inside a template contract and we need to update role
        Just tokenName -> callMainFrameAction $ MainFrame.DashboardAction $
          Dashboard.SetContactForRole tokenName walletNickname
        -- If we don't have a tokenName, then we added the contact from the contact dialog and we should close the panel
        Nothing -> callMainFrameAction $ MainFrame.DashboardAction $
          Dashboard.CloseCard
      -- We reset the form after adding the contact
      modify_
        $ set (_walletNicknameInput <<< _value) ""
            <<< set (_walletNicknameInput <<< _pristine) true
            <<< set (_addressInput <<< _value) ""
            <<< set (_addressInput <<< _pristine) true
    -- TODO: show error feedback to the user (just to be safe - but this should never happen, because
    -- the button to save a new wallet should be disabled in this case)
    _ -> pure unit

handleAction _ CancelNewContactForRole = pure unit -- handled in Dashboard.State

handleAction _ (WalletNicknameInputAction inputFieldAction) =
  toWalletNicknameInput $ InputField.handleAction inputFieldAction

handleAction _ (AddressInputAction inputFieldAction) = toAddressInput $
  InputField.handleAction inputFieldAction

handleAction _ (ClipboardAction clipboardAction) = do
  mapAction ClipboardAction $ Clipboard.handleAction clipboardAction
  addToast $ successToast "Copied to clipboard"

------------------------------------------------------------
toWalletNicknameInput
  :: forall m msg slots
   . Functor m
  => HalogenM (InputField.State WalletNicknameError)
       (InputField.Action WalletNicknameError)
       slots
       msg
       m
       Unit
  -> HalogenM State Action slots msg m Unit
toWalletNicknameInput = mapSubmodule _walletNicknameInput
  WalletNicknameInputAction

toAddressInput
  :: forall m msg slots
   . Functor m
  => HalogenM (InputField.State AddressError) (InputField.Action AddressError)
       slots
       msg
       m
       Unit
  -> HalogenM State Action slots msg m Unit
toAddressInput = mapSubmodule _addressInput AddressInputAction

------------------------------------------------------------
-- The cardano Blockchain has Multi-Asset support, each monetary policy is identified
-- by a policyId which is also known as the CurrencySymbol. The ADA token is a special
-- case that has the empty string as both the token name and the currency symbol
-- https://docs.cardano.org/native-tokens/learn
-- https://github.com/input-output-hk/plutus/blob/1f31e640e8a258185db01fa899da63f9018c0e85/plutus-ledger-api/src/Plutus/V1/Ledger/Ada.hs#L45
-- TODO: We should probably move this to the Semantic module (marlowe-web-common), or a common Ledger module (web-common?), but definitely not
--       the contacts module.
adaCurrencySymbol :: CurrencySymbol
adaCurrencySymbol = ""

adaTokenName :: TokenName
adaTokenName = ""

adaToken :: Token
adaToken = Token adaCurrencySymbol adaTokenName

getAda :: Assets -> BigInt
getAda assets = fromMaybe zero $ lookup adaTokenName =<< lookup
  adaCurrencySymbol
  (unwrap assets)

walletNicknameErrorToLegacyError
  :: WN.WalletNicknameError -> WalletNicknameError
walletNicknameErrorToLegacyError = case _ of
  WN.Empty -> EmptyWalletNickname
  WN.Exists -> DuplicateWalletNickname
  WN.ContainsNonAlphaNumeric -> BadWalletNickname

addressErrorToLegacyError :: A.AddressError -> AddressError
addressErrorToLegacyError = case _ of
  A.Empty -> EmptyAddress
  A.Exists -> DuplicateAddress
  A.Invalid -> InvalidAddress
