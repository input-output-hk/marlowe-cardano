module Component.Contacts.State
  ( mkInitialState
  , defaultWalletDetails
  , handleAction
  , adaToken
  , getAda
  , walletNicknameError
  , walletIdError
  ) where

import Prologue

import Capability.MainFrameLoop (callMainFrameAction)
import Capability.Marlowe (class ManageMarlowe)
import Capability.MarloweStorage
  ( class ManageMarloweStorage
  , insertIntoAddressBook
  )
import Capability.Toast (class Toast, addToast)
import Clipboard (class MonadClipboard)
import Clipboard (handleAction) as Clipboard
import Component.Contacts.Lenses
  ( _addressBook
  , _addressInput
  , _cardSection
  , _walletNicknameInput
  )
import Component.Contacts.Types
  ( Action(..)
  , AddressBook
  , AddressError(..)
  , CardSection(..)
  , State
  , WalletDetails
  , WalletId(..)
  , WalletInfo(..)
  , WalletNickname
  , WalletNicknameError(..)
  )
import Component.InputField.Lenses (_pristine, _value)
import Component.InputField.State (handleAction, mkInitialState) as InputField
import Component.InputField.Types (Action(..), State) as InputField
import Control.Monad.Reader (class MonadAsk)
import Data.BigInt.Argonaut (BigInt)
import Data.CodePoint.Unicode (isAlphaNum)
import Data.Foldable (any)
import Data.Lens (assign, modifying, set, use)
import Data.Map (insert, lookup, member)
import Data.Maybe (fromMaybe)
import Data.Newtype (unwrap)
import Data.String (codePointFromChar)
import Data.String.CodeUnits (length, toCharArray)
import Data.UUID.Argonaut (emptyUUID)
import Effect.Aff.Class (class MonadAff)
import Env (Env)
import Halogen (HalogenM, modify_)
import Halogen.Extra (mapSubmodule)
import Halogen.Query.HalogenM (mapAction)
import MainFrame.Types (Action(..)) as MainFrame
import MainFrame.Types (ChildSlots, Msg)
import Marlowe.PAB (PlutusAppId(..))
import Marlowe.Semantics
  ( Assets
  , CurrencySymbol
  , PubKeyHash
  , Token(..)
  , TokenName
  )
import Page.Dashboard.Types (Action(..)) as Dashboard
import Toast.Types (successToast)

mkInitialState :: AddressBook -> State
mkInitialState addressBook =
  { addressBook
  , cardSection: Home
  , walletNicknameInput: InputField.mkInitialState Nothing
  , addressInput: InputField.mkInitialState Nothing
  }

defaultWalletDetails :: WalletDetails
defaultWalletDetails =
  { walletNickname: mempty
  , companionAppId: PlutusAppId emptyUUID
  , marloweAppId: PlutusAppId emptyUUID
  , walletInfo: defaultWalletInfo
  , assets: mempty
  , previousCompanionAppState: Nothing
  }

defaultWalletInfo :: WalletInfo
defaultWalletInfo =
  WalletInfo
    { walletId: WalletId ""
    , pubKeyHash: ""
    }

handleAction
  :: forall m
   . MonadAff m
  => MonadAsk Env m
  => ManageMarlowe m
  => ManageMarloweStorage m
  => Toast m
  => MonadClipboard m
  => Action
  -> HalogenM State Action ChildSlots Msg m Unit
handleAction CloseContactsCard = callMainFrameAction $ MainFrame.DashboardAction
  $ Dashboard.CloseCard

handleAction (SetCardSection cardSection) = do
  case cardSection of
    NewWallet _ -> do
      addressBook <- use _addressBook
      handleAction $ WalletNicknameInputAction InputField.Reset
      handleAction
        $ WalletNicknameInputAction
        $ InputField.SetValidator
        $ walletNicknameError addressBook
      handleAction $ AddressInputAction InputField.Reset
      handleAction
        $ AddressInputAction
        $ InputField.SetValidator
        $ walletIdError addressBook
    _ -> pure unit
  assign _cardSection cardSection

handleAction (SaveWallet mTokenName) = do
  walletNickname <- use (_walletNicknameInput <<< _value)
  addressAsString <- use (_addressInput <<< _value)
  let
    mAddress = parseAddress addressAsString
  case mAddress of
    Just address -> do
      modifying _addressBook (insert walletNickname address)
      insertIntoAddressBook walletNickname address
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

handleAction CancelNewContactForRole = pure unit -- handled in Dashboard.State

handleAction (WalletNicknameInputAction inputFieldAction) =
  toWalletNicknameInput $ InputField.handleAction inputFieldAction

handleAction (AddressInputAction inputFieldAction) = toAddressInput $
  InputField.handleAction inputFieldAction

handleAction (ClipboardAction clipboardAction) = do
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

walletNicknameError
  :: AddressBook -> WalletNickname -> Maybe WalletNicknameError
walletNicknameError _ "" = Just EmptyWalletNickname

walletNicknameError addressBook walletNickname =
  if member walletNickname addressBook then
    Just DuplicateWalletNickname
  else if
    any (\char -> not $ isAlphaNum $ codePointFromChar char) $ toCharArray
      walletNickname then
    Just BadWalletNickname
  else
    Nothing

walletIdError :: AddressBook -> String -> Maybe AddressError
walletIdError _ "" = Just EmptyWalletId

walletIdError addressBook walletIdString = case parseAddress walletIdString of
  Nothing -> Just InvalidWalletId
  Just address
    | any (eq address) addressBook -> Just DuplicateWalletId
  _ -> Nothing

-- TODO: As part of 3145, provide a better way to create an Address from a string.
parseAddress :: String -> Maybe PubKeyHash
parseAddress addressAsString =
  if length addressAsString == 56 then
    Just addressAsString
  else
    Nothing
