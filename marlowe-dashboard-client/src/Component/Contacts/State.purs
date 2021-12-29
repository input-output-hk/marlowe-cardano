module Component.Contacts.State
  ( mkInitialState
  , defaultWalletDetails
  , handleAction
  , adaToken
  , getAda
  , walletNicknameError
  , walletIdError
  , parsePlutusAppId
  ) where

import Prologue

import Capability.MainFrameLoop (callMainFrameAction)
import Capability.Marlowe (class ManageMarlowe, lookupWalletInfo)
import Capability.MarloweStorage
  ( class ManageMarloweStorage
  , insertIntoAddressBook
  )
import Capability.Toast (class Toast, addToast)
import Clipboard (class MonadClipboard)
import Clipboard (handleAction) as Clipboard
import Component.Contacts.Lenses
  ( _addressBook
  , _cardSection
  , _pubKeyHash
  , _remoteWalletInfo
  , _walletIdInput
  , _walletNicknameInput
  )
import Component.Contacts.Types
  ( Action(..)
  , AddressBook
  , CardSection(..)
  , State
  , WalletDetails
  , WalletId(..)
  , WalletIdError(..)
  , WalletInfo(..)
  , WalletNickname
  , WalletNicknameError(..)
  )
import Component.InputField.Lenses (_pristine, _value)
import Component.InputField.State (handleAction, mkInitialState) as InputField
import Component.InputField.Types (Action(..), State) as InputField
import Control.Monad.Reader (class MonadAsk)
import Data.Array (any)
import Data.BigInt.Argonaut (BigInt)
import Data.CodePoint.Unicode (isAlphaNum)
import Data.Foldable (for_)
import Data.Lens (assign, modifying, set, use, view)
import Data.Map (filter, insert, isEmpty, lookup, member)
import Data.Maybe (fromMaybe)
import Data.Newtype (unwrap)
import Data.String (codePointFromChar)
import Data.String.CodeUnits (toCharArray)
import Data.UUID.Argonaut (emptyUUID, parseUUID)
import Effect.Aff.Class (class MonadAff)
import Env (Env)
import Halogen (HalogenM, modify_)
import Halogen.Extra (mapSubmodule)
import Halogen.Query.HalogenM (mapAction)
import MainFrame.Types (Action(..)) as MainFrame
import MainFrame.Types (ChildSlots, Msg)
import Marlowe.PAB (PlutusAppId(..))
import Marlowe.Semantics (Assets, CurrencySymbol, Token(..), TokenName)
import Network.RemoteData (RemoteData(..), fromEither)
import Page.Dashboard.Types (Action(..)) as Dashboard
import Toast.Types (successToast)
import Types (NotFoundWebData)

mkInitialState :: AddressBook -> State
mkInitialState addressBook =
  { addressBook
  , cardSection: Home
  , walletNicknameInput: InputField.mkInitialState Nothing
  , walletIdInput: InputField.mkInitialState Nothing
  , remoteWalletInfo: NotAsked
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
      assign _remoteWalletInfo NotAsked
      handleAction $ WalletNicknameInputAction InputField.Reset
      handleAction
        $ WalletNicknameInputAction
        $ InputField.SetValidator
        $ walletNicknameError addressBook
      handleAction $ WalletIdInputAction InputField.Reset
      handleAction
        $ WalletIdInputAction
        $ InputField.SetValidator
        $ walletIdError NotAsked addressBook
    _ -> pure unit
  assign _cardSection cardSection

handleAction (SaveWallet mTokenName) = do
  walletNickname <- use (_walletNicknameInput <<< _value)
  walletIdString <- use (_walletIdInput <<< _value)
  remoteWalletInfo <- use _remoteWalletInfo
  let
    mWalletId = parsePlutusAppId walletIdString
  case remoteWalletInfo, mWalletId of
    Success walletInfo, Just walletId -> do
      let
        address = view _pubKeyHash walletInfo
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
            <<< set (_walletIdInput <<< _value) ""
            <<< set (_walletIdInput <<< _pristine) true
    -- TODO: show error feedback to the user (just to be safe - but this should never happen, because
    -- the button to save a new wallet should be disabled in this case)
    _, _ -> pure unit

handleAction CancelNewContactForRole = pure unit -- handled in Dashboard.State

handleAction (WalletNicknameInputAction inputFieldAction) =
  toWalletNicknameInput $ InputField.handleAction inputFieldAction

handleAction (WalletIdInputAction inputFieldAction) = do
  case inputFieldAction of
    InputField.SetValue walletIdString -> do
      -- note we handle the inputFieldAction _first_ so that the InputField value is set - otherwise the
      -- validation feedback is wrong while the rest is happening
      toWalletIdInput $ InputField.handleAction inputFieldAction
      setRemoteWalletInfo NotAsked
      -- if this is a valid contract ID ...
      for_ (parsePlutusAppId walletIdString) \walletId -> do
        setRemoteWalletInfo Loading
        -- .. lookup wallet info
        ajaxWalletInfo <- lookupWalletInfo walletId
        setRemoteWalletInfo $ fromEither ajaxWalletInfo
    _ -> toWalletIdInput $ InputField.handleAction inputFieldAction

handleAction (ClipboardAction clipboardAction) = do
  mapAction ClipboardAction $ Clipboard.handleAction clipboardAction
  addToast $ successToast "Copied to clipboard"

-- TODO: As part of SCP-2865, we should refactor all InputField to be a proper halogen component instead
--       of a sub-component. Then we could remove the SetValidator logic here that requires this function
--       to have all the capabilities that the component has (because we need to use handleAction)
setRemoteWalletInfo
  :: forall m
   . MonadAff m
  => MonadAsk Env m
  => ManageMarlowe m
  => ManageMarloweStorage m
  => Toast m
  => MonadClipboard m
  => (NotFoundWebData WalletInfo)
  -> HalogenM State Action ChildSlots Msg m Unit
setRemoteWalletInfo info = do
  assign _remoteWalletInfo info
  addressBook <- use _addressBook
  handleAction
    $ WalletIdInputAction
    $ InputField.SetValidator
    $ walletIdError info addressBook

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

toWalletIdInput
  :: forall m msg slots
   . Functor m
  => HalogenM (InputField.State WalletIdError) (InputField.Action WalletIdError)
       slots
       msg
       m
       Unit
  -> HalogenM State Action slots msg m Unit
toWalletIdInput = mapSubmodule _walletIdInput WalletIdInputAction

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

walletIdError
  :: NotFoundWebData WalletInfo -> AddressBook -> String -> Maybe WalletIdError
walletIdError _ _ "" = Just EmptyWalletId

walletIdError remoteDataWalletInfo addressBook walletIdString =
  case parsePlutusAppId walletIdString of
    Nothing -> Just InvalidWalletId
    Just plutusAppId -> Just DuplicateWalletId
    {- FIXME: change parsePlutusAppId to parsePubKeyHash and add link to SCP-3145
  -- | not $ isEmpty $ filter (\pubKeyHash -> pubKeyHash == plutusAppId) addressBook -> Just DuplicateWalletId-}
    _ -> case remoteDataWalletInfo of
      Success _ -> Nothing
      Failure _ -> Just NonexistentWalletId
      _ -> Just UnconfirmedWalletId

parsePlutusAppId :: String -> Maybe PlutusAppId
parsePlutusAppId plutusAppIdString = case parseUUID plutusAppIdString of
  Just uuid -> Just $ PlutusAppId uuid
  Nothing -> Nothing
