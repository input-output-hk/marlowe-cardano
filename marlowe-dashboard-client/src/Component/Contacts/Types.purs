module Component.Contacts.Types
  ( State
  , AddressBook
  , WalletNickname
  , WalletDetails
  , WalletInfo(..)
  , WalletId(..)
  , CardSection(..)
  , WalletNicknameError(..)
  , AddressError(..)
  , Action(..)
  ) where

import Prologue

import API.Url (class ToUrlPiece)
import Analytics (class IsEvent, defaultEvent, toEvent)
import Clipboard (Action) as Clipboard
import Component.InputField.Types (class InputFieldError)
import Component.InputField.Types (Action, State) as InputField
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Marlowe.PAB (PlutusAppId)
import Marlowe.Semantics (Assets, MarloweData, MarloweParams, PubKeyHash)

type State =
  { addressBook :: AddressBook
  , cardSection :: CardSection
  , walletNicknameInput :: InputField.State WalletNicknameError
  , addressInput :: InputField.State AddressError
  }

-- TODO: The changes to this code take us closer to
--       "SCP-3145 Use addresses instead of WalletId in the UI", but we still need to show
--       an actual BECH32 address instead of a PubKeyHash (which is only a subpart of the address)
type AddressBook = Map WalletNickname PubKeyHash

type WalletNickname = String

-- TODO: Move this data type away from the Contacts module and possibly rename.
--       A good location might just be a global Wallet module, and the name
--       could be plain `Wallet` or maybe `Wallet.State` (using qualified imports)
type WalletDetails =
  { walletNickname :: WalletNickname
  , companionAppId :: PlutusAppId
  , marloweAppId :: PlutusAppId
  , walletInfo :: WalletInfo
  , assets :: Assets
  -- this property shouldn't be necessary, but at the moment we are getting too many update notifications
  -- through the PAB - so until that bug is fixed, we use this to check whether an update notification
  -- really has changed anything
  , previousCompanionAppState :: Maybe (Map MarloweParams MarloweData)
  }

-- this is the data that the wallet API returns when creating a wallet and when subsequently requesting
-- its "own-public-key"
newtype WalletInfo = WalletInfo
  { walletId :: WalletId
  , pubKeyHash :: PubKeyHash
  }

derive instance newtypeWalletInfo :: Newtype WalletInfo _

derive instance eqWalletInfo :: Eq WalletInfo

derive instance genericWalletInfo :: Generic WalletInfo _

derive newtype instance encodeWalletInfo :: EncodeJson WalletInfo

derive newtype instance decodeJsonWalletInfo :: DecodeJson WalletInfo

newtype WalletId = WalletId String

derive instance newtypeWalletId :: Newtype WalletId _

derive instance eqWalletId :: Eq WalletId

derive instance genericWalletId :: Generic WalletId _

derive newtype instance encodeJsonWalletId :: EncodeJson WalletId

derive newtype instance decodeJsonWalletId :: DecodeJson WalletId

derive newtype instance toUrlPieceWalletId :: ToUrlPiece WalletId

data CardSection
  = Home
  -- TODO: as part of SCP-3145 change PubKeyHash to BECH32 address
  | ViewWallet WalletNickname PubKeyHash
  | NewWallet (Maybe String)

derive instance eqCardSection :: Eq CardSection

data WalletNicknameError
  = EmptyWalletNickname
  | DuplicateWalletNickname
  | BadWalletNickname

derive instance eqWalletNicknameError :: Eq WalletNicknameError

instance inputFieldErrorWalletNicknameError ::
  InputFieldError WalletNicknameError where
  inputErrorToString EmptyWalletNickname = "Nickname cannot be blank"
  inputErrorToString DuplicateWalletNickname =
    "Nickname is already in use in your contacts"
  inputErrorToString BadWalletNickname =
    "Nicknames can only contain letters and numbers"

data AddressError
  = EmptyAddress
  | DuplicateAddress
  | InvalidAddress

derive instance eqAddressError :: Eq AddressError

instance inputeFieldErrorAddressError :: InputFieldError AddressError where
  inputErrorToString EmptyAddress = "The address cannot be blank"
  inputErrorToString DuplicateAddress =
    "The address is already in your contacts"
  inputErrorToString InvalidAddress = "The address is invalid"

data Action
  = CloseContactsCard
  | SetCardSection CardSection
  | SaveWallet (Maybe String)
  | CancelNewContactForRole
  | WalletNicknameInputAction (InputField.Action WalletNicknameError)
  | AddressInputAction (InputField.Action AddressError)
  | ClipboardAction Clipboard.Action

instance actionIsEvent :: IsEvent Action where
  toEvent CloseContactsCard = Just $ defaultEvent "CloseContactsCard"
  toEvent (SetCardSection _) = Just $ defaultEvent "SetCardSection"
  toEvent (SaveWallet _) = Just $ defaultEvent "SaveWallet"
  toEvent CancelNewContactForRole = Nothing
  toEvent (WalletNicknameInputAction inputFieldAction) =
    toEvent inputFieldAction
  toEvent (AddressInputAction inputFieldAction) = toEvent inputFieldAction
  toEvent (ClipboardAction _) = Just $ defaultEvent "ClipboardAction"
