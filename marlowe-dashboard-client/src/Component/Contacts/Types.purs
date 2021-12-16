module Component.Contacts.Types
  ( State
  , WalletLibrary
  , WalletNickname
  , WalletDetails
  , WalletInfo(..)
  , WalletId(..)
  , CardSection(..)
  , WalletNicknameError(..)
  , WalletIdError(..)
  , Action(..)
  ) where

import Prologue
import API.Url (class ToUrlPiece)
import Analytics (class IsEvent, defaultEvent, toEvent)
import Clipboard (Action) as Clipboard
import Component.InputField.Types (Action, State) as InputField
import Component.InputField.Types (class InputFieldError)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Marlowe.PAB (PlutusAppId)
import Marlowe.Semantics (Assets, MarloweData, MarloweParams, PubKeyHash)
import Types (NotFoundWebData)

type State
  = { walletLibrary :: WalletLibrary
    , cardSection :: CardSection
    , walletNicknameInput :: InputField.State WalletNicknameError
    , walletIdInput :: InputField.State WalletIdError
    , remoteWalletInfo :: NotFoundWebData WalletInfo
    }

type WalletLibrary
  = Map WalletNickname WalletDetails

type WalletNickname
  = String

type WalletDetails
  = { walletNickname :: WalletNickname
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
newtype WalletInfo
  = WalletInfo
  { walletId :: WalletId
  , pubKeyHash :: PubKeyHash
  }

derive instance newtypeWalletInfo :: Newtype WalletInfo _

derive instance eqWalletInfo :: Eq WalletInfo

derive instance genericWalletInfo :: Generic WalletInfo _

derive newtype instance encodeWalletInfo :: EncodeJson WalletInfo

derive newtype instance decodeJsonWalletInfo :: DecodeJson WalletInfo

newtype WalletId
  = WalletId String

derive instance newtypeWalletId :: Newtype WalletId _

derive instance eqWalletId :: Eq WalletId

derive instance genericWalletId :: Generic WalletId _

derive newtype instance encodeJsonWalletId :: EncodeJson WalletId

derive newtype instance decodeJsonWalletId :: DecodeJson WalletId

derive newtype instance toUrlPieceWalletId :: ToUrlPiece WalletId

data CardSection
  = Home
  | ViewWallet WalletDetails
  | NewWallet (Maybe String)

derive instance eqCardSection :: Eq CardSection

data WalletNicknameError
  = EmptyWalletNickname
  | DuplicateWalletNickname
  | BadWalletNickname

derive instance eqWalletNicknameError :: Eq WalletNicknameError

instance inputFieldErrorWalletNicknameError :: InputFieldError WalletNicknameError where
  inputErrorToString EmptyWalletNickname = "Nickname cannot be blank"
  inputErrorToString DuplicateWalletNickname = "Nickname is already in use in your contacts"
  inputErrorToString BadWalletNickname = "Nicknames can only contain letters and numbers"

data WalletIdError
  = EmptyWalletId
  | DuplicateWalletId
  | InvalidWalletId
  | UnconfirmedWalletId
  | NonexistentWalletId

derive instance eqWalletIdError :: Eq WalletIdError

instance inputeFieldErrorWalletIdError :: InputFieldError WalletIdError where
  inputErrorToString EmptyWalletId = "Wallet ID cannot be blank"
  inputErrorToString DuplicateWalletId = "Wallet ID is already in your contacts"
  inputErrorToString InvalidWalletId = "Wallet ID is not valid"
  inputErrorToString UnconfirmedWalletId = "Looking up wallet..."
  inputErrorToString NonexistentWalletId = "Wallet not found"

data Action
  = CloseContactsCard
  | SetCardSection CardSection
  | SaveWallet (Maybe String)
  | CancelNewContactForRole
  | WalletNicknameInputAction (InputField.Action WalletNicknameError)
  | WalletIdInputAction (InputField.Action WalletIdError)
  | SetRemoteWalletInfo (NotFoundWebData WalletInfo)
  | ConnectWallet WalletNickname PlutusAppId
  | ClipboardAction Clipboard.Action

instance actionIsEvent :: IsEvent Action where
  toEvent CloseContactsCard = Just $ defaultEvent "CloseContactsCard"
  toEvent (SetCardSection _) = Just $ defaultEvent "SetCardSection"
  toEvent (SaveWallet _) = Just $ defaultEvent "SaveWallet"
  toEvent CancelNewContactForRole = Nothing
  toEvent (WalletNicknameInputAction inputFieldAction) = toEvent inputFieldAction
  toEvent (WalletIdInputAction inputFieldAction) = toEvent inputFieldAction
  toEvent (SetRemoteWalletInfo _) = Nothing
  toEvent (ConnectWallet _ _) = Just $ defaultEvent "ConnectWallet"
  toEvent (ClipboardAction _) = Just $ defaultEvent "ClipboardAction"
