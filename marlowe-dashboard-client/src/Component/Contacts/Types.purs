module Component.Contacts.Types
  ( State
  , WalletDetails
  , WalletInfo(..)
  , WalletId(..)
  , CardSection(..)
  , Action(..)
  ) where

import Prologue

import API.Url (class ToUrlPiece)
import Analytics (class IsEvent, defaultEvent)
import Clipboard (Action) as Clipboard
import Data.Address (Address)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.WalletNickname (WalletNickname)
import Marlowe.PAB (PlutusAppId)
import Marlowe.Semantics (Assets)
import Servant.PureScript (class ToPathSegment)

type State = { cardSection :: CardSection }

-- TODO: Move this data type away from the Contacts module and possibly rename.
--       A good location might just be a global Wallet module, and the name
--       could be plain `Wallet` or maybe `Wallet.State` (using qualified imports)
-- TODO: Consider hiding internal representation and creating an API instead
--       (raw records are primitive obsession, especially when they are shared so
--       pervasively).
type WalletDetails =
  { walletNickname :: WalletNickname
  , companionAppId :: PlutusAppId
  , marloweAppId :: PlutusAppId
  , walletInfo :: WalletInfo
  , assets :: Assets
  }

-- this is the data that the wallet API returns when creating a wallet and when subsequently requesting
-- its "own-public-key"
newtype WalletInfo = WalletInfo
  { walletId :: WalletId
  , pubKeyHash :: Address
  }

derive instance newtypeWalletInfo :: Newtype WalletInfo _

derive instance eqWalletInfo :: Eq WalletInfo

derive instance genericWalletInfo :: Generic WalletInfo _

derive newtype instance encodeWalletInfo :: EncodeJson WalletInfo

derive newtype instance decodeJsonWalletInfo :: DecodeJson WalletInfo

-- TODO fix primitive obsession
newtype WalletId = WalletId String

derive newtype instance ToPathSegment WalletId

derive instance newtypeWalletId :: Newtype WalletId _

derive instance eqWalletId :: Eq WalletId

derive instance genericWalletId :: Generic WalletId _

derive newtype instance encodeJsonWalletId :: EncodeJson WalletId

derive newtype instance decodeJsonWalletId :: DecodeJson WalletId

derive newtype instance toUrlPieceWalletId :: ToUrlPiece WalletId

data CardSection
  = Home
  | ViewWallet WalletNickname Address
  -- TODO fix primitive obsession
  | NewWallet (Maybe String)

derive instance eqCardSection :: Eq CardSection

data Action
  = CloseContactsCard
  | SetCardSection CardSection
  | SaveWallet (Maybe String) WalletNickname Address
  | CancelNewContactForRole
  | ClipboardAction Clipboard.Action

instance actionIsEvent :: IsEvent Action where
  toEvent CloseContactsCard = Just $ defaultEvent "CloseContactsCard"
  toEvent (SetCardSection _) = Just $ defaultEvent "SetCardSection"
  toEvent (SaveWallet _ _ _) = Just $ defaultEvent "SaveWallet"
  toEvent CancelNewContactForRole = Nothing
  toEvent (ClipboardAction _) = Just $ defaultEvent "ClipboardAction"
