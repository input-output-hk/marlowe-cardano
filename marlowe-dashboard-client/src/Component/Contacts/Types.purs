module Component.Contacts.Types
  ( State
  , WalletDetails
  , CardSection(..)
  , Action(..)
  ) where

import Prologue

import Analytics (class IsEvent, defaultEvent)
import Clipboard (Action) as Clipboard
import Data.Address (Address)
import Data.WalletNickname (WalletNickname)
import Marlowe.PAB (PlutusAppId)
import Marlowe.Run.Wallet.V1.Types (WalletInfo)
import Marlowe.Semantics (Assets)

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
