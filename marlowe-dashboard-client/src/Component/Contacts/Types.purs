module Component.Contacts.Types
  ( State
  , CardSection(..)
  , Action(..)
  ) where

import Prologue

import Analytics (class IsEvent, defaultEvent)
import Clipboard (Action) as Clipboard
import Data.Address (Address)
import Data.WalletNickname (WalletNickname)

type State = { cardSection :: CardSection }

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
