module Component.Contacts.Types
  ( State
  , CardSection(..)
  , Action(..)
  ) where

import Prologue

import Analytics (class IsEvent, defaultEvent)
import Clipboard (Action) as Clipboard
import Component.AddContact.Types as AddContact
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
  | OnAddContactMsg (Maybe String) AddContact.Msg
  | ClipboardAction Clipboard.Action

instance actionIsEvent :: IsEvent Action where
  toEvent CloseContactsCard = Just $ defaultEvent "CloseContactsCard"
  toEvent (SetCardSection _) = Just $ defaultEvent "SetCardSection"
  toEvent (OnAddContactMsg _ _) = Nothing
  toEvent (ClipboardAction _) = Just $ defaultEvent "ClipboardAction"
