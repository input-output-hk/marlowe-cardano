module Component.Contacts.Types where

import Prologue

import Analytics (class IsEvent, defaultEvent)
import Clipboard (Action) as Clipboard
import Component.AddContact.Types as AddContact
import Data.Address (Address)
import Data.AddressBook (AddressBook)
import Data.PABConnectedWallet (PABConnectedWallet)
import Data.WalletNickname (WalletNickname)
import Halogen as H
import Halogen.Component.Reactive as Reactive
import Halogen.Store.Connect (Connected)
import Type.Proxy (Proxy(..))

type State =
  Reactive.State (Connected AddressBook PABConnectedWallet) Unit CardSection

data CardSection
  = Home
  | ViewWallet WalletNickname Address
  -- TODO fix primitive obsession
  | NewWallet (Maybe String)

derive instance eqCardSection :: Eq CardSection

data Msg = Closed

type ChildSlots =
  ( addContact :: AddContact.Slot Unit
  )

_contacts = Proxy :: Proxy "contacts"

data Query (a :: Type)

type Component = H.Component Query PABConnectedWallet Msg

type Slot m = H.Slot Query Msg m

data Action
  = CloseContactsCard
  | SetCardSection CardSection
  | OnAddContactMsg AddContact.Msg
  | ClipboardAction Clipboard.Action

instance actionIsEvent :: IsEvent Action where
  toEvent CloseContactsCard = Just $ defaultEvent "CloseContactsCard"
  toEvent (SetCardSection _) = Just $ defaultEvent "SetCardSection"
  toEvent (OnAddContactMsg _) = Nothing
  toEvent (ClipboardAction _) = Just $ defaultEvent "ClipboardAction"
