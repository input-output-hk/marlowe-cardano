module Component.Contacts.Lenses where

import Prologue

import Component.Contacts.Types (CardSection, State)
import Data.AddressBook (AddressBook)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.PABConnectedWallet (PABConnectedWallet)
import Halogen.Component.Reactive (_input, _transient)
import Type.Proxy (Proxy(..))

_addressBook :: Lens' State AddressBook
_addressBook = _input <<< prop (Proxy :: _ "context")

_cardSection :: Lens' State CardSection
_cardSection = _transient

_wallet :: Lens' State PABConnectedWallet
_wallet = _input <<< prop (Proxy :: _ "input")
