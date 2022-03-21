module Component.Contacts.Lenses where

import Component.Contacts.Types (CardSection, State)
import Data.AddressBook (AddressBook)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.PABConnectedWallet (PABConnectedWallet)
import Type.Proxy (Proxy(..))

_addressBook :: Lens' State AddressBook
_addressBook = prop (Proxy :: _ "addressBook")

_cardSection :: Lens' State CardSection
_cardSection = prop (Proxy :: _ "cardSection")

_wallet :: Lens' State PABConnectedWallet
_wallet = prop (Proxy :: _ "wallet")
