module Page.Welcome.Lenses where

import Prologue

import Component.Contacts.Types (AddressBook)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Marlowe.PAB (PlutusAppId)
import Page.Welcome.Types (Card, State)
import Type.Proxy (Proxy(..))

_card :: Lens' State (Maybe Card)
_card = prop (Proxy :: _ "card")

_cardOpen :: Lens' State Boolean
_cardOpen = prop (Proxy :: _ "cardOpen")

_addressBook :: Lens' State AddressBook
_addressBook = prop (Proxy :: _ "addressBook")

_walletId :: Lens' State PlutusAppId
_walletId = prop (Proxy :: _ "walletId")

_enteringDashboardState :: Lens' State Boolean
_enteringDashboardState = prop (Proxy :: _ "enteringDashboardState")
