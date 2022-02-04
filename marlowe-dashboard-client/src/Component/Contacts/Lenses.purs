module Component.Contacts.Lenses
  ( _cardSection
  ) where

import Component.Contacts.Types (CardSection, State)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Type.Proxy (Proxy(..))

_cardSection :: Lens' State CardSection
_cardSection = prop (Proxy :: _ "cardSection")

