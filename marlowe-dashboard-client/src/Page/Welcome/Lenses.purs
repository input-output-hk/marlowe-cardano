module Page.Welcome.Lenses where

import Prologue

import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Page.Welcome.Types (Card, State)
import Type.Proxy (Proxy(..))

_card :: Lens' State (Maybe Card)
_card = prop (Proxy :: _ "card")

_cardOpen :: Lens' State Boolean
_cardOpen = prop (Proxy :: _ "cardOpen")

_enteringDashboardState :: Lens' State Boolean
_enteringDashboardState = prop (Proxy :: _ "enteringDashboardState")
