module Page.Welcome.Lenses where

import Prologue

import Data.Lens (Lens', Prism', prism')
import Data.Lens.Record (prop)
import Page.Welcome.Types (Card(..), CreateWalletStep, State)
import Type.Proxy (Proxy(..))

_card :: Lens' State (Maybe Card)
_card = prop (Proxy :: _ "card")

_cardOpen :: Lens' State Boolean
_cardOpen = prop (Proxy :: _ "cardOpen")

_enteringDashboardState :: Lens' State Boolean
_enteringDashboardState = prop (Proxy :: _ "enteringDashboardState")

_CreateWalletCard :: Prism' Card CreateWalletStep
_CreateWalletCard = prism' CreateWalletCard case _ of
  CreateWalletCard step -> Just step
  _ -> Nothing
