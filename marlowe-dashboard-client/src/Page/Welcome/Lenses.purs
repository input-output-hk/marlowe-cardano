module Page.Welcome.Lenses where

import Prologue

import Capability.Marlowe (NewWalletDetails)
import Data.Lens (Lens', Prism', prism')
import Data.Lens.Record (prop)
import Data.Tuple (uncurry)
import Page.Welcome.ConfirmMnemonic.Types (ConfirmMnemonicFields)
import Page.Welcome.CreateWallet.Types (CreateWalletFields)
import Page.Welcome.RestoreWallet.Types (RestoreWalletFields)
import Page.Welcome.Types (Card(..), CreateWalletStep(..), State)
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

_RestoreWalletCard :: Prism' Card RestoreWalletFields
_RestoreWalletCard = prism' RestoreWalletCard case _ of
  RestoreWalletCard fields -> Just fields
  _ -> Nothing

_CreateWalletSetWalletName :: Prism' CreateWalletStep CreateWalletFields
_CreateWalletSetWalletName = prism' CreateWalletSetWalletName case _ of
  CreateWalletSetWalletName fields -> Just fields
  _ -> Nothing

_CreateWalletPresentMnemonic :: Prism' CreateWalletStep NewWalletDetails
_CreateWalletPresentMnemonic = prism' CreateWalletPresentMnemonic case _ of
  CreateWalletPresentMnemonic details -> Just details
  _ -> Nothing

_CreateWalletConfirmMnemonic
  :: Prism' CreateWalletStep (Tuple ConfirmMnemonicFields NewWalletDetails)
_CreateWalletConfirmMnemonic = prism' (uncurry CreateWalletConfirmMnemonic)
  case _ of
    CreateWalletConfirmMnemonic fields details -> Just $ Tuple fields details
    _ -> Nothing
