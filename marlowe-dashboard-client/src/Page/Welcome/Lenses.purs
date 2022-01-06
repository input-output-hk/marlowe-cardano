module Page.Welcome.Lenses where

import Prologue
import Component.Contacts.Types
  ( WalletDetails
  , WalletLibrary
  , WalletNicknameError
  )
import Component.InputField.Types (State) as InputField
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Marlowe.PAB (PlutusAppId)
import Page.Welcome.Types (Card, State, WalletMnemonicError)
import Type.Proxy (Proxy(..))
import Types (NotFoundWebData)

_card :: Lens' State (Maybe Card)
_card = prop (Proxy :: _ "card")

_cardOpen :: Lens' State Boolean
_cardOpen = prop (Proxy :: _ "cardOpen")

_walletLibrary :: Lens' State WalletLibrary
_walletLibrary = prop (Proxy :: _ "walletLibrary")

_walletNicknameInput :: Lens' State (InputField.State WalletNicknameError)
_walletNicknameInput = prop (Proxy :: _ "walletNicknameInput")

_walletMnemonicInput :: Lens' State (InputField.State WalletMnemonicError)
_walletMnemonicInput = prop (Proxy :: _ "walletMnemonicInput")

_walletId :: Lens' State PlutusAppId
_walletId = prop (Proxy :: _ "walletId")

_remoteWalletDetails :: Lens' State (NotFoundWebData WalletDetails)
_remoteWalletDetails = prop (Proxy :: _ "remoteWalletDetails")

_enteringDashboardState :: Lens' State Boolean
_enteringDashboardState = prop (Proxy :: _ "enteringDashboardState")
