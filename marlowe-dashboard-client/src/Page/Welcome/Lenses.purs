module Page.Welcome.Lenses
  ( _card
  , _cardOpen
  , _walletLibrary
  , _walletNicknameOrIdInput
  , _walletNicknameInput
  , _walletId
  , _remoteWalletDetails
  , _enteringDashboardState
  ) where

import Prologue
import Component.Contacts.Types
  ( WalletDetails
  , WalletLibrary
  , WalletNicknameError
  )
import Component.InputField.Types (State) as InputField
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Type.Proxy (Proxy(..))
import Marlowe.PAB (PlutusAppId)
import Page.Welcome.Types (Card, State, WalletNicknameOrIdError)
import Types (NotFoundWebData)

_card :: Lens' State (Maybe Card)
_card = prop (Proxy :: _ "card")

_cardOpen :: Lens' State Boolean
_cardOpen = prop (Proxy :: _ "cardOpen")

_walletLibrary :: Lens' State WalletLibrary
_walletLibrary = prop (Proxy :: _ "walletLibrary")

_walletNicknameOrIdInput :: Lens' State
  (InputField.State WalletNicknameOrIdError)
_walletNicknameOrIdInput = prop (Proxy :: _ "walletNicknameOrIdInput")

_walletNicknameInput :: Lens' State (InputField.State WalletNicknameError)
_walletNicknameInput = prop (Proxy :: _ "walletNicknameInput")

_walletId :: Lens' State PlutusAppId
_walletId = prop (Proxy :: _ "walletId")

_remoteWalletDetails :: Lens' State (NotFoundWebData WalletDetails)
_remoteWalletDetails = prop (Proxy :: _ "remoteWalletDetails")

_enteringDashboardState :: Lens' State Boolean
_enteringDashboardState = prop (Proxy :: _ "enteringDashboardState")
