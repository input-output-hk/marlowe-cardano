module Component.AddContact.Types where

import Data.Address (Address)
import Data.WalletNickname (WalletNickname)
import Halogen as H
import Halogen.Form.Input (FieldState)
import Type.Proxy (Proxy(..))

type Input =
  {
  }

data Msg
  = BackClicked
  | SaveClicked Contact

data Query (a :: Type)

type Slot m = H.Slot Query Msg m

type Component m = H.Component Query Input Msg m

type Contact =
  { address :: Address
  , nickname :: WalletNickname
  }

type AddContactFields =
  { address :: FieldState Address
  , nickname :: FieldState WalletNickname
  }

_nickname = Proxy :: Proxy "nickname"
_address = Proxy :: Proxy "address"
