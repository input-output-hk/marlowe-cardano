module Page.Welcome.CreateWallet.Types where

import Capability.Marlowe (NewWalletDetails)
import Data.WalletNickname (WalletNickname)
import Halogen as H
import Halogen.Form.Input (FieldState)
import Type.Proxy (Proxy(..))

type Input =
  {
  }

data Msg
  = CancelClicked
  | WalletCreated NewWalletDetails

data Query (a :: Type)

type Slot m = H.Slot Query Msg m

type Component m = H.Component Query Input Msg m

type CreateWalletParams =
  { nickname :: WalletNickname
  }

type CreateWalletFields =
  { nickname :: FieldState WalletNickname
  }

_nickname = Proxy :: Proxy "nickname"
