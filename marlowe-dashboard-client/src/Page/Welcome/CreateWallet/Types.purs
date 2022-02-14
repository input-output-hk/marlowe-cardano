module Page.Welcome.CreateWallet.Types where

import Capability.Marlowe (NewWalletDetails)
import Halogen as H
import Type.Proxy (Proxy(..))

type Input = {}

data Msg
  = CancelClicked
  | WalletCreated NewWalletDetails

data Query (a :: Type)

type Slot m = H.Slot Query Msg m

type Component m = H.Component Query Input Msg m

_nickname = Proxy :: Proxy "nickname"
