module Page.Welcome.ConfirmMnemonic.Types where

import Capability.Marlowe (NewWalletDetails)
import Data.MnemonicPhrase (MnemonicPhrase)
import Halogen as H
import Halogen.Form.Input (FieldState)
import Type.Proxy (Proxy(..))

type Input =
  { newWalletDetails :: NewWalletDetails
  }

data Msg
  = BackClicked NewWalletDetails
  | MnemonicConfirmed NewWalletDetails

data Query (a :: Type)

type Slot m = H.Slot Query Msg m

type Component m = H.Component Query Input Msg m

type ConfirmMnemonicParams =
  { mnemonic :: MnemonicPhrase
  }

type ConfirmMnemonicFields =
  { mnemonic :: FieldState MnemonicPhrase
  }

_mnemonic = Proxy :: Proxy "mnemonic"
