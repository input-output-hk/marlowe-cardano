module Page.Welcome.ConfirmMnemonic.Types where

import Capability.Marlowe (NewWalletDetails)
import Data.MnemonicPhrase (MnemonicPhrase)
import Halogen as H
import Halogen.Form.Input (FieldState)
import Type.Proxy (Proxy(..))

type Input =
  { fields :: ConfirmMnemonicFields
  , newWalletDetails :: NewWalletDetails
  }

data Msg
  = BackClicked NewWalletDetails
  | MnemonicConfirmed NewWalletDetails
  | FieldsUpdated ConfirmMnemonicFields

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
