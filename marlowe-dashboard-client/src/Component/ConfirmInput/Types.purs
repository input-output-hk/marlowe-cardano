module Component.ConfirmInput.Types where

import Data.BigInt.Argonaut (BigInt)
import Marlowe.Execution.Types (NamedAction)
import Marlowe.Semantics (Slot) as Semantics
import Page.Contract.Types as Contract

type Input =
  { action :: NamedAction
  , contractState :: Contract.StartedState
  , currentSlot :: Semantics.Slot
  , transactionFeeQuote :: BigInt
  , userNickname :: String
  , walletBalance :: BigInt
  }
