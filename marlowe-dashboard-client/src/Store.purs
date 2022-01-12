module Store
  ( Action(..)
  , Store
  , reduce
  ) where

import Prologue

import Marlowe.Semantics (Slot)
import Toast.Types (ToastMessage)

type Store =
  { currentSlot :: Slot
  , toast :: Maybe ToastMessage
  }

data Action
  = AdvanceToSlot Slot
  | ShowToast ToastMessage
  | ClearToast

reduce :: Store -> Action -> Store
-- TODO: currently we are only setting the currentSlot global variable, but once we
--       refactor contract state to live under the halogen store (SCP-3208) we can also move the
--       logic of AdvanceTimedoutSteps here.
reduce store = case _ of
  AdvanceToSlot newSlot -> store { currentSlot = newSlot }
  ShowToast msg -> store { toast = Just msg }
  ClearToast -> store { toast = Nothing }
