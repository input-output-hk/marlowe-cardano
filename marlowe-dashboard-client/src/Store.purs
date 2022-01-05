module Store
  ( Action(..)
  , Store
  , reduce
  ) where

import Marlowe.Semantics (Slot)

type Store =
  { currentSlot :: Slot
  }

data Action = AdvanceToSlot Slot

reduce :: Store -> Action -> Store
-- TODO: currently we are only setting the currentSlot global variable, but once we
--       refactor contract state to live under the halogen store (SCP-3208) we can also move the
--       logic of AdvanceTimedoutSteps here.
reduce store (AdvanceToSlot newSlot) = store { currentSlot = newSlot }
