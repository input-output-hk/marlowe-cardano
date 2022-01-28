module Store.Contract
  ( Store
  , initialStore
  ) where

import Data.Map (Map)
import Data.Map as Map
import Marlowe.Execution.Types as Execution
import Marlowe.Semantics (MarloweParams)

-- This store reflects the logical state (or execution) of the Marlowe Contracts. It is populated from the
-- backend, and it is used to render the preview in the Dashboard page and the details in the Contract page
type Store = Map MarloweParams Execution.State

initialStore :: Store
initialStore = Map.empty
