module Data.Actus.Contract where

import Data.Actus.Types (ContractRole)
import Data.DateTime (DateTime)

type Contract r =
  { statusDate :: DateTime
  , contractRole :: ContractRole
  , contractId :: String
  | r
  }
