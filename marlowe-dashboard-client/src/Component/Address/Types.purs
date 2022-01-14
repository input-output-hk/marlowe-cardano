module Component.Address.Types (Input) where

import Data.Address (Address)

type Input =
  { inputId :: String
  , label :: String
  , value :: Address
  }
