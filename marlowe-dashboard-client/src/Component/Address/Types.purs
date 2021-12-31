module Component.Address.Types (Input) where

import Marlowe.Semantics (PubKeyHash)

type Input
  =
  { inputId :: String
  , label :: String
  -- TODO: as part of SCP-3145 we should change this for a BECH32 address
  , value :: PubKeyHash
  }
