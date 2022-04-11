{-# LANGUAGE TemplateHaskell #-}
module Marlowe.Run.Contract.V1.Types where

import Cardano.Api (AddressInEra, AlonzoEra)
import Control.Lens (makeLenses)
import Plutus.V1.Ledger.Api (CurrencySymbol, TokenName)
import Prelude

data RoleToken = RoleToken
  { _roleTokenCurrencySymbol :: !CurrencySymbol
  , _roleTokenTokenName      :: !TokenName
  , _roleTokenUtxoAddress    :: !(AddressInEra AlonzoEra)
  } deriving (Eq, Show)

makeLenses ''RoleToken
