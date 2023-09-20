module Language.Marlowe.Client where

import Language.Marlowe.Core.V1.Semantics (MarloweParams (MarloweParams, rolesCurrency))
import PlutusLedgerApi.V2 (CurrencySymbol (..), adaSymbol)

marloweParams :: CurrencySymbol -> MarloweParams
marloweParams rc =
  MarloweParams
    { rolesCurrency = rc
    }

defaultMarloweParams :: MarloweParams
defaultMarloweParams = marloweParams adaSymbol
