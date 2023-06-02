module Language.Marlowe.Client
  where

import Language.Marlowe.Core.V1.Semantics (MarloweParams(MarloweParams, rolesCurrency))
import Plutus.V2.Ledger.Api (CurrencySymbol(..), adaSymbol)

marloweParams :: CurrencySymbol -> MarloweParams
marloweParams rc = MarloweParams
    { rolesCurrency = rc
    }

defaultMarloweParams :: MarloweParams
defaultMarloweParams = marloweParams adaSymbol


