module Language.Marlowe.Client where

import Language.Marlowe.Core.V1.Semantics (MarloweParams (MarloweParams, rolesCurrency))
import Plutus.V1.Ledger.Ada (adaSymbol)
import Plutus.V1.Ledger.Api (CurrencySymbol (..))

marloweParams :: CurrencySymbol -> MarloweParams
marloweParams rc = MarloweParams
    { rolesCurrency = rc
    }

defaultMarloweParams :: MarloweParams
defaultMarloweParams = marloweParams adaSymbol


