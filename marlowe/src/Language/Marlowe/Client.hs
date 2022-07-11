module Language.Marlowe.Client where

import Language.Marlowe.Core.V1.Semantics (MarloweParams (MarloweParams, rolePayoutValidatorHash, rolesCurrency))
import Language.Marlowe.Scripts (mkRolePayoutValidatorHash)
import Ledger (CurrencySymbol)
import Plutus.V1.Ledger.Ada (adaSymbol)

marloweParams :: CurrencySymbol -> MarloweParams
marloweParams rc = MarloweParams
    { rolesCurrency = rc
    , rolePayoutValidatorHash = mkRolePayoutValidatorHash rc}

defaultMarloweParams :: MarloweParams
defaultMarloweParams = marloweParams adaSymbol


