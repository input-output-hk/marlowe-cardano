module Language.Marlowe
    ( module Language.Marlowe.Core.V1.Semantics
    , module Language.Marlowe.Core.V1.Semantics.Types
    , module Language.Marlowe.Pretty
    -- , module Language.Marlowe.Client
    -- , module Language.Marlowe.Util
    , POSIXTime (..)
    , adaSymbol
    , adaToken
    , (%)
    )
where

import Language.Marlowe.Core.V1.Semantics
import Language.Marlowe.Core.V1.Semantics.Types hiding (getAction)
import Language.Marlowe.Pretty
-- import Language.Marlowe.Client
-- import Language.Marlowe.Util
import Plutus.V1.Ledger.Ada (adaSymbol, adaToken)
import Plutus.V1.Ledger.Api (POSIXTime (..))
import PlutusTx.Ratio as P


{-# INLINABLE (%) #-}

(%) :: Integer -> Integer -> P.Rational
(%) = P.unsafeRatio
