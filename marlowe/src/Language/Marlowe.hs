module Language.Marlowe
  ( module Language.Marlowe.Core.V1.Semantics
  , module Language.Marlowe.Core.V1.Semantics.Types
  , module Language.Marlowe.Pretty
  , POSIXTime(..)
  , adaSymbol
  , adaToken
  , (%)
  ) where

import Language.Marlowe.Core.V1.Semantics
import Language.Marlowe.Core.V1.Semantics.Types hiding (getAction)
import Language.Marlowe.Pretty
import Plutus.V2.Ledger.Api (POSIXTime(..), adaSymbol, adaToken)
import PlutusTx.Ratio as P


{-# INLINABLE (%) #-}

(%) :: Integer -> Integer -> P.Rational
(%) = P.unsafeRatio
