-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Utility functions for testing.
--
-----------------------------------------------------------------------------


{-# LANGUAGE RecordWildCards #-}


module Spec.Marlowe.Semantics.Util
  ( -- * Functions
    flattenMoney
  , stateEq
  , truncatedDivide
  ) where


import Data.Function (on)
import Language.Marlowe.Core.V1.Semantics.Types (Money, State(..), Token(..))
import Plutus.V1.Ledger.Value (flattenValue)
import Spec.Marlowe.Semantics.AssocMap (assocMapSort)

import qualified PlutusTx.Prelude as P (ratio)
import qualified PlutusTx.Ratio as P (truncate)


-- | Canonicalize a Marlowe state, so association-map entries are in sorted order.
canonicalState :: State -> State
canonicalState State{..} =
  State
    (assocMapSort accounts)
    (assocMapSort choices)
    (assocMapSort boundValues)
    minTime


-- | Check if two states are identical.
stateEq :: State -> State -> Bool
stateEq = (==) `on` canonicalState


-- | Flatten money into token-amount pairs.
flattenMoney :: Money -> [(Token, Integer)]
flattenMoney = fmap (\(s, n, a) -> (Token s n, a)) .  flattenValue


-- | Perform truncated division.
truncatedDivide :: Integer
                -> Integer
                -> Integer
truncatedDivide x y = maybe 0 P.truncate $ x `P.ratio` y
