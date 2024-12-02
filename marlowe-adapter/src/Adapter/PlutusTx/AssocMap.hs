{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Adapter.PlutusTx.AssocMap where

import PlutusTx.AssocMap (Map, toList)
import PlutusTx.Eq (Eq (..))

-- | 'Eq' instance for Plutus Tx 'Map' type.
-- Nicolas Henin N.B :  It has been removed in plutus-tx versions  > 1.21, because it a partial equality implementation
-- You can compare only list with the same order of elements.
instance (Eq k, Eq v) => Eq (Map k v) where
  {-# INLINEABLE (==) #-}
  l == r = toList l == toList r
