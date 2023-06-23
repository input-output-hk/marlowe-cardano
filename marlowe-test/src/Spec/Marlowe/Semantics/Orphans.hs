-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-----------------------------------------------------------------------------
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Orphans, only for the test framework.
module Spec.Marlowe.Semantics.Orphans (
  ) where

import Language.Marlowe.Core.V1.Semantics (ApplyResult (..), ApplyWarning (..), ReduceResult (..), ReduceWarning (..))

import qualified PlutusTx.AssocMap as AM (Map, empty, fromList, toList)

deriving instance Eq ApplyWarning

deriving instance Eq ApplyResult

deriving instance Eq ReduceWarning

deriving instance Eq ReduceResult

instance Semigroup (AM.Map k v) where
  x <> y = AM.fromList $ AM.toList x <> AM.toList y

instance Monoid (AM.Map k v) where
  mempty = AM.empty
