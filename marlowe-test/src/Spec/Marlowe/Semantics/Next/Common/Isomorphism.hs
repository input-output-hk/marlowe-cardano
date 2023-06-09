{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Marlowe.Semantics.Next.Common.Isomorphism
  (
  ) where

import Data.Types.Isomorphic (Injective(..), Iso)
import Language.Marlowe.Core.V1.Semantics.Next (Indexed(..))



instance Injective a b  => Injective (Indexed a) (Indexed b) where
  to (Indexed caseIndex a) = Indexed caseIndex (to a)
instance Injective a b  => Injective [a]  [b] where
  to as = to <$> as
instance Iso a b  => Iso (Indexed a) (Indexed b)

