module Contrib.Variant where

import Data.Variant (Unvariant(..), Variant, unvariant)
import Type.Prelude (reflectSymbol)

tag ∷ ∀ v. Variant v → String
tag v = do
  let
    Unvariant c = unvariant v
  c \s _ → reflectSymbol s
