module Spec.Marlowe.Semantics.Next.Common.Tuple
  ( uncurry3
  ) where


uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f ~(a,b,c) = f a b c
