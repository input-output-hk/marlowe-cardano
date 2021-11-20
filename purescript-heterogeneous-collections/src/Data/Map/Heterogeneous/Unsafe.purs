module Data.Map.Heterogeneous.Unsafe
  ( UnsafeHMap
  , unsafeDelete
  , unsafeEmpty
  , unsafeGet
  , unsafeMember
  , unsafeSet
  ) where

foreign import data UnsafeHMap :: forall k. Row k -> Type

foreign import unsafeDelete
  :: forall r1 r2. String -> UnsafeHMap r1 -> UnsafeHMap r2

foreign import unsafeGet
  :: forall r a b. (a -> b) -> b -> String -> UnsafeHMap r -> b

foreign import unsafeMember :: forall r. String -> UnsafeHMap r -> Boolean
foreign import unsafeSet
  :: forall r1 r2 a. String -> a -> UnsafeHMap r1 -> UnsafeHMap r2

foreign import unsafeEmpty :: forall r. UnsafeHMap r
