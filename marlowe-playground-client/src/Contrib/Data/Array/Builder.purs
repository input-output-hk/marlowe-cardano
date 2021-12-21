module Contrib.Data.Array.Builder where

import Prelude

-- | Monoidal array builder which should
-- | have much better performance than concatenation.
-- |
-- | Use it for relatively small arrays (length < 10000).
-- | It is not stack safe.
newtype Builder a
  = Builder (Array a -> Array a)

instance semigroupBuilder :: Semigroup (Builder a) where
  append (Builder b1) (Builder b2) = Builder (b1 <<< b2)

instance monoidBuilder :: Monoid (Builder a) where
  mempty = Builder identity

foreign import unsafeAppend :: forall a. Array a -> Array a -> Array a

foreign import unsafeCons :: forall a. a -> Array a -> Array a

foreign import unsafePrepend :: forall a. Array a -> Array a -> Array a

foreign import unsafeSnoc :: forall a. a -> Array a -> Array a

append :: forall a. Array a -> Builder a
append suffix = Builder (unsafeAppend suffix)

cons :: forall a. a -> Builder a
cons a = Builder (unsafeCons a)

prepend :: forall a. Array a -> Builder a
prepend prefix = Builder (unsafePrepend prefix)

snoc :: forall a. a -> Builder a
snoc a = Builder (unsafeSnoc a)

build :: forall a. Builder a -> Array a
build (Builder f) = f []
