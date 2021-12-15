module Contrib.Data.Array.Builder where

import Prelude

-- | Monoidal array builder which should
-- | have much better performance than concatenation.
-- |
-- | Use it for relatively small arrays (length < 10000).
-- | It is not stack safe.
newtype Builder a
  = Builder (Array a → Array a)

instance semigroupBuilder ∷ Semigroup (Builder a) where
  append (Builder b1) (Builder b2) = Builder (b1 <<< b2)

instance monoidBuilder ∷ Monoid (Builder a) where
  mempty = Builder identity

foreign import unsafeAppend ∷ ∀ a. Array a → Array a → Array a

foreign import unsafeCons ∷ ∀ a. a → Array a → Array a

foreign import unsafePrepend ∷ ∀ a. Array a → Array a → Array a

foreign import unsafeSnoc ∷ ∀ a. a → Array a → Array a

append :: forall a. Array a -> Builder a
append suffix = Builder (unsafeAppend suffix)

cons ∷ ∀ a. a → Builder a
cons a = Builder (unsafeCons a)

prepend :: forall a. Array a -> Builder a
prepend prefix = Builder (unsafePrepend prefix)

snoc ∷ ∀ a. a → Builder a
snoc a = Builder (unsafeSnoc a)

build ∷ ∀ a. Builder a → Array a
build (Builder f) = f []
