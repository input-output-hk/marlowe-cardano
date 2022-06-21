module Contrib.Data.Array.Builder where

-- | Monoidal array builder which should have much better performance than concatenation.
-- |
-- | Please be aware that `JS` engines are generaly optimized for `push` (`snoc`)
-- | operation - O(1) vs `unshift` (`cons`) operation - O(n).
-- | We have here the opposit characteristic then in the case of `Data.List`.
-- |
-- | Use it for relatively small arrays (length < 10000) otherwise you can get `Nothing`...
-- | or just crash (stack overflow) in the case of `unsafeBuild`.
-- |
-- | `<>` is right associative and we execute its right hand
-- | side argument to first so it works should work in
-- | a pretty natural way:
-- | ```
-- | build $ cons 8 <> cons 9 <> cons 10 <> mempty == [8, 9, 10]
-- | ```
-- | and
-- | ```
-- | build $ snoc 8 <> snoc 9 <> snoc 10 <> mempty == [10, 9, 8]
-- | ```
-- | There are right (`:>` and `+>`) and right (`<:` and `<+`)
-- |associative operators provided by the lib which "should" behave
-- | as you would expect when mixed:
-- | ```
-- | build $ -3 :> [-2, -1] +> 0 :> mempty <: 1 <+ [1, 2] <: 3
-- | ```
-- |

import Prelude

import Data.Either (hush)
import Data.Maybe (Maybe)
import Effect.Exception as EE
import Effect.Unsafe (unsafePerformEffect)

newtype Builder a = Builder (Array a -> Array a)

instance semigroupBuilder :: Semigroup (Builder a) where
  append (Builder b1) (Builder b2) = Builder (b1 <<< b2)

instance monoidBuilder :: Monoid (Builder a) where
  mempty = Builder identity

foreign import unsafeCons :: forall a. a -> Array a -> Array a

foreign import unsafeConsArray :: forall a. Array a -> Array a -> Array a

foreign import unsafeSnoc :: forall a. a -> Array a -> Array a

foreign import unsafeSnocArray :: forall a. Array a -> Array a -> Array a

appendCons :: forall a. a -> Builder a -> Builder a
appendCons e1 e2 = cons e1 <> e2

-- | Like a `List.Cons` - it expects
-- | that the second argument is a `Builder`.
-- | So usually you want to do something like:
-- |
-- | ```
-- | x = build $ e1 :> e2 :> e3 :> mempty
-- | ```
infixr 6 appendCons as :>

appendSnoc :: forall a. Builder a -> a -> Builder a
appendSnoc e1 e2 = snoc e2 <> e1

-- | ```
-- | x = build $ mempty <: e1 <: e2 <: e3
-- | ```
infixl 7 appendSnoc as <:

snocArray :: forall a. Array a -> Builder a
snocArray suffix = Builder (unsafeSnocArray suffix)

appendSnocArray :: forall a. Builder a -> Array a -> Builder a
appendSnocArray b a = snocArray a <> b

-- | ```
-- | build $ <> -3 :> [-2, -1] +> 0 :> 1 :> 2 :> mempty <+ [3,4] <: 5 <: 6 <+ [7, 8] <: 9 <: 10
-- | ```
infixl 7 appendSnocArray as <+

-- | ```
-- | cons 1 <> consArray [2,3,4] <> cons 5 <> mempty
-- | ```
consArray :: forall a. Array a -> Builder a
consArray prefix = Builder (unsafeConsArray prefix)

appendConsArray :: forall a. Array a -> Builder a -> Builder a
appendConsArray a b = consArray a <> b

infixr 6 appendConsArray as +>

cons :: forall a. a -> Builder a
cons a = Builder (unsafeCons a)

snoc :: forall a. a -> Builder a
snoc a = Builder (unsafeSnoc a)

build :: forall a. Builder a -> Maybe (Array a)
build (Builder f) = hush $ unsafePerformEffect (EE.try (pure $ f []))

unsafeBuild :: forall a. Builder a -> Array a
unsafeBuild (Builder f) = f []

