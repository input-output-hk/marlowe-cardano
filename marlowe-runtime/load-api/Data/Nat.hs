{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Nat
  ( module Network.TypedProtocol
  , SingNat(..)
  , SomeNat(..)
  , decideNat
  , fromNatural
  , toNatural
  , type (+)
  , type (-)
  , withSingNat
  , (%+)
  , (%-)
  ) where

import Data.Type.Equality (type (:~:)(Refl))
import Data.Void (Void)
import GHC.Natural (intToNatural, naturalToInt)
import GHC.TypeLits (TypeError)
import qualified GHC.TypeLits as TL
import Network.TypedProtocol (N(..), Nat(..), natToInt, unsafeIntToNat)
import Numeric.Natural (Natural)

data SomeNat = forall (n :: N). SomeNat (Nat n)

type family (+) (n :: N) (m :: N) :: N where
  'Z + n = n
  ('S n) + m = 'S (n + m)

type family (-) (n :: N) (m :: N) :: N where
  n - 'Z = n
  ('S n) - ('S m) = n - m
  'Z - ('S m) = TypeError ('TL.Text "cannot subtract a larger nat from a smaller one")

class SingNat (n :: N) where
  singNat :: Nat n

instance SingNat 'Z where
  singNat = Zero

instance SingNat n => SingNat ('S n) where
  singNat = Succ singNat

(%+) :: Nat n -> Nat m -> Nat (n + m)
(%+) n m = unsafeIntToNat $ natToInt n + natToInt m

(%-) :: Nat n -> Nat m -> Nat (n - m)
(%-) n m = unsafeIntToNat $ natToInt n - natToInt m

toNatural :: Nat n -> Natural
toNatural = intToNatural . natToInt

fromNatural :: Natural -> SomeNat
fromNatural = SomeNat . unsafeIntToNat . naturalToInt

type Refutation p = p -> Void

decideNat :: Nat n -> Nat m -> Either (Refutation (n :~: m)) (n :~: m)
decideNat Zero Zero = Right Refl
decideNat Succ{} Zero = Left \case
decideNat Zero Succ{} = Left \case
decideNat (Succ n) (Succ m) = case decideNat n m of
  Left refutation -> Left \Refl -> refutation Refl
  Right Refl -> Right Refl

withSingNat :: Nat n -> (SingNat n => a) -> a
withSingNat Zero = id
withSingNat (Succ n) = withSingNat n
