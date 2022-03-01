module Arbitrary where

import Prologue

import Control.Lazy (defer)
import Control.Monad.Gen (class MonadGen, chooseInt, frequency, resize, sized)
import Data.Address.Bech32 (Bech32Address(..))
import Data.Address.Bech32.DataPart (Bech32DataPart)
import Data.Address.Bech32.DataPart as Bech32DataPart
import Data.Address.Bech32.DataPart.CodePoint (DataPartCodePoint)
import Data.Address.Bech32.HRP (Bech32HRP)
import Data.Address.Bech32.HRP as Bech32HRP
import Data.Address.Bech32.HRP.CodePoint (HRPCodePoint)
import Data.Address.Bech32.HRP.CodePoint as HRPCodePoint
import Data.Array.NonEmpty as ANE
import Data.Bifunctor (lmap)
import Data.Enum (class BoundedEnum, class Enum, fromEnum, pred, succ, toEnum)
import Data.Filterable (filter)
import Data.Functor.Compose (Compose(..))
import Data.Int as Int
import Data.Maybe (fromMaybe, maybe)
import Data.Newtype (class Newtype, un)
import Test.QuickCheck
  ( class Arbitrary
  , class Coarbitrary
  , arbitrary
  , coarbitrary
  )
import Test.QuickCheck.Arbitrary (genericArbitrary, genericCoarbitrary)

newtype ArbT a = ArbT a

derive instance Newtype (ArbT a) _
derive instance Functor ArbT
instance Apply ArbT where
  apply (ArbT f) (ArbT a) = ArbT $ f a

instance Applicative ArbT where
  pure = ArbT

derive newtype instance Eq a => Eq (ArbT a)
derive newtype instance Ord a => Ord (ArbT a)
derive newtype instance Show a => Show (ArbT a)
derive newtype instance Bounded a => Bounded (ArbT a)
derive newtype instance Enum a => Enum (ArbT a)
derive newtype instance BoundedEnum a => BoundedEnum (ArbT a)

sizedFrequenceDefault
  :: forall m a. MonadGen m => a -> Array (Tuple Int (m a)) -> m a
sizedFrequenceDefault default gens = sized \size ->
  fromMaybe (pure default)
    $ map frequency
    $ ANE.fromArray
    $ map (lmap Int.toNumber)
    $ filter ((_ >= size) <<< fst)
    $ gens

instance Arbitrary (ArbT HRPCodePoint) where
  arbitrary = sizedFrequenceDefault bottom
    [ Tuple 1 $ pure bottom
    , Tuple 1 $ pure top
    , Tuple 1 $ chooseInt 33 126 <#> \n ->
        ArbT $ fromMaybe bottom $ HRPCodePoint.fromCodePoint =<< toEnum n
    , Tuple 2 $ recurse 2 <#> \n -> fromMaybe n $ succ n
    , Tuple 2 $ recurse 2 <#> \n -> fromMaybe n $ pred n
    ]
    where
    recurse n = resize (_ / n) $ defer \_ -> arbitrary

instance Coarbitrary (ArbT HRPCodePoint) where
  coarbitrary = coarbitrary <<< fromEnum

instance Arbitrary (ArbT Bech32HRP) where
  arbitrary = do
    mHRP <-
      Bech32HRP.fromCodePoints <<< map (un ArbT) <<< ANE.toArray <$> arbitrary
    maybe arbitrary (pure <<< ArbT) mHRP

instance Coarbitrary (ArbT Bech32HRP) where
  coarbitrary = coarbitrary <<< Bech32HRP.toString <<< un ArbT

instance Arbitrary (ArbT DataPartCodePoint) where
  arbitrary = ArbT <$> genericArbitrary

instance Coarbitrary (ArbT DataPartCodePoint) where
  coarbitrary = genericCoarbitrary <<< un ArbT

instance Arbitrary (ArbT Bech32DataPart) where
  arbitrary = do
    mDataPart <-
      Bech32DataPart.fromCodePoints <<< map (un ArbT) <<< ANE.toArray <$>
        arbitrary
    maybe arbitrary (pure <<< ArbT) mDataPart

instance Coarbitrary (ArbT Bech32DataPart) where
  coarbitrary = coarbitrary <<< Bech32DataPart.toString <<< un ArbT

instance Arbitrary (ArbT Bech32Address) where
  arbitrary =
    un Compose $ Bech32Address <$> Compose arbitrary <*> Compose arbitrary

instance Coarbitrary (ArbT Bech32Address) where
  coarbitrary (ArbT (Bech32Address hrp dp)) =
    coarbitrary (ArbT hrp) <<< coarbitrary (ArbT dp)
