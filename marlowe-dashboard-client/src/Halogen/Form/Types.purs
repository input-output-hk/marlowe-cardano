module Halogen.Form.Types where

import Prologue

import Data.Bifunctor (class Bifunctor)
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum (class BoundedEnum, class Enum)
import Data.Enum.Generic
  ( genericCardinality
  , genericFromEnum
  , genericPred
  , genericSucc
  , genericToEnum
  )
import Data.Generic.Rep (class Generic)
import Data.Lens (Prism, Prism', prism, prism')
import Data.Show.Generic (genericShow)
import Test.QuickCheck (class Arbitrary, class Coarbitrary)
import Test.QuickCheck.Arbitrary (genericArbitrary, genericCoarbitrary)

data FieldState input output
  = Blank
  | Incomplete input
  | Complete output

derive instance Generic (FieldState input output) _
derive instance Functor (FieldState input)
derive instance (Eq input, Eq output) => Eq (FieldState input output)
derive instance (Ord input, Ord output) => Ord (FieldState input output)

instance (Show input, Show output) => Show (FieldState input output) where
  show = genericShow

instance Bifunctor FieldState where
  bimap f g = case _ of
    Blank -> Blank
    Incomplete input -> Incomplete $ f input
    Complete output -> Complete $ g output

instance
  ( Enum input
  , Bounded input
  , Enum output
  , Bounded output
  ) =>
  Enum (FieldState input output) where
  succ = genericSucc
  pred = genericPred

instance
  ( Bounded input
  , Bounded output
  ) =>
  Bounded (FieldState input output) where
  top = genericTop
  bottom = genericBottom

instance
  ( BoundedEnum input
  , BoundedEnum output
  ) =>
  BoundedEnum (FieldState input output) where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

instance
  ( Arbitrary input
  , Arbitrary output
  ) =>
  Arbitrary (FieldState input output) where
  arbitrary = genericArbitrary

instance
  ( Coarbitrary input
  , Coarbitrary output
  ) =>
  Coarbitrary (FieldState input output) where
  coarbitrary = genericCoarbitrary

_Blank
  :: forall input output. Prism' (FieldState input output) Unit
_Blank = prism' (const Blank) case _ of
  Blank -> Just unit
  _ -> Nothing

_Incomplete
  :: forall input input' output
   . Prism
       (FieldState input output)
       (FieldState input' output)
       input
       input'
_Incomplete = prism Incomplete case _ of
  Blank -> Left Blank
  Incomplete input -> Right input
  Complete output -> Left $ Complete output

_Complete
  :: forall input output output'
   . Prism
       (FieldState input output)
       (FieldState input output')
       output
       output'
_Complete = prism Complete case _ of
  Blank -> Left Blank
  Incomplete input -> Left $ Incomplete input
  Complete output -> Right output
