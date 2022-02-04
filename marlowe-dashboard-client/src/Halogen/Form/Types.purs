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
import Data.Lens (Lens, Prism, Prism', lens, prism, prism')
import Data.Show.Generic (genericShow)
import Test.QuickCheck (class Arbitrary, class Coarbitrary)
import Test.QuickCheck.Arbitrary (genericArbitrary, genericCoarbitrary)

data FieldState input output = FieldState input (Maybe output)

derive instance Generic (FieldState input output) _
derive instance Functor (FieldState input)
derive instance (Eq input, Eq output) => Eq (FieldState input output)
derive instance (Ord input, Ord output) => Ord (FieldState input output)

instance (Show input, Show output) => Show (FieldState input output) where
  show = genericShow

instance Bifunctor FieldState where
  bimap f g (FieldState input output) =
    FieldState (f input) (g <$> output)

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

_fieldInput
  :: forall input input' output
   . Lens (FieldState input output) (FieldState input' output) input input'
_fieldInput = lens get set
  where
  get (FieldState input _) = input
  set (FieldState _ output) input = FieldState input output

_fieldOutput
  :: forall input output output'
   . Lens
       (FieldState input output)
       (FieldState input output')
       (Maybe output)
       (Maybe output')
_fieldOutput = lens get set
  where
  get (FieldState _ output) = output
  set (FieldState input _) output = FieldState input output

data InitializeField input output
  = FromBlank
  | FromInput input
  | FromOutput output

fromMaybe :: forall input. Maybe ~> InitializeField input
fromMaybe Nothing = FromBlank
fromMaybe (Just output) = FromOutput output

derive instance Generic (InitializeField input output) _
derive instance Functor (InitializeField input)
derive instance (Eq input, Eq output) => Eq (InitializeField input output)
derive instance (Ord input, Ord output) => Ord (InitializeField input output)

instance (Show input, Show output) => Show (InitializeField input output) where
  show = genericShow

instance
  ( Enum input
  , Bounded input
  , Enum output
  , Bounded output
  ) =>
  Enum (InitializeField input output) where
  succ = genericSucc
  pred = genericPred

instance
  ( Bounded input
  , Bounded output
  ) =>
  Bounded (InitializeField input output) where
  top = genericTop
  bottom = genericBottom

instance
  ( BoundedEnum input
  , BoundedEnum output
  ) =>
  BoundedEnum (InitializeField input output) where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

instance Bifunctor InitializeField where
  bimap f g = case _ of
    FromBlank -> FromBlank
    FromInput input -> FromInput $ f input
    FromOutput output -> FromOutput $ g output

instance
  ( Arbitrary input
  , Arbitrary output
  ) =>
  Arbitrary (InitializeField input output) where
  arbitrary = genericArbitrary

instance
  ( Coarbitrary input
  , Coarbitrary output
  ) =>
  Coarbitrary (InitializeField input output) where
  coarbitrary = genericCoarbitrary

_FromBlank
  :: forall input output. Prism' (InitializeField input output) Unit
_FromBlank = prism' (const FromBlank) case _ of
  FromBlank -> Just unit
  _ -> Nothing

_FromInput
  :: forall input input' output
   . Prism
       (InitializeField input output)
       (InitializeField input' output)
       input
       input'
_FromInput = prism FromInput case _ of
  FromBlank -> Left FromBlank
  FromInput input -> Right input
  FromOutput output -> Left $ FromOutput output

_FromOutput
  :: forall input output output'
   . Prism
       (InitializeField input output)
       (InitializeField input output')
       output
       output'
_FromOutput = prism FromOutput case _ of
  FromBlank -> Left FromBlank
  FromInput input -> Left $ FromInput input
  FromOutput output -> Right output
