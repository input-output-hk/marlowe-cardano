module Marlowe.Template where

import Prelude

import Data.BigInt.Argonaut (BigInt)
import Data.DateTime.Instant (Instant)
import Data.Foldable (foldl)
import Data.Lens (Lens', iso, re)
import Data.Lens.Iso (mapping)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Map as Map
import Data.Map.Ordered.OMap (OMap)
import Data.Map.Ordered.OMap as OMap
import Data.Maybe (fromMaybe, maybe)
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Set as Set
import Data.Set.Ordered.OSet (OSet)
import Data.Traversable (foldMap)
import Marlowe.Time (unixEpoch)
import Plutus.V1.Ledger.Time as POSIXTime
import Type.Proxy (Proxy(..))

newtype Placeholders = Placeholders
  { timeoutPlaceholderIds :: Set String
  , valuePlaceholderIds :: Set String
  }

derive instance newTypePlaceholders :: Newtype Placeholders _

-- The eq and show instances are required for doing property based testing with quickcheck
-- and are not needed by the actual code.
derive newtype instance eqPlaceholders :: Eq Placeholders

derive newtype instance showPlaceholders :: Show Placeholders

derive newtype instance semigroupPlaceholders :: Semigroup Placeholders

derive newtype instance monoidPlaceholders :: Monoid Placeholders

data IntegerTemplateType
  = TimeContent
  | ValueContent

newtype TemplateContent = TemplateContent
  { timeContent :: Map String Instant
  , valueContent :: Map String BigInt
  }

_timeContent :: Lens' TemplateContent (Map String Instant)
_timeContent = _Newtype <<< prop (Proxy :: _ "timeContent")

_valueContent :: Lens' TemplateContent (Map String BigInt)
_valueContent = _Newtype <<< prop (Proxy :: _ "valueContent")

typeToLens :: IntegerTemplateType -> Lens' TemplateContent (Map String BigInt)
typeToLens TimeContent =
  _timeContent <<< mapping
    ( re _Newtype <<< iso POSIXTime.toBigInt
        (fromMaybe bottom <<< POSIXTime.fromBigInt)
    )
typeToLens ValueContent = _valueContent

derive instance newTypeTemplateContent :: Newtype TemplateContent _

instance semigroupTemplateContent :: Semigroup TemplateContent where
  append (TemplateContent a) (TemplateContent b) =
    TemplateContent
      { timeContent: Map.unionWith (const identity) a.timeContent b.timeContent
      , valueContent: Map.unionWith (const identity) a.valueContent
          b.valueContent
      }

instance monoidTemplateContent :: Monoid TemplateContent where
  mempty = TemplateContent { timeContent: Map.empty, valueContent: Map.empty }

initializeWith :: forall a b. Ord a => (a -> b) -> Set a -> Map a b
initializeWith f = foldl (\m x -> Map.insert x (f x) m) Map.empty

initializeTemplateContent :: Placeholders -> TemplateContent
initializeTemplateContent
  ( Placeholders
      { timeoutPlaceholderIds, valuePlaceholderIds }
  ) =
  TemplateContent
    { timeContent: initializeWith (const unixEpoch) timeoutPlaceholderIds
    , valueContent: initializeWith (const zero) valuePlaceholderIds
    }

updateTemplateContent :: Placeholders -> TemplateContent -> TemplateContent
updateTemplateContent
  ( Placeholders { timeoutPlaceholderIds, valuePlaceholderIds }
  )
  (TemplateContent { timeContent, valueContent }) =
  TemplateContent
    { timeContent: initializeWith
        (\x -> fromMaybe unixEpoch $ Map.lookup x timeContent)
        timeoutPlaceholderIds
    , valueContent: initializeWith
        (\x -> fromMaybe zero $ Map.lookup x valueContent)
        valuePlaceholderIds
    }

class Template a b where
  getPlaceholderIds :: a -> b

class Fillable a b where
  fillTemplate :: b -> a -> a

orderContentUsingMetadata
  :: forall a. Map String a -> OSet String -> OMap String a
orderContentUsingMetadata content orderedMetadataSet = orderedTaggedContent <>
  OMap.fromFoldableWithIndex untaggedContent
  where
  orderedTaggedContent = foldMap
    (\x -> maybe mempty (\y -> OMap.singleton x y) (Map.lookup x content))
    orderedMetadataSet

  metadataSet = Set.fromFoldable orderedMetadataSet -- For efficiency

  untaggedContent = Map.filterKeys (\x -> not $ Set.member x metadataSet)
    content
