module Marlowe.Template where

import Prelude
import Data.BigInt.Argonaut (BigInt)
import Data.Foldable (foldl)
import Data.Lens (Lens')
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
import Type.Proxy (Proxy(..))

newtype Placeholders
  = Placeholders
  { slotPlaceholderIds :: Set String
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
  = SlotContent
  | ValueContent

newtype TemplateContent
  = TemplateContent
  { slotContent :: Map String BigInt
  , valueContent :: Map String BigInt
  }

_slotContent :: Lens' TemplateContent (Map String BigInt)
_slotContent = _Newtype <<< prop (Proxy :: _ "slotContent")

_valueContent :: Lens' TemplateContent (Map String BigInt)
_valueContent = _Newtype <<< prop (Proxy :: _ "valueContent")

typeToLens :: IntegerTemplateType -> Lens' TemplateContent (Map String BigInt)
typeToLens SlotContent = _slotContent
typeToLens ValueContent = _valueContent

derive instance newTypeTemplateContent :: Newtype TemplateContent _

instance semigroupTemplateContent :: Semigroup TemplateContent where
  append (TemplateContent a) (TemplateContent b) =
    TemplateContent
      { slotContent: Map.unionWith (const identity) a.slotContent b.slotContent
      , valueContent: Map.unionWith (const identity) a.valueContent
          b.valueContent
      }

instance monoidTemplateContent :: Monoid TemplateContent where
  mempty = TemplateContent { slotContent: Map.empty, valueContent: Map.empty }

initializeWith :: forall a b. Ord a => (a -> b) -> Set a -> Map a b
initializeWith f = foldl (\m x -> Map.insert x (f x) m) Map.empty

initializeTemplateContent :: Placeholders -> TemplateContent
initializeTemplateContent
  ( Placeholders
      { slotPlaceholderIds, valuePlaceholderIds }
  ) =
  TemplateContent
    { slotContent: initializeWith (const one) slotPlaceholderIds
    , valueContent: initializeWith (const zero) valuePlaceholderIds
    }

updateTemplateContent :: Placeholders -> TemplateContent -> TemplateContent
updateTemplateContent
  ( Placeholders { slotPlaceholderIds, valuePlaceholderIds }
  )
  (TemplateContent { slotContent, valueContent }) =
  TemplateContent
    { slotContent: initializeWith
        (\x -> fromMaybe one $ Map.lookup x slotContent)
        slotPlaceholderIds
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
