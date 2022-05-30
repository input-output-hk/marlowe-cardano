module Marlowe.Template where

import Prologue

import Data.BigInt.Argonaut (BigInt)
import Data.DateTime (adjust)
import Data.DateTime.Instant (Instant)
import Data.DateTime.Instant as Instant
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
import Data.Time.Duration (class Duration)
import Data.Traversable (foldMap, scanl)
import Data.Tuple.Nested ((/\))
import Marlowe.Time (unixEpoch)
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

newtype TemplateContent = TemplateContent
  { timeContent :: Map String Instant
  , valueContent :: Map String BigInt
  }

_timeContent :: Lens' TemplateContent (Map String Instant)
_timeContent = _Newtype <<< prop (Proxy :: _ "timeContent")

_valueContent :: Lens' TemplateContent (Map String BigInt)
_valueContent = _Newtype <<< prop (Proxy :: _ "valueContent")

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

-- Adjust an Instant by a duration and if it overflows returns the same instant.
adjustInstantIfPossible :: forall d. Duration d => d -> Instant -> Instant
adjustInstantIfPossible duration instant = maybe
  instant
  Instant.fromDateTime
  ( adjust duration
      $ Instant.toDateTime instant
  )

-- Similar to initializeTemplateContent this function gets all the parameters placeholders
-- and initializes the value params with zero, but unlike that function it starts the time parameter using the
-- initialTime + duration iteratively.
-- TODO: Modify Marlowe Run to also use this version and remove the original `initializeTemplateContent`.
initializeTemplateContentWithIncreasingTime
  :: forall d
   . Duration d
  => Instant
  -> d
  -> OSet String
  -> Placeholders
  -> TemplateContent
initializeTemplateContentWithIncreasingTime initialTime d orderSet placeholders =
  let
    Placeholders { timeoutPlaceholderIds, valuePlaceholderIds } = placeholders

    valueContent = initializeWith (const zero) valuePlaceholderIds

    orderedTimeContent =
      orderContentUsingMetadata
        (initializeWith (const initialTime) timeoutPlaceholderIds)
        orderSet

    timeContent = Map.fromFoldable
      $ scanl
          ( \(_ /\ prevTime) (key /\ _) ->
              let
                adjustedTime = adjustInstantIfPossible d prevTime
              in
                key /\ adjustedTime
          )
          ("discarded" /\ initialTime)
      $ (OMap.toUnfoldable orderedTimeContent :: Array _)
  in
    TemplateContent { timeContent, valueContent }

updateTemplateContent
  :: forall d
   . Duration d
  => Instant
  -> d
  -> OSet String
  -> Placeholders
  -> TemplateContent
  -> TemplateContent
updateTemplateContent initialTime d orderSet placeholders prevTemplateContent =
  let
    Placeholders { timeoutPlaceholderIds, valuePlaceholderIds } = placeholders

    TemplateContent
      { timeContent: prevTimeContent, valueContent: prevValueContent } =
      prevTemplateContent

    valueContent = initializeWith
      (\x -> fromMaybe zero $ Map.lookup x prevValueContent)
      valuePlaceholderIds

    -- Create an ordered Map using the previous time value if available
    -- The order is defined by
    --  * Preserving the order of the metadata first
    --  * Values of the placeholder in a non-stable way later
    orderedTimeContent :: OMap String (Maybe Instant)
    orderedTimeContent =
      orderContentUsingMetadata
        ( initializeWith
            (\x -> Map.lookup x prevTimeContent)
            timeoutPlaceholderIds
        )
        orderSet

    timeContent = Map.fromFoldable
      -- For each element in the sorted time content array
      --  * If there isn't a time set, use the previous time plus the duration
      --  * If there is a time set leave it as it is (as long as its bigger than the previous time)
      $ scanl
          ( \(_ /\ timeOfPrevParam) (key /\ mThisTime) ->
              let
                prevTimePlusDuration = adjustInstantIfPossible d timeOfPrevParam
                thisTime = fromMaybe unixEpoch mThisTime
                time =
                  if thisTime > timeOfPrevParam then thisTime
                  else prevTimePlusDuration
              in
                key /\ time
          )
          ("discarded" /\ initialTime)
      $ (OMap.toUnfoldable orderedTimeContent :: Array _)

  in
    TemplateContent
      { timeContent
      , valueContent
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
