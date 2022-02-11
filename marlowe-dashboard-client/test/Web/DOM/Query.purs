module Test.Web.DOM.Query
  ( ByRoleOptions
  , Match(..)
  , Matcher
  , MatcherFunction
  , MatcherOptions
  , class IsMatcher
  , byRoleDefault
  , toMatcher
  , findBy
  , findAllBy
  , getBy
  , getAllBy
  , queryBy
  , queryAllBy
  , altText
  , displayValue
  , labelText
  , placeholderText
  , role
  , testId
  , text
  , title
  , altText'
  , displayValue'
  , labelText'
  , placeholderText'
  , role'
  , testId'
  , text'
  , title'
  ) where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Function.Uncurried (mkFn2)
import Data.Maybe (Maybe(..))
import Data.String.Regex (Regex)
import Data.Undefinable (Undefinable, toMaybe, toUndefinable)
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn3, runEffectFn3)
import Foreign (Foreign)
import Test.Web.Monad (class MonadTest, getContainer)
import Unsafe.Coerce (unsafeCoerce)
import Web.ARIA (ARIARole)
import Web.DOM (Element)

type MatcherFunction = String -> Maybe Element -> Boolean

-- | ```ts
-- | export type Matcher = MatcherFunction | RegExp | number | string
-- | ```
newtype Matcher = Matcher Foreign

class IsMatcher a where
  toMatcher :: a -> Matcher

instance IsMatcher Matcher where
  toMatcher = identity

instance IsMatcher String where
  toMatcher = unsafeCoerce

instance IsMatcher Regex where
  toMatcher = unsafeCoerce

-- | ```ts
-- | export type MatcherFunction = (
-- |   content: string,
-- |   element: Element | null,
-- | ) => boolean
-- | ```
instance IsMatcher MatcherFunction where
  toMatcher fn = unsafeCoerce $ mkFn2 \content -> fn content <<< toMaybe

type MatcherOptions =
  { exact :: Boolean
  , normalizer :: String -> String
  -- | suppress suggestions for a specific query
  , suggest :: Boolean
  }

type ByRoleOptions matcher =
  {
    -- | If true includes elements in the query set that are usually excluded from
    -- | the accessibility tree. `role="none"` or `role="presentation"` are included
    -- | in either case.
    hidden :: Undefinable Boolean
  -- | If true only includes elements in the query set that are marked as
  -- | selected in the accessibility tree, i.e., `aria-selected="true"`
  , selected :: Undefinable Boolean
  -- | If true only includes elements in the query set that are marked as
  -- | checked in the accessibility tree, i.e., `aria-checked="true"`
  , checked :: Undefinable Boolean
  -- | If true only includes elements in the query set that are marked as
  -- | pressed in the accessibility tree, i.e., `aria-pressed="true"`
  , pressed :: Undefinable Boolean
  -- | Filters elements by their `aria-current` state. `true` and `false`
  -- | match `aria-current="true"` and `aria-current="false"` (as well as a
  -- | missing `aria-current` attribute) respectively.
  , current :: Undefinable Boolean
  -- | If true only includes elements in the query set that are marked as
  -- | expanded in the accessibility tree, i.e., `aria-expanded="true"`
  , expanded :: Undefinable Boolean
  -- | Includes elements with the `"heading"` role matching the indicated level,
  -- | either by the semantic HTML heading elements `<h1>-<h6>` or matching
  -- | the `aria-level` attribute.
  , level :: Undefinable Int
  -- | Includes every role used in the `role` attribute
  -- | For example *ByRole('progressbar', {queryFallbacks: true})` will find <div role="meter progressbar">`.
  , queryFallbacks :: Undefinable Boolean
  -- | Only considers elements with the specified accessible name.
  , name :: Undefinable matcher
  }

byRoleDefault :: forall matcher. ByRoleOptions matcher
byRoleDefault =
  { hidden: toUndefinable Nothing
  , selected: toUndefinable Nothing
  , checked: toUndefinable Nothing
  , pressed: toUndefinable Nothing
  , current: toUndefinable Nothing
  , expanded: toUndefinable Nothing
  , level: toUndefinable Nothing
  , queryFallbacks: toUndefinable Nothing
  , name: toUndefinable Nothing
  }

data Match
  = AltText Matcher (Maybe MatcherOptions)
  | DisplayValue Matcher (Maybe MatcherOptions)
  | LabelText Matcher (Maybe MatcherOptions)
  | PlaceholderText Matcher (Maybe MatcherOptions)
  | Role ARIARole (Maybe (ByRoleOptions Matcher))
  | TestId Matcher (Maybe MatcherOptions)
  | Text Matcher (Maybe MatcherOptions)
  | Title Matcher (Maybe MatcherOptions)

altText :: forall matcher. IsMatcher matcher => matcher -> Match
altText matcher = AltText (toMatcher matcher) Nothing

displayValue :: forall matcher. IsMatcher matcher => matcher -> Match
displayValue matcher = DisplayValue (toMatcher matcher) Nothing

labelText :: forall matcher. IsMatcher matcher => matcher -> Match
labelText matcher = LabelText (toMatcher matcher) Nothing

placeholderText :: forall matcher. IsMatcher matcher => matcher -> Match
placeholderText matcher = PlaceholderText (toMatcher matcher) Nothing

role :: ARIARole -> Match
role r = Role r Nothing

testId :: forall matcher. IsMatcher matcher => matcher -> Match
testId matcher = TestId (toMatcher matcher) Nothing

text :: forall matcher. IsMatcher matcher => matcher -> Match
text matcher = Text (toMatcher matcher) Nothing

title :: forall matcher. IsMatcher matcher => matcher -> Match
title matcher = Title (toMatcher matcher) Nothing

altText'
  :: forall matcher. IsMatcher matcher => matcher -> MatcherOptions -> Match
altText' matcher = AltText (toMatcher matcher) <<< Just

displayValue'
  :: forall matcher. IsMatcher matcher => matcher -> MatcherOptions -> Match
displayValue' matcher = DisplayValue (toMatcher matcher) <<< Just

labelText'
  :: forall matcher. IsMatcher matcher => matcher -> MatcherOptions -> Match
labelText' matcher = LabelText (toMatcher matcher) <<< Just

placeholderText'
  :: forall matcher. IsMatcher matcher => matcher -> MatcherOptions -> Match
placeholderText' matcher = PlaceholderText (toMatcher matcher) <<< Just

role'
  :: forall matcher
   . IsMatcher matcher
  => ARIARole
  -> ByRoleOptions matcher
  -> Match
role' r options = Role r $ Just options
  { name = toUndefinable $ toMatcher <$> toMaybe options.name }

testId'
  :: forall matcher. IsMatcher matcher => matcher -> MatcherOptions -> Match
testId' matcher = TestId (toMatcher matcher) <<< Just

text' :: forall matcher. IsMatcher matcher => matcher -> MatcherOptions -> Match
text' matcher = Text (toMatcher matcher) <<< Just

title'
  :: forall matcher. IsMatcher matcher => matcher -> MatcherOptions -> Match
title' matcher = Title (toMatcher matcher) <<< Just

findBy :: forall m. MonadTest m => Match -> m Element
findBy = case _ of
  AltText matcher options ->
    liftAff <<< toAffE =<< callForeignMatcherFn findByAltText matcher options
  DisplayValue matcher options ->
    liftAff <<< toAffE
      =<< callForeignMatcherFn findByDisplayValue matcher options
  LabelText matcher options ->
    liftAff <<< toAffE =<< callForeignMatcherFn findByLabelText matcher options
  PlaceholderText matcher options ->
    liftAff <<< toAffE
      =<< callForeignMatcherFn findByPlaceholderText matcher options
  Role r options ->
    liftAff <<< toAffE =<< callForeignByRoleFn findByRole r options
  TestId matcher options ->
    liftAff <<< toAffE =<< callForeignMatcherFn findByTestId matcher options
  Text matcher options ->
    liftAff <<< toAffE =<< callForeignMatcherFn findByText matcher options
  Title matcher options ->
    liftAff <<< toAffE =<< callForeignMatcherFn findByTitle matcher options

findAllBy
  :: forall m
   . MonadTest m
  => Match
  -> m (NonEmptyArray Element)
findAllBy = case _ of
  AltText matcher options ->
    liftAff <<< toAffE =<< callForeignMatcherFn findAllByAltText matcher options
  DisplayValue matcher options ->
    liftAff <<< toAffE
      =<< callForeignMatcherFn findAllByDisplayValue matcher options
  LabelText matcher options ->
    liftAff <<< toAffE
      =<< callForeignMatcherFn findAllByLabelText matcher options
  PlaceholderText matcher options ->
    liftAff <<< toAffE
      =<< callForeignMatcherFn findAllByPlaceholderText matcher options
  Role r options ->
    liftAff <<< toAffE =<< callForeignByRoleFn findAllByRole r options
  TestId matcher options ->
    liftAff <<< toAffE =<< callForeignMatcherFn findAllByTestId matcher options
  Text matcher options ->
    liftAff <<< toAffE =<< callForeignMatcherFn findAllByText matcher options
  Title matcher options ->
    liftAff <<< toAffE =<< callForeignMatcherFn findAllByTitle matcher options

getBy
  :: forall m
   . MonadTest m
  => Match
  -> m Element
getBy = case _ of
  AltText matcher options ->
    liftEffect =<< callForeignMatcherFn getByAltText matcher options
  DisplayValue matcher options ->
    liftEffect =<< callForeignMatcherFn getByDisplayValue matcher options
  LabelText matcher options ->
    liftEffect =<< callForeignMatcherFn getByLabelText matcher options
  PlaceholderText matcher options ->
    liftEffect =<< callForeignMatcherFn getByPlaceholderText matcher options
  Role r options ->
    liftEffect =<< callForeignByRoleFn getByRole r options
  TestId matcher options ->
    liftEffect =<< callForeignMatcherFn getByTestId matcher options
  Text matcher options ->
    liftEffect =<< callForeignMatcherFn getByText matcher options
  Title matcher options ->
    liftEffect =<< callForeignMatcherFn getByTitle matcher options

getAllBy
  :: forall m
   . MonadTest m
  => Match
  -> m (NonEmptyArray Element)
getAllBy = case _ of
  AltText matcher options ->
    liftEffect =<< callForeignMatcherFn getAllByAltText matcher options
  DisplayValue matcher options ->
    liftEffect =<< callForeignMatcherFn getAllByDisplayValue matcher options
  LabelText matcher options ->
    liftEffect =<< callForeignMatcherFn getAllByLabelText matcher options
  PlaceholderText matcher options ->
    liftEffect =<< callForeignMatcherFn getAllByPlaceholderText matcher options
  Role r options ->
    liftEffect =<< callForeignByRoleFn getAllByRole r options
  TestId matcher options ->
    liftEffect =<< callForeignMatcherFn getAllByTestId matcher options
  Text matcher options ->
    liftEffect =<< callForeignMatcherFn getAllByText matcher options
  Title matcher options ->
    liftEffect =<< callForeignMatcherFn getAllByTitle matcher options

queryBy
  :: forall m
   . MonadTest m
  => Match
  -> m (Maybe Element)
queryBy = case _ of
  AltText matcher options ->
    map toMaybe $ liftEffect
      =<< callForeignMatcherFn queryByAltText matcher options
  DisplayValue matcher options ->
    map toMaybe $ liftEffect
      =<< callForeignMatcherFn queryByDisplayValue matcher options
  LabelText matcher options ->
    map toMaybe $ liftEffect
      =<< callForeignMatcherFn queryByLabelText matcher options
  PlaceholderText matcher options ->
    map toMaybe $ liftEffect
      =<< callForeignMatcherFn queryByPlaceholderText matcher options
  Role r options ->
    map toMaybe $ liftEffect =<< callForeignByRoleFn queryByRole r options
  TestId matcher options ->
    map toMaybe $ liftEffect
      =<< callForeignMatcherFn queryByTestId matcher options
  Text matcher options ->
    map toMaybe $ liftEffect
      =<< callForeignMatcherFn queryByText matcher options
  Title matcher options ->
    map toMaybe $ liftEffect
      =<< callForeignMatcherFn queryByTitle matcher options

queryAllBy
  :: forall m
   . MonadTest m
  => Match
  -> m (Array Element)
queryAllBy = case _ of
  AltText matcher options ->
    liftEffect =<< callForeignMatcherFn queryAllByAltText matcher options
  DisplayValue matcher options ->
    liftEffect =<< callForeignMatcherFn queryAllByDisplayValue matcher options
  LabelText matcher options ->
    liftEffect =<< callForeignMatcherFn queryAllByLabelText matcher options
  PlaceholderText matcher options ->
    liftEffect
      =<< callForeignMatcherFn queryAllByPlaceholderText matcher options
  Role r options ->
    liftEffect =<< callForeignByRoleFn queryAllByRole r options
  TestId matcher options ->
    liftEffect =<< callForeignMatcherFn queryAllByTestId matcher options
  Text matcher options ->
    liftEffect =<< callForeignMatcherFn queryAllByText matcher options
  Title matcher options ->
    liftEffect =<< callForeignMatcherFn queryAllByTitle matcher options

callForeignMatcherFn
  :: forall m a
   . MonadTest m
  => ForeignMatcherFn a
  -> Matcher
  -> Maybe MatcherOptions
  -> m (Effect a)
callForeignMatcherFn fn matcher options = do
  container <- getContainer
  pure $ runEffectFn3 fn container (unsafeCoerce matcher) $ toUndefinable
    options

callForeignByRoleFn
  :: forall m a
   . MonadTest m
  => ForeignByRoleFn a
  -> ARIARole
  -> Maybe (ByRoleOptions Matcher)
  -> m (Effect a)
callForeignByRoleFn fn r options = do
  container <- getContainer
  pure $ runEffectFn3 fn container (show r) $ toUndefinable options

type ForeignMatcherFn =
  EffectFn3 Element Matcher (Undefinable MatcherOptions)

type ForeignByRoleFn =
  EffectFn3 Element String (Undefinable (ByRoleOptions Matcher))

foreign import findAllByAltText
  :: ForeignMatcherFn (Promise (NonEmptyArray Element))

foreign import findAllByDisplayValue
  :: ForeignMatcherFn (Promise (NonEmptyArray Element))

foreign import findAllByLabelText
  :: ForeignMatcherFn (Promise (NonEmptyArray Element))

foreign import findAllByPlaceholderText
  :: ForeignMatcherFn (Promise (NonEmptyArray Element))

foreign import findAllByRole
  :: ForeignByRoleFn (Promise (NonEmptyArray Element))

foreign import findAllByTestId
  :: ForeignMatcherFn (Promise (NonEmptyArray Element))

foreign import findAllByText
  :: ForeignMatcherFn (Promise (NonEmptyArray Element))

foreign import findAllByTitle
  :: ForeignMatcherFn (Promise (NonEmptyArray Element))

foreign import findByAltText :: ForeignMatcherFn (Promise Element)
foreign import findByDisplayValue :: ForeignMatcherFn (Promise Element)
foreign import findByLabelText :: ForeignMatcherFn (Promise Element)
foreign import findByPlaceholderText :: ForeignMatcherFn (Promise Element)
foreign import findByRole :: ForeignByRoleFn (Promise Element)
foreign import findByTestId :: ForeignMatcherFn (Promise Element)
foreign import findByText :: ForeignMatcherFn (Promise Element)
foreign import findByTitle :: ForeignMatcherFn (Promise Element)
foreign import getAllByAltText :: ForeignMatcherFn (NonEmptyArray Element)
foreign import getAllByDisplayValue
  :: ForeignMatcherFn (NonEmptyArray Element)

foreign import getAllByLabelText :: ForeignMatcherFn (NonEmptyArray Element)
foreign import getAllByPlaceholderText
  :: ForeignMatcherFn (NonEmptyArray Element)

foreign import getAllByRole :: ForeignByRoleFn (NonEmptyArray Element)
foreign import getAllByTestId :: ForeignMatcherFn (NonEmptyArray Element)
foreign import getAllByText :: ForeignMatcherFn (NonEmptyArray Element)
foreign import getAllByTitle :: ForeignMatcherFn (NonEmptyArray Element)
foreign import getByAltText :: ForeignMatcherFn Element
foreign import getByDisplayValue :: ForeignMatcherFn Element
foreign import getByLabelText :: ForeignMatcherFn Element
foreign import getByPlaceholderText :: ForeignMatcherFn Element
foreign import getByRole :: ForeignByRoleFn Element
foreign import getByTestId :: ForeignMatcherFn Element
foreign import getByText :: ForeignMatcherFn Element
foreign import getByTitle :: ForeignMatcherFn Element
foreign import queryAllByAltText :: ForeignMatcherFn (Array (Element))
foreign import queryAllByDisplayValue :: ForeignMatcherFn (Array Element)
foreign import queryAllByLabelText :: ForeignMatcherFn (Array Element)
foreign import queryAllByPlaceholderText :: ForeignMatcherFn (Array Element)
foreign import queryAllByRole :: ForeignByRoleFn (Array Element)
foreign import queryAllByTestId :: ForeignMatcherFn (Array Element)
foreign import queryAllByText :: ForeignMatcherFn (Array Element)
foreign import queryAllByTitle :: ForeignMatcherFn (Array Element)
foreign import queryByAltText :: ForeignMatcherFn (Undefinable Element)
foreign import queryByDisplayValue :: ForeignMatcherFn (Undefinable Element)
foreign import queryByLabelText :: ForeignMatcherFn (Undefinable Element)
foreign import queryByPlaceholderText
  :: ForeignMatcherFn (Undefinable Element)

foreign import queryByRole :: ForeignByRoleFn (Undefinable Element)
foreign import queryByTestId :: ForeignMatcherFn (Undefinable Element)
foreign import queryByText :: ForeignMatcherFn (Undefinable Element)
foreign import queryByTitle :: ForeignMatcherFn (Undefinable Element)
