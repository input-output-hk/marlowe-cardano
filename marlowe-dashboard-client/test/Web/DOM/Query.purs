module Test.Web.DOM.Query
  ( ByRoleBuilder
  , Match
  , Matcher
  , MatcherFunction
  , MatchBuilder
  , class IsMatcher
  , altText
  , checked
  , current
  , displayValue
  , exact
  , expanded
  , findAllBy
  , findBy
  , getAllBy
  , getBy
  , hidden
  , labelText
  , level
  , name
  , nameRegex
  , nameRegexi
  , normalizer
  , placeholderText
  , pressed
  , queryAllBy
  , queryBy
  , queryFallbacks
  , role
  , selected
  , suggest
  , testId
  , text
  , title
  , toMatcher
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, try)
import Control.Monad.Error.Extra (toMonadThrow)
import Control.Monad.State (State, modify_, runState)
import Control.Promise (Promise, toAffE)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Function.Uncurried (mkFn2)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Data.Nullable as N
import Data.String.Regex (Regex)
import Data.String.Regex.Flags (RegexFlags, ignoreCase)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Tuple (Tuple(..))
import Data.Undefinable (Undefinable, toMaybe, toUndefinable)
import Effect (Effect)
import Effect.Aff (Error)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Uncurried (EffectFn3, runEffectFn3)
import Foreign (Foreign)
import Record.Builder (Builder)
import Record.Builder as Builder
import Test.Web.Monad (class MonadTest, getContainer)
import Type.Proxy (Proxy(..))
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

newtype MatchBuilder a =
  MatchBuilder (State (Builder MatcherOptions MatcherOptions) a)

derive newtype instance Functor MatchBuilder
derive newtype instance Apply MatchBuilder
derive newtype instance Applicative MatchBuilder
derive newtype instance Bind MatchBuilder
derive newtype instance Monad MatchBuilder

type MatcherOptions =
  { exact :: Undefinable Boolean
  , normalizer :: Undefinable (String -> String)
  -- | suppress suggestions for a specific query
  , suggest :: Undefinable Boolean
  }

defaultMatcherOptions :: MatcherOptions
defaultMatcherOptions =
  { exact: undefined
  , normalizer: undefined
  , suggest: undefined
  }

undefined :: forall a. Undefinable a
undefined = toUndefinable Nothing

defined :: forall a. a -> Undefinable a
defined = toUndefinable <<< Just

exact :: Boolean -> MatchBuilder Unit
exact = MatchBuilder
  <<< modify_
  <<< compose
  <<< Builder.modify (Proxy :: _ "exact")
  <<< const
  <<< defined

normalizer :: (String -> String) -> MatchBuilder Unit
normalizer = MatchBuilder
  <<< modify_
  <<< compose
  <<< Builder.modify (Proxy :: _ "normalizer")
  <<< const
  <<< defined

suggest :: Boolean -> MatchBuilder Unit
suggest = MatchBuilder
  <<< modify_
  <<< compose
  <<< Builder.modify (Proxy :: _ "suggest")
  <<< const
  <<< defined

buildMatch
  :: forall matcher
   . IsMatcher matcher
  => (Matcher -> MatcherOptions -> Match)
  -> MatchBuilder matcher
  -> Match
buildMatch mkMatch (MatchBuilder builder) =
  let
    Tuple matcher options = runState builder identity
  in
    mkMatch (toMatcher matcher) (Builder.build options defaultMatcherOptions)

newtype ByRoleBuilder a =
  ByRoleBuilder (State (Builder ByRoleOptions ByRoleOptions) a)

derive newtype instance Functor ByRoleBuilder
derive newtype instance Apply ByRoleBuilder
derive newtype instance Applicative ByRoleBuilder
derive newtype instance Bind ByRoleBuilder
derive newtype instance Monad ByRoleBuilder

type ByRoleOptions =
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
  , name :: Undefinable Matcher
  }

defaultByRoleOptions :: ByRoleOptions
defaultByRoleOptions =
  { hidden: undefined
  , selected: undefined
  , checked: undefined
  , pressed: undefined
  , current: undefined
  , expanded: undefined
  , level: undefined
  , queryFallbacks: undefined
  , name: undefined
  }

hidden :: Boolean -> ByRoleBuilder Unit
hidden = ByRoleBuilder
  <<< modify_
  <<< compose
  <<< Builder.modify (Proxy :: _ "hidden")
  <<< const
  <<< defined

selected :: Boolean -> ByRoleBuilder Unit
selected = ByRoleBuilder
  <<< modify_
  <<< compose
  <<< Builder.modify (Proxy :: _ "selected")
  <<< const
  <<< defined

checked :: Boolean -> ByRoleBuilder Unit
checked = ByRoleBuilder
  <<< modify_
  <<< compose
  <<< Builder.modify (Proxy :: _ "checked")
  <<< const
  <<< defined

pressed :: Boolean -> ByRoleBuilder Unit
pressed = ByRoleBuilder
  <<< modify_
  <<< compose
  <<< Builder.modify (Proxy :: _ "pressed")
  <<< const
  <<< defined

current :: Boolean -> ByRoleBuilder Unit
current = ByRoleBuilder
  <<< modify_
  <<< compose
  <<< Builder.modify (Proxy :: _ "current")
  <<< const
  <<< defined

expanded :: Boolean -> ByRoleBuilder Unit
expanded = ByRoleBuilder
  <<< modify_
  <<< compose
  <<< Builder.modify (Proxy :: _ "expanded")
  <<< const
  <<< defined

level :: Int -> ByRoleBuilder Unit
level = ByRoleBuilder
  <<< modify_
  <<< compose
  <<< Builder.modify (Proxy :: _ "level")
  <<< const
  <<< defined

queryFallbacks :: Boolean -> ByRoleBuilder Unit
queryFallbacks = ByRoleBuilder
  <<< modify_
  <<< compose
  <<< Builder.modify (Proxy :: _ "queryFallbacks")
  <<< const
  <<< defined

nameRegex :: String -> RegexFlags -> ByRoleBuilder Unit
nameRegex regex = name <<< unsafeRegex regex

nameRegexi :: String -> ByRoleBuilder Unit
nameRegexi regex = nameRegex regex ignoreCase

name :: forall matcher. IsMatcher matcher => matcher -> ByRoleBuilder Unit
name = ByRoleBuilder
  <<< modify_
  <<< compose
  <<< Builder.modify (Proxy :: _ "name")
  <<< const
  <<< defined
  <<< toMatcher

data Match
  = AltText Matcher MatcherOptions
  | DisplayValue Matcher MatcherOptions
  | LabelText Matcher MatcherOptions
  | PlaceholderText Matcher MatcherOptions
  | Role ARIARole ByRoleOptions
  | TestId Matcher MatcherOptions
  | Text Matcher MatcherOptions
  | Title Matcher MatcherOptions

altText :: forall matcher. IsMatcher matcher => MatchBuilder matcher -> Match
altText = buildMatch AltText

displayValue
  :: forall matcher. IsMatcher matcher => MatchBuilder matcher -> Match
displayValue = buildMatch DisplayValue

labelText :: forall matcher. IsMatcher matcher => MatchBuilder matcher -> Match
labelText = buildMatch LabelText

placeholderText
  :: forall matcher. IsMatcher matcher => MatchBuilder matcher -> Match
placeholderText = buildMatch PlaceholderText

role :: ByRoleBuilder ARIARole -> Match
role (ByRoleBuilder builder) =
  let
    Tuple ariaRole options = runState builder identity
  in
    Role ariaRole $ Builder.build options defaultByRoleOptions

testId :: forall matcher. IsMatcher matcher => MatchBuilder matcher -> Match
testId = buildMatch TestId

text :: forall matcher. IsMatcher matcher => MatchBuilder matcher -> Match
text = buildMatch Text

title :: forall matcher. IsMatcher matcher => MatchBuilder matcher -> Match
title = buildMatch Title

liftPromise
  :: forall a m. MonadAff m => MonadError Error m => Effect (Promise a) -> m a
liftPromise p = toMonadThrow =<< liftAff (try $ toAffE p)

liftForeignEffect
  :: forall a m. MonadEffect m => MonadError Error m => Effect a -> m a
liftForeignEffect e = toMonadThrow =<< liftEffect (try e)

findBy
  :: forall builder a m
   . MonadTest m
  => MonadError Error m
  => (builder a -> Match)
  -> builder a
  -> m Element
findBy build builder = case build builder of
  AltText matcher options ->
    liftPromise =<< callForeignMatcherFn findByAltText matcher options
  DisplayValue matcher options ->
    liftPromise =<< callForeignMatcherFn findByDisplayValue matcher options
  LabelText matcher options ->
    liftPromise =<< callForeignMatcherFn findByLabelText matcher options
  PlaceholderText matcher options ->
    liftPromise =<< callForeignMatcherFn findByPlaceholderText matcher options
  Role r options ->
    liftPromise =<< callForeignByRoleFn findByRole r options
  TestId matcher options ->
    liftPromise =<< callForeignMatcherFn findByTestId matcher options
  Text matcher options ->
    liftPromise =<< callForeignMatcherFn findByText matcher options
  Title matcher options ->
    liftPromise =<< callForeignMatcherFn findByTitle matcher options

findAllBy
  :: forall builder a m
   . MonadTest m
  => MonadError Error m
  => (builder a -> Match)
  -> builder a
  -> m (NonEmptyArray Element)
findAllBy build builder = case build builder of
  AltText matcher options ->
    liftPromise =<< callForeignMatcherFn findAllByAltText matcher options
  DisplayValue matcher options ->
    liftPromise =<< callForeignMatcherFn findAllByDisplayValue matcher options
  LabelText matcher options ->
    liftPromise =<< callForeignMatcherFn findAllByLabelText matcher options
  PlaceholderText matcher options ->
    liftPromise =<< callForeignMatcherFn findAllByPlaceholderText matcher
      options
  Role r options ->
    liftPromise =<< callForeignByRoleFn findAllByRole r options
  TestId matcher options ->
    liftPromise =<< callForeignMatcherFn findAllByTestId matcher options
  Text matcher options ->
    liftPromise =<< callForeignMatcherFn findAllByText matcher options
  Title matcher options ->
    liftPromise =<< callForeignMatcherFn findAllByTitle matcher options

getBy
  :: forall builder a m
   . MonadTest m
  => MonadError Error m
  => (builder a -> Match)
  -> builder a
  -> m Element
getBy build builder = case build builder of
  AltText matcher options ->
    liftForeignEffect =<< callForeignMatcherFn getByAltText matcher options
  DisplayValue matcher options ->
    liftForeignEffect =<< callForeignMatcherFn getByDisplayValue matcher options
  LabelText matcher options ->
    liftForeignEffect =<< callForeignMatcherFn getByLabelText matcher options
  PlaceholderText matcher options ->
    liftForeignEffect
      =<< callForeignMatcherFn getByPlaceholderText matcher options
  Role r options ->
    liftForeignEffect =<< callForeignByRoleFn getByRole r options
  TestId matcher options ->
    liftForeignEffect =<< callForeignMatcherFn getByTestId matcher options
  Text matcher options ->
    liftForeignEffect =<< callForeignMatcherFn getByText matcher options
  Title matcher options ->
    liftForeignEffect =<< callForeignMatcherFn getByTitle matcher options

getAllBy
  :: forall builder a m
   . MonadTest m
  => MonadError Error m
  => (builder a -> Match)
  -> builder a
  -> m (NonEmptyArray Element)
getAllBy build builder = case build builder of
  AltText matcher options ->
    liftForeignEffect =<< callForeignMatcherFn getAllByAltText matcher options
  DisplayValue matcher options ->
    liftForeignEffect
      =<< callForeignMatcherFn getAllByDisplayValue matcher options
  LabelText matcher options ->
    liftForeignEffect =<< callForeignMatcherFn getAllByLabelText matcher options
  PlaceholderText matcher options ->
    liftForeignEffect
      =<< callForeignMatcherFn getAllByPlaceholderText matcher options
  Role r options ->
    liftForeignEffect =<< callForeignByRoleFn getAllByRole r options
  TestId matcher options ->
    liftForeignEffect =<< callForeignMatcherFn getAllByTestId matcher options
  Text matcher options ->
    liftForeignEffect =<< callForeignMatcherFn getAllByText matcher options
  Title matcher options ->
    liftForeignEffect =<< callForeignMatcherFn getAllByTitle matcher options

queryBy
  :: forall builder a m
   . MonadTest m
  => MonadError Error m
  => (builder a -> Match)
  -> builder a
  -> m (Maybe Element)
queryBy build builder = case build builder of
  AltText matcher options ->
    map N.toMaybe $ liftForeignEffect
      =<< callForeignMatcherFn queryByAltText matcher options
  DisplayValue matcher options ->
    map N.toMaybe $ liftForeignEffect
      =<< callForeignMatcherFn queryByDisplayValue matcher options
  LabelText matcher options ->
    map N.toMaybe $ liftForeignEffect
      =<< callForeignMatcherFn queryByLabelText matcher options
  PlaceholderText matcher options ->
    map N.toMaybe $ liftForeignEffect
      =<< callForeignMatcherFn queryByPlaceholderText matcher options
  Role r options ->
    map N.toMaybe $ liftForeignEffect
      =<< callForeignByRoleFn queryByRole r options
  TestId matcher options ->
    map N.toMaybe $ liftForeignEffect
      =<< callForeignMatcherFn queryByTestId matcher options
  Text matcher options ->
    map N.toMaybe $ liftForeignEffect
      =<< callForeignMatcherFn queryByText matcher options
  Title matcher options ->
    map N.toMaybe $ liftForeignEffect
      =<< callForeignMatcherFn queryByTitle matcher options

queryAllBy
  :: forall builder a m
   . MonadTest m
  => MonadError Error m
  => (builder a -> Match)
  -> builder a
  -> m (Array Element)
queryAllBy build builder = case build builder of
  AltText matcher options ->
    liftForeignEffect =<< callForeignMatcherFn queryAllByAltText matcher options
  DisplayValue matcher options ->
    liftForeignEffect
      =<< callForeignMatcherFn queryAllByDisplayValue matcher options
  LabelText matcher options ->
    liftForeignEffect =<< callForeignMatcherFn queryAllByLabelText matcher
      options
  PlaceholderText matcher options ->
    liftForeignEffect
      =<< callForeignMatcherFn queryAllByPlaceholderText matcher options
  Role r options ->
    liftForeignEffect =<< callForeignByRoleFn queryAllByRole r options
  TestId matcher options ->
    liftForeignEffect =<< callForeignMatcherFn queryAllByTestId matcher options
  Text matcher options ->
    liftForeignEffect =<< callForeignMatcherFn queryAllByText matcher options
  Title matcher options ->
    liftForeignEffect =<< callForeignMatcherFn queryAllByTitle matcher options

callForeignMatcherFn
  :: forall m a
   . MonadTest m
  => MonadError Error m
  => ForeignMatcherFn a
  -> Matcher
  -> MatcherOptions
  -> m (Effect a)
callForeignMatcherFn fn matcher options = do
  container <- getContainer
  pure $ runEffectFn3 fn container (unsafeCoerce matcher) options

callForeignByRoleFn
  :: forall m a
   . MonadTest m
  => MonadError Error m
  => ForeignByRoleFn a
  -> ARIARole
  -> ByRoleOptions
  -> m (Effect a)
callForeignByRoleFn fn r options = do
  container <- getContainer
  pure $ runEffectFn3 fn container (show r) options

type ForeignMatcherFn =
  EffectFn3 Element Matcher MatcherOptions

type ForeignByRoleFn =
  EffectFn3 Element String ByRoleOptions

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
foreign import queryByAltText :: ForeignMatcherFn (Nullable Element)
foreign import queryByDisplayValue :: ForeignMatcherFn (Nullable Element)
foreign import queryByLabelText :: ForeignMatcherFn (Nullable Element)
foreign import queryByPlaceholderText
  :: ForeignMatcherFn (Nullable Element)

foreign import queryByRole :: ForeignByRoleFn (Nullable Element)
foreign import queryByTestId :: ForeignMatcherFn (Nullable Element)
foreign import queryByText :: ForeignMatcherFn (Nullable Element)
foreign import queryByTitle :: ForeignMatcherFn (Nullable Element)
