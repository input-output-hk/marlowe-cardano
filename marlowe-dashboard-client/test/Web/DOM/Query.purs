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

import Control.Monad.State (State, modify_, runState)
import Control.Promise (Promise, toAffE)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Function.Uncurried (mkFn2)
import Data.Maybe (Maybe(..))
import Data.String.Regex (Regex)
import Data.Tuple (Tuple(..))
import Data.Undefinable (Undefinable, toMaybe, toUndefinable)
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
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

findBy
  :: forall builder a m
   . MonadTest m
  => (builder a -> Match)
  -> builder a
  -> m Element
findBy build builder = case build builder of
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
  :: forall builder a m
   . MonadTest m
  => (builder a -> Match)
  -> builder a
  -> m (NonEmptyArray Element)
findAllBy build builder = case build builder of
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
  :: forall builder a m
   . MonadTest m
  => (builder a -> Match)
  -> builder a
  -> m Element
getBy build builder = case build builder of
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
  :: forall builder a m
   . MonadTest m
  => (builder a -> Match)
  -> builder a
  -> m (NonEmptyArray Element)
getAllBy build builder = case build builder of
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
  :: forall builder a m
   . MonadTest m
  => (builder a -> Match)
  -> builder a
  -> m (Maybe Element)
queryBy build builder = case build builder of
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
  :: forall builder a m
   . MonadTest m
  => (builder a -> Match)
  -> builder a
  -> m (Array Element)
queryAllBy build builder = case build builder of
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
  -> MatcherOptions
  -> m (Effect a)
callForeignMatcherFn fn matcher options = do
  container <- getContainer
  pure $ runEffectFn3 fn container (unsafeCoerce matcher) options

callForeignByRoleFn
  :: forall m a
   . MonadTest m
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
foreign import queryByAltText :: ForeignMatcherFn (Undefinable Element)
foreign import queryByDisplayValue :: ForeignMatcherFn (Undefinable Element)
foreign import queryByLabelText :: ForeignMatcherFn (Undefinable Element)
foreign import queryByPlaceholderText
  :: ForeignMatcherFn (Undefinable Element)

foreign import queryByRole :: ForeignByRoleFn (Undefinable Element)
foreign import queryByTestId :: ForeignMatcherFn (Undefinable Element)
foreign import queryByText :: ForeignMatcherFn (Undefinable Element)
foreign import queryByTitle :: ForeignMatcherFn (Undefinable Element)
