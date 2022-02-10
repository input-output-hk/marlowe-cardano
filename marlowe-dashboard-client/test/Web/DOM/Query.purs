module Test.Web.DOM.Query
  ( ByRoleOptions
  , Match(..)
  , Matcher
  , MatcherFunction
  , MatcherOptions
  , class IsMatcher
  , toMatcher
  , find
  , findAll
  , get
  , getAll
  , query
  , queryAll
  ) where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Function.Uncurried (mkFn2)
import Data.Maybe (Maybe)
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
import Web.HTML (HTMLElement)
import Web.HTML.HTMLElement as HTMLElement

type MatcherFunction = String -> Maybe HTMLElement -> Boolean

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
  toMatcher fn = unsafeCoerce $ mkFn2 \content ->
    fn content <<< (HTMLElement.fromElement <=< toMaybe)

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
    hidden :: Boolean
  -- | If true only includes elements in the query set that are marked as
  -- | selected in the accessibility tree, i.e., `aria-selected="true"`
  , selected :: Boolean
  -- | If true only includes elements in the query set that are marked as
  -- | checked in the accessibility tree, i.e., `aria-checked="true"`
  , checked :: Boolean
  -- | If true only includes elements in the query set that are marked as
  -- | pressed in the accessibility tree, i.e., `aria-pressed="true"`
  , pressed :: Boolean
  -- | Filters elements by their `aria-current` state. `true` and `false`
  -- | match `aria-current="true"` and `aria-current="false"` (as well as a
  -- | missing `aria-current` attribute) respectively.
  , current :: Boolean
  -- | If true only includes elements in the query set that are marked as
  -- | expanded in the accessibility tree, i.e., `aria-expanded="true"`
  , expanded :: Boolean
  -- | Includes elements with the `"heading"` role matching the indicated level,
  -- | either by the semantic HTML heading elements `<h1>-<h6>` or matching
  -- | the `aria-level` attribute.
  , level :: Int
  -- | Includes every role used in the `role` attribute
  -- | For example *ByRole('progressbar', {queryFallbacks: true})` will find <div role="meter progressbar">`.
  , queryFallbacks :: Boolean
  -- | Only considers elements with the specified accessible name.
  , name :: Maybe matcher
  }

data Match matcher
  = ByAltText matcher (Maybe MatcherOptions)
  | ByDisplayValue matcher (Maybe MatcherOptions)
  | ByLabelText matcher (Maybe MatcherOptions)
  | ByPlaceholderText matcher (Maybe MatcherOptions)
  | ByRole ARIARole (Maybe (ByRoleOptions matcher))
  | ByTestId matcher (Maybe MatcherOptions)
  | ByText matcher (Maybe MatcherOptions)
  | ByTitle matcher (Maybe MatcherOptions)

find
  :: forall m matcher
   . IsMatcher matcher
  => MonadTest m
  => Match matcher
  -> m HTMLElement
find = case _ of
  ByAltText matcher options ->
    liftAff <<< toAffE =<< callForeignMatcherFn findByAltText matcher options
  ByDisplayValue matcher options ->
    liftAff <<< toAffE
      =<< callForeignMatcherFn findByDisplayValue matcher options
  ByLabelText matcher options ->
    liftAff <<< toAffE =<< callForeignMatcherFn findByLabelText matcher options
  ByPlaceholderText matcher options ->
    liftAff <<< toAffE
      =<< callForeignMatcherFn findByPlaceholderText matcher options
  ByRole role options ->
    liftAff <<< toAffE =<< callForeignByRoleFn findByRole role options
  ByTestId matcher options ->
    liftAff <<< toAffE
      =<< callForeignMatcherFn findByTestId matcher options
  ByText matcher options ->
    liftAff <<< toAffE =<< callForeignMatcherFn findByText matcher options
  ByTitle matcher options ->
    liftAff <<< toAffE =<< callForeignMatcherFn findByTitle matcher options

findAll
  :: forall m matcher
   . IsMatcher matcher
  => MonadTest m
  => Match matcher
  -> m (NonEmptyArray HTMLElement)
findAll = case _ of
  ByAltText matcher options ->
    liftAff <<< toAffE =<< callForeignMatcherFn findAllByAltText matcher options
  ByDisplayValue matcher options ->
    liftAff <<< toAffE
      =<< callForeignMatcherFn findAllByDisplayValue matcher options
  ByLabelText matcher options ->
    liftAff <<< toAffE
      =<< callForeignMatcherFn findAllByLabelText matcher options
  ByPlaceholderText matcher options ->
    liftAff <<< toAffE
      =<< callForeignMatcherFn findAllByPlaceholderText matcher options
  ByRole role options ->
    liftAff <<< toAffE =<< callForeignByRoleFn findAllByRole role options
  ByTestId matcher options ->
    liftAff <<< toAffE
      =<< callForeignMatcherFn findAllByTestId matcher options
  ByText matcher options ->
    liftAff <<< toAffE =<< callForeignMatcherFn findAllByText matcher options
  ByTitle matcher options ->
    liftAff <<< toAffE =<< callForeignMatcherFn findAllByTitle matcher options

get
  :: forall m matcher
   . IsMatcher matcher
  => MonadTest m
  => Match matcher
  -> m HTMLElement
get = case _ of
  ByAltText matcher options ->
    liftEffect =<< callForeignMatcherFn getByAltText matcher options
  ByDisplayValue matcher options ->
    liftEffect =<< callForeignMatcherFn getByDisplayValue matcher options
  ByLabelText matcher options ->
    liftEffect =<< callForeignMatcherFn getByLabelText matcher options
  ByPlaceholderText matcher options ->
    liftEffect =<< callForeignMatcherFn getByPlaceholderText matcher options
  ByRole role options ->
    liftEffect =<< callForeignByRoleFn getByRole role options
  ByTestId matcher options ->
    liftEffect =<< callForeignMatcherFn getByTestId matcher options
  ByText matcher options ->
    liftEffect =<< callForeignMatcherFn getByText matcher options
  ByTitle matcher options ->
    liftEffect =<< callForeignMatcherFn getByTitle matcher options

getAll
  :: forall m matcher
   . IsMatcher matcher
  => MonadTest m
  => Match matcher
  -> m (NonEmptyArray HTMLElement)
getAll = case _ of
  ByAltText matcher options ->
    liftEffect =<< callForeignMatcherFn getAllByAltText matcher options
  ByDisplayValue matcher options ->
    liftEffect =<< callForeignMatcherFn getAllByDisplayValue matcher options
  ByLabelText matcher options ->
    liftEffect =<< callForeignMatcherFn getAllByLabelText matcher options
  ByPlaceholderText matcher options ->
    liftEffect =<< callForeignMatcherFn getAllByPlaceholderText matcher options
  ByRole role options ->
    liftEffect =<< callForeignByRoleFn getAllByRole role options
  ByTestId matcher options ->
    liftEffect =<< callForeignMatcherFn getAllByTestId matcher options
  ByText matcher options ->
    liftEffect =<< callForeignMatcherFn getAllByText matcher options
  ByTitle matcher options ->
    liftEffect =<< callForeignMatcherFn getAllByTitle matcher options

query
  :: forall m matcher
   . IsMatcher matcher
  => MonadTest m
  => Match matcher
  -> m (Maybe HTMLElement)
query = case _ of
  ByAltText matcher options ->
    map toMaybe $ liftEffect
      =<< callForeignMatcherFn queryByAltText matcher options
  ByDisplayValue matcher options ->
    map toMaybe $ liftEffect
      =<< callForeignMatcherFn queryByDisplayValue matcher options
  ByLabelText matcher options ->
    map toMaybe $ liftEffect
      =<< callForeignMatcherFn queryByLabelText matcher options
  ByPlaceholderText matcher options ->
    map toMaybe $ liftEffect
      =<< callForeignMatcherFn queryByPlaceholderText matcher options
  ByRole role options ->
    map toMaybe $ liftEffect =<< callForeignByRoleFn queryByRole role options
  ByTestId matcher options ->
    map toMaybe $ liftEffect
      =<< callForeignMatcherFn queryByTestId matcher options
  ByText matcher options ->
    map toMaybe $ liftEffect
      =<< callForeignMatcherFn queryByText matcher options
  ByTitle matcher options ->
    map toMaybe $ liftEffect
      =<< callForeignMatcherFn queryByTitle matcher options

queryAll
  :: forall m matcher
   . IsMatcher matcher
  => MonadTest m
  => Match matcher
  -> m (Array HTMLElement)
queryAll = case _ of
  ByAltText matcher options ->
    liftEffect =<< callForeignMatcherFn queryAllByAltText matcher options
  ByDisplayValue matcher options ->
    liftEffect =<< callForeignMatcherFn queryAllByDisplayValue matcher options
  ByLabelText matcher options ->
    liftEffect =<< callForeignMatcherFn queryAllByLabelText matcher options
  ByPlaceholderText matcher options ->
    liftEffect
      =<< callForeignMatcherFn queryAllByPlaceholderText matcher options
  ByRole role options ->
    liftEffect =<< callForeignByRoleFn queryAllByRole role options
  ByTestId matcher options ->
    liftEffect =<< callForeignMatcherFn queryAllByTestId matcher options
  ByText matcher options ->
    liftEffect =<< callForeignMatcherFn queryAllByText matcher options
  ByTitle matcher options ->
    liftEffect =<< callForeignMatcherFn queryAllByTitle matcher options

callForeignMatcherFn
  :: forall m matcher a
   . IsMatcher matcher
  => MonadTest m
  => ForeignMatcherFn a
  -> matcher
  -> Maybe MatcherOptions
  -> m (Effect a)
callForeignMatcherFn fn matcher options = do
  container <- getContainer
  pure $ runEffectFn3 fn container (unsafeCoerce matcher) $ toUndefinable
    options

callForeignByRoleFn
  :: forall m matcher a
   . IsMatcher matcher
  => MonadTest m
  => ForeignByRoleFn a
  -> ARIARole
  -> Maybe (ByRoleOptions matcher)
  -> m (Effect a)
callForeignByRoleFn fn role options = do
  container <- getContainer
  pure
    $ runEffectFn3 fn container (show role)
    $ toUndefinable
    $ map (\o -> o { name = toMatcher <$> o.name })
    $ options

type ForeignMatcherFn =
  EffectFn3 HTMLElement Matcher (Undefinable MatcherOptions)

type ForeignByRoleFn =
  EffectFn3 HTMLElement String (Undefinable (ByRoleOptions Matcher))

foreign import findAllByAltText
  :: ForeignMatcherFn (Promise (NonEmptyArray HTMLElement))

foreign import findAllByDisplayValue
  :: ForeignMatcherFn (Promise (NonEmptyArray HTMLElement))

foreign import findAllByLabelText
  :: ForeignMatcherFn (Promise (NonEmptyArray HTMLElement))

foreign import findAllByPlaceholderText
  :: ForeignMatcherFn (Promise (NonEmptyArray HTMLElement))

foreign import findAllByRole
  :: ForeignByRoleFn (Promise (NonEmptyArray HTMLElement))

foreign import findAllByTestId
  :: ForeignMatcherFn (Promise (NonEmptyArray HTMLElement))

foreign import findAllByText
  :: ForeignMatcherFn (Promise (NonEmptyArray HTMLElement))

foreign import findAllByTitle
  :: ForeignMatcherFn (Promise (NonEmptyArray HTMLElement))

foreign import findByAltText :: ForeignMatcherFn (Promise HTMLElement)
foreign import findByDisplayValue :: ForeignMatcherFn (Promise HTMLElement)
foreign import findByLabelText :: ForeignMatcherFn (Promise HTMLElement)
foreign import findByPlaceholderText :: ForeignMatcherFn (Promise HTMLElement)
foreign import findByRole :: ForeignByRoleFn (Promise HTMLElement)
foreign import findByTestId :: ForeignMatcherFn (Promise HTMLElement)
foreign import findByText :: ForeignMatcherFn (Promise HTMLElement)
foreign import findByTitle :: ForeignMatcherFn (Promise HTMLElement)
foreign import getAllByAltText :: ForeignMatcherFn (NonEmptyArray HTMLElement)
foreign import getAllByDisplayValue
  :: ForeignMatcherFn (NonEmptyArray HTMLElement)

foreign import getAllByLabelText :: ForeignMatcherFn (NonEmptyArray HTMLElement)
foreign import getAllByPlaceholderText
  :: ForeignMatcherFn (NonEmptyArray HTMLElement)

foreign import getAllByRole :: ForeignByRoleFn (NonEmptyArray HTMLElement)
foreign import getAllByTestId :: ForeignMatcherFn (NonEmptyArray HTMLElement)
foreign import getAllByText :: ForeignMatcherFn (NonEmptyArray HTMLElement)
foreign import getAllByTitle :: ForeignMatcherFn (NonEmptyArray HTMLElement)
foreign import getByAltText :: ForeignMatcherFn HTMLElement
foreign import getByDisplayValue :: ForeignMatcherFn HTMLElement
foreign import getByLabelText :: ForeignMatcherFn HTMLElement
foreign import getByPlaceholderText :: ForeignMatcherFn HTMLElement
foreign import getByRole :: ForeignByRoleFn HTMLElement
foreign import getByTestId :: ForeignMatcherFn HTMLElement
foreign import getByText :: ForeignMatcherFn HTMLElement
foreign import getByTitle :: ForeignMatcherFn HTMLElement
foreign import queryAllByAltText :: ForeignMatcherFn (Array (HTMLElement))
foreign import queryAllByDisplayValue :: ForeignMatcherFn (Array HTMLElement)
foreign import queryAllByLabelText :: ForeignMatcherFn (Array HTMLElement)
foreign import queryAllByPlaceholderText :: ForeignMatcherFn (Array HTMLElement)
foreign import queryAllByRole :: ForeignByRoleFn (Array HTMLElement)
foreign import queryAllByTestId :: ForeignMatcherFn (Array HTMLElement)
foreign import queryAllByText :: ForeignMatcherFn (Array HTMLElement)
foreign import queryAllByTitle :: ForeignMatcherFn (Array HTMLElement)
foreign import queryByAltText :: ForeignMatcherFn (Undefinable HTMLElement)
foreign import queryByDisplayValue :: ForeignMatcherFn (Undefinable HTMLElement)
foreign import queryByLabelText :: ForeignMatcherFn (Undefinable HTMLElement)
foreign import queryByPlaceholderText
  :: ForeignMatcherFn (Undefinable HTMLElement)

foreign import queryByRole :: ForeignByRoleFn (Undefinable HTMLElement)
foreign import queryByTestId :: ForeignMatcherFn (Undefinable HTMLElement)
foreign import queryByText :: ForeignMatcherFn (Undefinable HTMLElement)
foreign import queryByTitle :: ForeignMatcherFn (Undefinable HTMLElement)
