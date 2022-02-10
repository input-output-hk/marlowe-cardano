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
  -> m Element
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
  -> m (NonEmptyArray Element)
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
  -> m Element
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
  -> m (NonEmptyArray Element)
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
  -> m (Maybe Element)
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
  -> m (Array Element)
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
