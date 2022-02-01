-- TODO move this to its own library
module Forms.Uni where

--   ( Action(..)
--   , AsyncForm
--   , AsyncFormSpec
--   , AsyncInput(..)
--   , Form(..)
--   , FormHTML
--   , FormSpec
--   , decorate
--   , hoistForm
--   , html
--   , mkAsyncForm
--   , mkForm
--   , runForm
--   , runFormHalogenM
--   , sequenceForms
--   , split
--   , subform
--   , traverseForms
--   , traverseFormsWithIndex
--   , update
--   ) where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Plus (class Plus, empty)
import Data.Array (head) as Array
import Data.FormURLEncoded.Query (FieldId(..), Query)
import Data.FormURLEncoded.Query (FieldId, alter, lookup, singleton) as Query
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map.Ordered.OMap (singleton) as OMap
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Profunctor (class Profunctor, dimap)
import Data.Profunctor.Choice (class Choice, left, right)
import Data.Profunctor.Strong (class Strong, first, second)
import Forms.FormM (FormM)
import Forms.FormM (mapUpdateFormDataFn) as FormM
import Forms.Types (RenderFn, WidgetId(..))
import Polyform (Validator)
import Polyform.Batteries.UrlEncoded (Errors) as UrlEncoded
import Polyform.Batteries.UrlEncoded.Types.Errors (ErrorId(..))
import Polyform.Batteries.UrlEncoded.Types.Errors (lookup, singleton) as Errors
import Polyform.Validator (hoist) as Validator
import Polyform.Validator (liftFn, lmapValidator)
import Prim.Row (class Cons) as Row
import Record (get) as Record
import Safe.Coerce (coerce) as Safe
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)

-- | We keep `input` polymorphic here (it will be `Payload`)
-- | because we want to provide `Profunctor` and `Category`
-- | for `Form`.
-- | We should probably abstract error type away.
newtype Form m state widget err input output = Form
  { init :: state -> Query
  , render :: RenderFn state err widget
  , validator ::
      Validator (FormM state Query m) (UrlEncoded.Errors err) input output
  }

-- | Usual starting point of the validation.
type Form' m state widget err output = Form m state widget err Query output

derive instance Newtype (Form m st wd err i o) _
derive instance Applicative m => Functor (Form m st wd err i)
instance (Applicative m) => Apply (Form m st wd err i) where
  apply (Form a) (Form b) = Form
    { init: a.init <> b.init
    , render: a.render <> b.render
    , validator: a.validator <*> b.validator
    }

instance (Applicative m) => Applicative (Form m st wd err i) where
  pure a = Form
    { init: mempty
    , render: mempty
    , validator: pure a
    }

instance (Monad m) => Alt (Form m st wd err i) where
  alt (Form a) (Form b) = Form
    { init: a.init <> b.init
    , render: a.render <> b.render
    , validator: a.validator <|> b.validator
    }

instance (Monad m) => Plus (Form m st wd err i) where
  empty = Form
    { init: mempty
    , render: mempty
    , validator: empty
    }

instance Functor m => Profunctor (Form m st wd err) where
  dimap f g (Form { init, render, validator }) = Form
    { init
    , render
    , validator: dimap f g validator
    }

instance Monad m => Strong (Form m st wd err) where
  first (Form { init, render, validator }) = Form
    { init
    , render
    , validator: first validator
    }
  second (Form { init, render, validator }) = Form
    { init
    , render
    , validator: second validator
    }

instance Monad m => Choice (Form m st wd err) where
  left (Form { init, render, validator }) = Form
    { init
    , render
    , validator: left validator
    }
  right (Form { init, render, validator }) = Form
    { init
    , render
    , validator: right validator
    }

instance (Monad m) => Semigroupoid (Form m st wd err) where
  compose (Form a) (Form b) = Form
    { init: a.init <> b.init
    , render: a.render <> b.render
    , validator: a.validator <<< b.validator
    }

instance (Monad m) => Category (Form m st wd err) where
  identity = Form
    { init: mempty
    , render: mempty
    , validator: identity
    }

_init :: forall wd err i o m st. Lens' (Form m st wd err i o) (st -> Query)
_init = _Newtype <<< prop (Proxy :: Proxy "init")

_render
  :: forall wd err i o m st. Lens' (Form m st wd err i o) (RenderFn st err wd)
_render = _Newtype <<< prop (Proxy :: Proxy "render")

_validator
  :: forall wd err i o m st
   . Lens' (Form m st wd err i o)
       (Validator (FormM st Query m) (UrlEncoded.Errors err) i o)
_validator = _Newtype <<< prop (Proxy :: Proxy "validator")

type FieldErrors err = Array err

type FieldValue = String

type RenderFieldFn err wd = Maybe FieldValue -> FieldErrors err -> wd

type FieldM st = FormM st (Maybe FieldValue)

-- | Helper functions
flattenValue :: FieldId -> Query -> Maybe FieldValue
flattenValue id = Array.head <=< Query.lookup id

alterOverFieldValue
  :: FieldId
  -> (Maybe FieldValue -> Maybe FieldValue)
  -> (Query -> Query)
alterOverFieldValue id f = flip Query.alter id \m -> do
  r <- f do
    arr <- m
    Array.head arr
  pure [ r ]

-- | Stateless field which accesses only its own data.
type FieldSpec m st wd err o =
  { id :: Query.FieldId
  , validator ::
      Validator (FormM st (Maybe FieldValue) m) (FieldErrors err)
        (Maybe FieldValue)
        o
  , render :: RenderFieldFn err wd
  }

mkFieldForm
  :: forall wd err m o st
   . Monad m
  => FieldSpec m st wd err o
  -> Form' m st wd err o
mkFieldForm { id: fieldId, render, validator } = do
  let
    errId :: ErrorId
    errId = Safe.coerce fieldId

    wdId :: WidgetId
    wdId = Safe.coerce fieldId
  Form
    { init: const $ mempty
    , render: \_ payload errs -> do
        let
          widget = render (flattenValue fieldId payload)
            (Errors.lookup errId errs)
        OMap.singleton wdId widget
    , validator:
        validator <<< liftFn (flattenValue fieldId)
          # lmapValidator (Errors.singleton errId)
          # Validator.hoist
              (FormM.mapUpdateFormDataFn (alterOverFieldValue fieldId))
    }

type RenderStFieldFn o err wd = o -> Maybe FieldValue -> FieldErrors err -> wd

type StFieldSpec
  :: (Type -> Type) -> Type -> Type -> Type -> Type -> Symbol -> Type
type StFieldSpec m st wd errs o l =
  { init :: Maybe (o -> FieldValue)
  , id :: Proxy l
  , validator ::
      Validator (FormM st (Maybe FieldValue) m) (FieldErrors errs)
        (Maybe FieldValue)
        o
  , render :: RenderStFieldFn o errs wd
  }

mkStFieldForm
  :: forall err l m o st st_ wd
   . Monad m
  => IsSymbol l
  => Row.Cons l o st_ st
  => StFieldSpec m { | st } wd err o l
  -> Form' m { | st } wd err o
mkStFieldForm { id: l, init, render, validator } = do
  let
    id = reflectSymbol l
    fieldId = FieldId id
    errId = ErrorId id
    wdId = WidgetId id
  Form
    { init: case init of
        Nothing -> mempty
        Just f -> \st -> do
          let
            v = Record.get l st
          Query.singleton fieldId [ f v ]
    , render: \st payload errs -> do
        let
          widget = render (Record.get l st) (flattenValue fieldId payload)
            (Errors.lookup errId errs)
        OMap.singleton wdId widget
    , validator:
        validator <<< liftFn (flattenValue fieldId)
          # lmapValidator (Errors.singleton errId)
          # Validator.hoist
              (FormM.mapUpdateFormDataFn (alterOverFieldValue fieldId))
    }

