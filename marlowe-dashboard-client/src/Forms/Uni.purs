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
import Control.Bind (bindFlipped)
import Control.Monad.Maybe.Trans (MaybeT(..), mapMaybeT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (WriterT(..), mapWriterT)
import Control.Plus (class Plus, empty)
import Data.Array (head, singleton) as Array
import Data.Bifunctor (bimap, lmap)
import Data.Either (Either, hush)
import Data.Foldable (class Foldable)
import Data.FoldableWithIndex (class FoldableWithIndex)
import Data.FormURLEncoded.Query (Query(..))
import Data.Functor.Contravariant (cmap)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens, Lens', _1, _2, iso, lens, over, preview, set, view)
import Data.Lens as Lens
import Data.Lens.Index (class Index, ix)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map (SemigroupMap(..))
import Data.Map (insert, lookup) as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Profunctor (class Profunctor, dimap, lcmap)
import Data.Profunctor.Choice (class Choice, left, right)
import Data.Profunctor.Star (Star(..))
import Data.Profunctor.Strong (class Strong, first, second)
import Data.Show.Generic (genericShow)
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndex)
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup (V(..))
import Forms.FormM (FormM(..))
import Forms.Types (RenderFn)
import Network.RemoteData (RemoteData(..), fromEither, toMaybe)
import Polyform (Reporter(..), Validator)
import Polyform.Batteries.UrlEncoded (Errors) as UrlEncoded
import Polyform.Reporter (R, liftFnR, lmapReporter)
import Polyform.Reporter as Reporter
import Polyform.Validator (hoist) as Validator
import Polyform.Validator (liftFn, lmapValidator, runValidator)
import Type.Prelude (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

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
type Form' m state widget doc output = Form m state widget Query output

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
